(define-module (entelequia system lib security-hardening)
  #:use-module (gnu)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services networking)
  #:use-module (gnu services ssh)
  #:use-module (gnu services security-token)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages security-token)
  #:use-module (gnu packages ssh)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:export (kernel-hardening-service
            fail2ban-service
            nftables-firewall-service
            hardened-ssh-service
            auditd-service
            pam-hardening-service
            security-hardening-services))

;;; Security hardening module
;;;
;;; This module provides comprehensive security hardening services
;;; for the entelequia system, including kernel hardening, firewall,
;;; intrusion prevention, and audit logging.

;;; Kernel hardening via sysctl parameters
;;; Configure kernel security parameters via sysctl.

(define sysctl-hardening-config
  (plain-file "99-security-hardening.conf"
              "# Security hardening sysctl parameters

# Network hardening
# Enable SYN cookies for SYN flood protection
net.ipv4.tcp_syncookies = 1
# Disable IP forwarding (unless needed for routing)
net.ipv4.ip_forward = 0
net.ipv6.conf.all.forwarding = 0
# Disable ICMP redirect acceptance
net.ipv4.conf.all.accept_redirects = 0
net.ipv4.conf.default.accept_redirects = 0
net.ipv6.conf.all.accept_redirects = 0
net.ipv6.conf.default.accept_redirects = 0
# Disable secure ICMP redirect acceptance
net.ipv4.conf.all.secure_redirects = 0
net.ipv4.conf.default.secure_redirects = 0
# Disable ICMP redirect sending
net.ipv4.conf.all.send_redirects = 0
net.ipv4.conf.default.send_redirects = 0
# Enable source address verification (anti-spoofing)
net.ipv4.conf.all.rp_filter = 1
net.ipv4.conf.default.rp_filter = 1
# Ignore ICMP echo requests (ping) - set to 0 to allow ping
net.ipv4.icmp_echo_ignore_all = 0
# Ignore broadcast ICMP requests
net.ipv4.icmp_echo_ignore_broadcasts = 1
# Ignore bogus ICMP error responses
net.ipv4.icmp_ignore_bogus_error_responses = 1
# Log martian packets (packets with impossible addresses)
net.ipv4.conf.all.log_martians = 1
net.ipv4.conf.default.log_martians = 1
# Disable IPv6 router advertisements
net.ipv6.conf.all.accept_ra = 0
net.ipv6.conf.default.accept_ra = 0
# TCP hardening
net.ipv4.tcp_rfc1337 = 1
# Increase TCP connection tracking
net.netfilter.nf_conntrack_max = 2000000
net.netfilter.nf_conntrack_tcp_timeout_established = 600

# Kernel hardening
# Restrict kernel pointer exposure via /proc
kernel.kptr_restrict = 2
# Restrict dmesg to root only
kernel.dmesg_restrict = 1
# Restrict perf events to root
kernel.perf_event_paranoid = 3
# Disable kexec (prevents loading unsigned kernels)
kernel.kexec_load_disabled = 1
# Enable ASLR (Address Space Layout Randomization)
kernel.randomize_va_space = 2
# Restrict ptrace to parent processes only
kernel.yama.ptrace_scope = 1
# Disable unprivileged BPF
# NOTE: I suspect, it produces -- guix system: warning: failed to load operating system for kexec: In procedure kexec-load-file: Operation not permitted
kernel.unprivileged_bpf_disabled = 1
# Restrict unprivileged user namespaces (prevents container escapes)
# NOTE: Disabled for rootless containers (podman/distrobox) - they require user namespaces
# Also not available on all kernels
# kernel.unprivileged_userns_clone = 0

# Filesystem hardening
# Enable protection against hardlink/symlink attacks
fs.protected_hardlinks = 1
fs.protected_symlinks = 1
# Protect against FIFO attacks in world-writable directories
fs.protected_fifos = 2
fs.protected_regular = 2
# Restrict access to /proc/<pid>/maps and similar
fs.suid_dumpable = 0

# Virtual memory hardening
# Restrict mmap to prevent null pointer dereference exploits
vm.mmap_min_addr = 65536
"))

(define kernel-hardening-service
  (list
   ;; Install sysctl configuration
   (simple-service 'sysctl-hardening-config
                   etc-service-type
                   (list `("sysctl.d/99-security-hardening.conf" ,sysctl-hardening-config)))

   ;; Shepherd service to apply sysctl settings on boot
   (simple-service 'sysctl-hardening-apply
                   shepherd-root-service-type
                   (list
                    (shepherd-service
                     (documentation "Apply security hardening sysctl parameters")
                     (provision '(sysctl-hardening))
                     (requirement '(file-systems))
                     (start #~(lambda ()
                                (invoke #$(file-append procps "/sbin/sysctl")
                                        "-p" "/etc/sysctl.d/99-security-hardening.conf")
                                #t))
                     (stop #~(const #f))
                     (one-shot? #t))))))

;;; Fail2Ban service for SSH protection
;;; Provides brute-force protection for SSH by banning IPs after failed attempts.

(define fail2ban-config
  (plain-file "jail.local"
              "[DEFAULT]
# Ban hosts for 1 hour
bantime = 3600
# Find time window: 10 minutes
findtime = 600
# Max retries before ban
maxretry = 5

[sshd]
enabled = true
port = 2222
filter = sshd
logpath = /var/log/secure
maxretry = 3
bantime = 7200
"))

(define fail2ban-service
  (list
   ;; Install fail2ban configuration
   (simple-service 'fail2ban-config
                   etc-service-type
                   (list `("fail2ban/jail.local" ,fail2ban-config)))

   ;; Fail2Ban shepherd service
   (simple-service 'fail2ban-daemon
                   shepherd-root-service-type
                   (list
                    (shepherd-service
                     (documentation "Fail2Ban intrusion prevention service")
                     (provision '(fail2ban))
                     (requirement '(networking syslogd))
                     (start #~(make-forkexec-constructor
                               (list #$(file-append fail2ban "/bin/fail2ban-client")
                                     "start")
                               #:log-file "/var/log/fail2ban.log"))
                     (stop #~(make-forkexec-constructor
                              (list #$(file-append fail2ban "/bin/fail2ban-client")
                                    "stop")))
                     (actions
                      (list
                       (shepherd-action
                        (name 'status)
                        (documentation "Show Fail2Ban status")
                        (procedure
                         #~(lambda (pid)
                             (invoke #$(file-append fail2ban "/bin/fail2ban-client")
                                     "status"))))))
                     (respawn? #t))))))

;;; nftables firewall service
;;; Provides stateful firewall with secure defaults using nftables.

(define* (make-nftables-ruleset #:key (extra-tcp-ports '()) (extra-udp-ports '()))
  "Create nftables ruleset with optional extra TCP/UDP ports.
   EXTRA-TCP-PORTS: List of TCP port numbers to allow (e.g., 24800 for Synergy)
   EXTRA-UDP-PORTS: List of UDP port numbers to allow"
  (let ((tcp-rules (if (null? extra-tcp-ports)
                       ""
                       (string-append
                        "\n    # Extra TCP ports\n"
                        (string-join
                         (map (lambda (port)
                                (format #f "    tcp dport ~a ct state new accept" port))
                              extra-tcp-ports)
                         "\n"))))
        (udp-rules (if (null? extra-udp-ports)
                       ""
                       (string-append
                        "\n    # Extra UDP ports\n"
                        (string-join
                         (map (lambda (port)
                                (format #f "    udp dport ~a accept" port))
                              extra-udp-ports)
                         "\n")))))
    (plain-file "nftables.conf"
              (string-append
               "#!/usr/sbin/nft -f

# Clear existing rules
flush ruleset

# Main firewall table
table inet filter {
  # Input chain - filter incoming traffic
  chain input {
    type filter hook input priority 0; policy drop;

    # Allow established/related connections
    ct state established,related accept

    # Allow loopback traffic
    iface lo accept

    # Drop invalid packets
    ct state invalid drop

    # Allow ICMP (ping) with rate limiting
    ip protocol icmp icmp type { echo-request, destination-unreachable, time-exceeded } limit rate 1/second accept
    ip6 nexthdr icmpv6 icmpv6 type { echo-request, destination-unreachable, time-exceeded } limit rate 1/second accept

    # Allow SSH on port 2222 with rate limiting
    tcp dport 2222 ct state new limit rate 4/minute accept

    # Allow Tailscale VPN (UDP port 41641)
    udp dport 41641 accept

    # Allow mDNS for local network discovery
    udp dport 5353 accept

    # Allow DHCP client
    udp sport 68 udp dport 67 accept"
               tcp-rules
               udp-rules
               "

    # Log and drop everything else
    log prefix \"[nftables] Input DROP: \" drop
  }

  # Forward chain - filter forwarded traffic
  chain forward {
    type filter hook forward priority 0; policy drop;

    # Allow established/related connections
    ct state established,related accept

    # Log and drop everything else
    log prefix \"[nftables] Forward DROP: \" drop
  }

  # Output chain - filter outgoing traffic
  chain output {
    type filter hook output priority 0; policy accept;

    # Allow all outgoing traffic (can be restricted if needed)
  }
}
"))))

(define* (nftables-firewall-service #:key (extra-tcp-ports '()) (extra-udp-ports '()))
  "Create nftables firewall service with optional extra ports.
   EXTRA-TCP-PORTS: List of TCP port numbers to allow
   EXTRA-UDP-PORTS: List of UDP port numbers to allow"
  (let ((ruleset (make-nftables-ruleset #:extra-tcp-ports extra-tcp-ports
                                        #:extra-udp-ports extra-udp-ports)))
    (list
     ;; Install nftables ruleset
     (simple-service 'nftables-config
                     etc-service-type
                     (list `("nftables.conf" ,ruleset)))

   ;; nftables shepherd service
   (simple-service 'nftables-daemon
                   shepherd-root-service-type
                   (list
                    (shepherd-service
                     (documentation "nftables firewall service")
                     (provision '(nftables firewall))
                     (requirement '(networking))
                     (start #~(make-system-constructor
                               "nft -f /etc/nftables.conf"))
                     (stop #~(make-system-destructor
                              "nft flush ruleset"))
                     (actions
                      (list
                       (shepherd-action
                        (name 'reload)
                        (documentation "Reload nftables rules")
                        (procedure
                         #~(lambda (pid)
                             (invoke #$(file-append nftables "/sbin/nft")
                                     "-f" "/etc/nftables.conf"))))
                       (shepherd-action
                        (name 'status)
                        (documentation "Show nftables rules")
                        (procedure
                         #~(lambda (pid)
                             (invoke #$(file-append nftables "/sbin/nft")
                                     "list" "ruleset"))))))
                     (respawn? #f))))))) ; Close let and function

;;; Hardened SSH service configuration

(define (hardened-ssh-service port-number)
  "Create a hardened SSH service configuration.
   PORT-NUMBER: SSH port to listen on (default 2222)."
  (service openssh-service-type
           (openssh-configuration
            (openssh openssh-sans-x)
            (port-number port-number)
            ;; Security hardening
            (permit-root-login #f)           ; Disable root login
            (password-authentication? #f)    ; Key-only authentication
            (challenge-response-authentication? #f)
            (public-key-authentication? #t)
            ;; Strong crypto settings
            (extra-content
             "
# Protocol and crypto hardening
Protocol 2
Ciphers chacha20-poly1305@openssh.com,aes256-gcm@openssh.com,aes128-gcm@openssh.com,aes256-ctr,aes192-ctr,aes128-ctr
MACs hmac-sha2-512-etm@openssh.com,hmac-sha2-256-etm@openssh.com,hmac-sha2-512,hmac-sha2-256
KexAlgorithms curve25519-sha256,curve25519-sha256@libssh.org,diffie-hellman-group-exchange-sha256

# Connection limits
MaxAuthTries 3
MaxSessions 5
LoginGraceTime 30

# Logging
LogLevel VERBOSE
SyslogFacility AUTH

# Other security settings
X11Forwarding no
AllowAgentForwarding yes
AllowTcpForwarding yes
PermitUserEnvironment no
PermitTunnel no
PrintLastLog yes
TCPKeepAlive yes
Compression no
ClientAliveInterval 300
ClientAliveCountMax 2
UseDNS no
"))))

;;; Audit logging service (auditd)
;;; Provides comprehensive security event logging and monitoring.

(define auditd-rules
  (plain-file "audit.rules"
              "# Audit rules for security monitoring

# Remove existing rules
-D

# Buffer size
-b 8192

# Failure mode: 1 = print to syslog
-f 1

# Monitor authentication events
-w /etc/passwd -p wa -k identity
-w /etc/group -p wa -k identity
-w /etc/shadow -p wa -k identity
-w /etc/gshadow -p wa -k identity
-w /etc/sudoers -p wa -k sudoers
-w /etc/sudoers.d/ -p wa -k sudoers

# Monitor SSH configuration
-w /etc/ssh/sshd_config -p wa -k sshd_config

# Monitor kernel modules
-w /sbin/insmod -p x -k modules
-w /sbin/rmmod -p x -k modules
-w /sbin/modprobe -p x -k modules

# Monitor system calls
-a exit,always -F arch=b64 -S adjtimex -S settimeofday -k time_change
-a exit,always -F arch=b64 -S mount -S umount2 -k mounts
-a exit,always -F arch=b64 -S unlink -S unlinkat -S rename -S renameat -k delete

# Monitor privileged commands
-a exit,always -F path=/usr/bin/sudo -F perm=x -F auid>=1000 -F auid!=4294967295 -k privileged
"))

(define auditd-service
  (list
   ;; Install audit rules
   (simple-service 'auditd-rules
                   etc-service-type
                   (list `("audit/audit.rules" ,auditd-rules)))

   ;; auditd shepherd service
   (simple-service 'auditd-daemon
                   shepherd-root-service-type
                   (list
                    (shepherd-service
                     (documentation "Linux audit daemon")
                     (provision '(auditd))
                     (requirement '())
                     (start #~(make-forkexec-constructor
                               (list #$(file-append audit "/sbin/auditd")
                                     "-f")
                               #:log-file "/var/log/audit.log"))
                     (stop #~(make-kill-destructor))
                     (respawn? #t))))))

;;; PAM hardening service
;;; Configure PAM for account security, process limits, and core dumps.

(define pam-hardening-service
  (simple-service 'pam-hardening
                  pam-limits-service-type
                  (list
                   ;; Account lockout after failed attempts
                   ;; Note: This is basic - full pam_faillock needs more config

                   ;; Realtime limits for audio (already in base.scm)
                   (pam-limits-entry "@realtime" 'both 'rtprio 99)
                   (pam-limits-entry "@realtime" 'both 'nice -19)
                   (pam-limits-entry "@realtime" 'both 'memlock 'unlimited)

                   ;; Process limits for regular users
                   (pam-limits-entry "*" 'soft 'nproc 1024)
                   (pam-limits-entry "*" 'hard 'nproc 4096)

                   ;; Core dump limits (disable for security)
                   (pam-limits-entry "*" 'hard 'core 0))))

;;; Combined security hardening services

(define* (security-hardening-services #:key (ssh-port 2222) (enable-fail2ban? #t)
                                      (enable-firewall? #t) (enable-audit? #t)
                                      (firewall-extra-tcp-ports '())
                                      (firewall-extra-udp-ports '()))
  "Return a list of all security hardening services.

   Options:
   - ssh-port: SSH port number (default 2222)
   - enable-fail2ban?: Enable Fail2Ban (default #t)
   - enable-firewall?: Enable nftables firewall (default #t)
   - enable-audit?: Enable auditd (default #t)
   - firewall-extra-tcp-ports: Extra TCP ports to allow through firewall (e.g., '(24800) for Synergy)
   - firewall-extra-udp-ports: Extra UDP ports to allow through firewall"
  (append
   ;; Always enable kernel hardening
   kernel-hardening-service

   ;; Optional services
   (if enable-fail2ban? fail2ban-service '())
   (if enable-firewall?
       (nftables-firewall-service #:extra-tcp-ports firewall-extra-tcp-ports
                                  #:extra-udp-ports firewall-extra-udp-ports)
       '())
   (if enable-audit? auditd-service '())

   ;; Always enable PAM hardening (single service, wrap in list)
   (list pam-hardening-service)))
