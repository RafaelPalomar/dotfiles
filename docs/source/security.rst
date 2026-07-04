==============
Security Notes
==============

The system implements comprehensive security hardening through the ``security-hardening`` module.

Access Control
==============

SSH
---

Hardened configuration on port 2222:

- Key-only authentication (password auth disabled)
- Root login disabled
- Strong ciphers and MACs (ChaCha20-Poly1305, AES-GCM, SHA2)
- Connection rate limiting (4 connections/minute)
- Maximum 3 authentication attempts

User Access
-----------

- ``rafael`` in wheel group for sudo

GPG Agent
---------

- Rofi for graphical password prompts

OAuth2
------

- Institutional email (NTNU/UiO accounts)

Network Security
================

Firewall
--------

nftables with stateful packet filtering:

- Default DROP policy for incoming traffic
- Allows: SSH (2222), Tailscale (41641), mDNS (5353), established connections
- Einstein-specific: Synergy port (24800) for keyboard/mouse sharing
- Rate-limited ICMP (1 packet/second)
- Connection tracking with reasonable limits
- Logging of dropped packets
- Machine-specific ports can be added via ``firewall-extra-tcp-ports`` and ``firewall-extra-udp-ports`` parameters

Fail2Ban
--------

SSH brute-force protection:

- Ban after 3 failed attempts
- 2-hour ban duration, 10-minute detection window
- Automatic IP blocking

Tailscale VPN
-------------

- System-wide secure mesh networking

Kernel Hardening
================

- **Network**: SYN cookies, IP forwarding disabled, source validation, martian packet logging
- **Process isolation**: Restricted ptrace, disabled unprivileged BPF, restricted user namespaces
- **Memory protection**: ASLR enabled, mmap restrictions, kexec disabled
- **Information hiding**: dmesg restricted, kptr restricted, perf events restricted
- **Filesystem**: Protected hardlinks/symlinks/FIFOs, disabled SUID core dumps

Filesystem Security
===================

/tmp
----

Mounted with ``noexec``, ``nosuid``, ``nodev`` flags

Audit & Monitoring
==================

Auditd
------

Security event logging:

- Authentication events (/etc/passwd, /etc/shadow, /etc/sudoers)
- SSH configuration changes
- Kernel module operations
- System time changes, mounts, file deletions
- Privileged command execution
- Logs: ``/var/log/audit.log``

PAM Limits
----------

Process limits, core dumps disabled:

- Regular users: 1024 soft / 4096 hard process limit
- Core dumps completely disabled for security

Security Commands
=================

.. code-block:: bash

    # Check firewall rules
    sudo nft list ruleset

    # Reload firewall
    herd reload nftables

    # Check Fail2Ban status
    herd status fail2ban

    # View banned IPs
    sudo fail2ban-client status sshd

    # View audit logs
    sudo ausearch -ts today

    # View kernel hardening parameters
    sysctl kernel.yama.ptrace_scope
    sysctl kernel.unprivileged_bpf_disabled

Security Configuration Files
=============================

- Kernel hardening: ``/etc/sysctl.d/99-security-hardening.conf``
- Firewall rules: ``/etc/nftables.conf``
- Fail2Ban: ``/etc/fail2ban/jail.local``
- Audit rules: ``/etc/audit/audit.rules``
- SSH config: Generated from ``hardened-ssh-service``

Important Notes
===============

- Firewall allows all outgoing connections (can be restricted if needed)
- Some security features may need adjustment for specific use cases (e.g., containers need user namespaces)
- Audit logs can grow large; rotate regularly
