# DataLocker Auto-Unlock - Fixes and Solutions

## Issues Found and Fixed

### 1. ✅ FIXED: expect not available after deployment

**Root Cause**: The home profile packages (base, development, email) were defined but never actually included in the home-environment configuration.

**Solution**: Updated `entelequia/system/machines/einstein.scm` to import and use the profile packages:

```scheme
;; Added imports
#:use-module (entelequia home profiles base)
#:use-module (entelequia home profiles development)
#:use-module (entelequia home profiles email)

;; Updated home-environment to include packages
(home-environment
 (packages (append base-home-packages
                   development-home-packages
                   email-home-packages))
 (services desktop-home-services))
```

**Impact**: Now `expect`, `libnotify` (notify-send), and all other base/development/email profile packages will be installed properly.

###  2. ⚠️ TODO: Udev rule not triggering automatically

**Current Status**: The udev rule file exists in the Guix store but is not being symlinked to `/etc/udev/rules.d/`.

**What We Know**:
- Rule file created: `/gnu/store/.../90-datalocker.rules`
- Not symlinked to: `/etc/udev/rules.d/90-datalocker.rules`
- Other udev rules (from packages) work fine

**Possible Solutions to Try**:

#### Option A: Manual udev rule installation (temporary workaround)

```bash
# Create symlink manually
sudo ln -s /gnu/store/3bgidv5rc2b1ynv1k2mvnkvvacnhkkng-90-datalocker.rules \
  /etc/udev/rules.d/90-datalocker.rules

# Reload udev
sudo udevadm control --reload-rules
sudo udevadm trigger

# Test by unplugging and replugging the device
```

#### Option B: Create a proper udev package

Instead of using `plain-file`, create a package that installs the udev rule:

```scheme
(define-public datalocker-udev-rules
  (package
    (name "datalocker-udev-rules")
    (version "1.0")
    (source #f)
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (assoc-ref %outputs "out"))
                (rules-dir (string-append out "/lib/udev/rules.d")))
           (mkdir-p rules-dir)
           (call-with-output-file (string-append rules-dir "/90-datalocker.rules")
             (lambda (port)
               (display "ACTION==\"add\", SUBSYSTEM==\"block\", ENV{ID_VENDOR_ID}==\"230a\", ENV{ID_MODEL_ID}==\"1550\", ENV{ID_CDROM}==\"1\", RUN+=\"/bin/sh -c '/run/setuid-programs/sudo -u rafael env DISPLAY=:0 XAUTHORITY=/home/rafael/.Xauthority DBUS_SESSION_BUS_ADDRESS=unix:path=/run/user/1000/bus /home/rafael/.local/bin/datalocker-unlock &'\"" port)))
           #t))))
    (synopsis "Udev rules for DataLocker Sentry ONE auto-unlock")
    (description "Automatically triggers unlock script when DataLocker USB is inserted")
    (home-page "")
    (license license:gpl3+)))

;; Then use it like:
(simple-service 'datalocker-udev-rules
                udev-service-type
                (list datalocker-udev-rules))
```

#### Option C: Use udev-rule procedure

Check if there's a `udev-rule` procedure in Guix:

```scheme
(use-modules (gnu services base))

(define datalocker-udev-rules-service
  (udev-rules-service 'datalocker-udev-rules
                      (plain-file "90-datalocker.rules" "...")))
```

### 3. ✅ FIXED: Lock script for safe USB removal

**Solution**: Created `~/.local/bin/datalocker-lock` script that:
- Unmounts the data partition
- Locks the encrypted volume using `Unlocker_64.exe -l`
- Shows notifications for status
- Logs to `/tmp/datalocker-lock.log`

**Usage**:
```bash
# Before unplugging the USB drive
datalocker-lock
```

## Deployment Steps (With Fixes)

### Step 1: Apply the Configuration

```bash
cd ~/.dotfiles

# System reconfiguration (udev rule + datalocker-lock script)
sudo guix time-machine -C channels.scm -- \
  system reconfigure entelequia/system/machines/einstein.scm

# Home reconfiguration (expect, libnotify, and all profile packages)
guix home reconfigure entelequia/system/machines/einstein.scm
```

**Note**: After this, you won't need `guix install expect` manually anymore - it will be in your profile.

### Step 2: Verify Packages Are Installed

```bash
# Check for expect
which expect

# Check for notify-send
which notify-send

# If missing, the home reconfigure may not have completed properly
```

### Step 3: Workaround for Udev Rule (Until Fixed)

Since the udev rule isn't auto-linking yet:

```bash
# Find the rule in the store
RULE_FILE=$(find /gnu/store -name "90-datalocker.rules" 2>/dev/null | head -1)

# Create symlink manually
sudo ln -s "$RULE_FILE" /etc/udev/rules.d/90-datalocker.rules

# Reload udev
sudo udevadm control --reload-rules
sudo udevadm trigger
```

### Step 4: Test Everything

```bash
# Test unlock manually first
datalocker-unlock

# Test lock script
datalocker-lock

# Test automatic trigger (after udev workaround)
# Unplug and replug the DataLocker
# Rofi password prompt should appear automatically
```

## Manual Usage (While Debugging)

### Unlock
```bash
# Option 1: With expect installed (from profile)
datalocker-unlock

# Option 2: Without expect (temporary shell)
guix shell expect -- datalocker-unlock
```

### Lock
```bash
datalocker-lock
```

### Check Logs
```bash
# Unlock log
tail -f /tmp/datalocker-unlock.log

# Lock log
tail -f /tmp/datalocker-lock.log
```

## Next Steps to Investigate

1. **Figure out why udev rule isn't symlinking**:
   - Check Guix manual for udev-service-type documentation
   - Look at how other custom udev rules are added in Guix
   - Consider creating a proper package for the udev rule

2. **Test home profile packages**:
   - After reconfiguration, verify all base/development/email packages are available
   - Remove manual `guix install expect` once confirmed working

3. **Add curie (laptop) support**:
   - Apply same home profile package fix to curie.scm
   - Test on laptop

## Files Modified

- `entelequia/system/machines/einstein.scm` - Added profile package imports and usage
- `entelequia/system/machines/datalocker-udev-rules.scm` - Udev rule definition
- `entelequia/home/profiles/base.scm` - Added expect
- `entelequia/home/services/desktop.scm` - Added libnotify
- `dotfiles/.local/bin/datalocker-unlock` - Unlock script
- `dotfiles/.local/bin/datalocker-lock` - Lock script (NEW)

## Known Limitations

1. **Udev rule requires manual symlinking** (temporary workaround needed)
2. **Password briefly visible in process list** during expect execution (low risk)
3. **Hardcoded DISPLAY=:0** may not work on multi-user systems
4. **Single device support** (script assumes one DataLocker at a time)

## Future Enhancements

- [ ] Fix udev rule auto-installation
- [ ] Add keyboard shortcut to trigger datalocker-lock (e.g., Super+Shift+L)
- [ ] Create rofi menu with "Unlock" and "Lock" options
- [ ] Support multiple DataLocker devices
- [ ] Store password in GPG-encrypted file for unattended unlock
- [ ] Add systemd/shepherd service for auto-lock on sleep/suspend
