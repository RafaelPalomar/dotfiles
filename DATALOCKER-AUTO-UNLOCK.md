# DataLocker Sentry ONE Auto-Unlock Setup

This document describes the automatic unlocking system for DataLocker Sentry ONE encrypted USB drives.

## Overview

When you plug in your DataLocker Sentry ONE USB drive, the system will automatically:
1. Detect the device via udev rules
2. Mount the UNLOCKER partition (if not already mounted)
3. Prompt for your password using Rofi (same style as GPG pinentry)
4. Unlock the encrypted volume using the DataLocker software
5. Auto-mount the decrypted data partition
6. Show desktop notifications for success/failure

## Components

### 1. Unlock Script (`~/.local/bin/datalocker-unlock`)

Location: `dotfiles/.local/bin/datalocker-unlock`

This bash script:
- Waits for the UNLOCKER partition to be mounted
- Prompts for password via Rofi
- Uses `expect` to feed the password to the DataLocker unlocker software
- Monitors the unlock process and shows notifications
- Logs activity to `/tmp/datalocker-unlock.log`

### 2. Udev Rule (`entelequia/system/machines/datalocker-udev-rules.scm`)

Guix service that installs a udev rule to:
- Detect DataLocker Sentry ONE by vendor ID (230a) and product ID (1550)
- Trigger the unlock script when the device is inserted
- Run as user `rafael` with proper X11 environment variables

### 3. Package Dependencies

Added `expect` to `entelequia/home/profiles/base.scm` for password automation.

## Deployment

### Apply the Configuration

```bash
# From ~/.dotfiles directory

# 1. Dry-run (verify what will change)
sudo guix time-machine -C channels.scm -- \
  system reconfigure entelequia/system/machines/einstein.scm --dry-run

# 2. Apply the configuration
sudo guix time-machine -C channels.scm -- \
  system reconfigure entelequia/system/machines/einstein.scm

# 3. Reboot (or restart udev)
sudo herd restart udev

# 4. Update home environment (for expect package)
guix home reconfigure entelequia/home/home-config.scm
```

### Quick Udev Restart (without reboot)

```bash
# Reload udev rules without reboot
sudo udevadm control --reload-rules
sudo udevadm trigger
```

## Testing

### Test the Unlock Script Manually

With the DataLocker plugged in and UNLOCKER partition mounted:

```bash
# Run the unlock script manually
~/.local/bin/datalocker-unlock

# Check the log file
tail -f /tmp/datalocker-unlock.log
```

### Test Automatic Trigger

1. Unplug the DataLocker USB drive
2. Wait a few seconds
3. Plug it back in
4. You should see a Rofi password prompt appear automatically
5. Enter your password
6. Check notifications for success/failure

### Debug Udev Triggering

```bash
# Monitor udev events for DataLocker
udevadm monitor | grep -i "sentry\|230a"

# Check if udev rule is installed
cat /etc/udev/rules.d/90-datalocker.rules

# Test udev rule matching
udevadm info --query=all --name=/dev/sr1 | grep -E "ID_VENDOR|ID_MODEL"

# Manually trigger udev rule for device
sudo udevadm trigger --action=add --name-match=/dev/sr1
```

## Usage

### Normal Operation

1. **Plug in the USB drive**
2. **Wait for Rofi prompt** (appears automatically within 2-5 seconds)
3. **Enter your password**
4. **Wait for notification** confirming unlock
5. **Access your files** (automatically mounted, notification shows mount point)

### Manual Unlock (if auto-unlock fails)

```bash
# Run unlock script manually
~/.local/bin/datalocker-unlock
```

### Locking the Drive

The encrypted volume remains unlocked until you:

**Option 1: Physically remove the drive**
```bash
# Just unplug it
```

**Option 2: Lock before unplugging**
```bash
# Unmount the data partition first
umount /run/media/rafael/YOUR_DATA_PARTITION

# Lock the device
/run/media/rafael/UNLOCKER/linux/Unlocker_64.exe -l
```

### Check Mounted Volumes

```bash
# See all DataLocker volumes
lsblk | grep -i "DL\|Sentry"

# See mount points
mount | grep -i sentry
```

## Troubleshooting

### Rofi Doesn't Appear

**Check if udev rule fired:**
```bash
# Check log file
tail -20 /tmp/datalocker-unlock.log

# Check system journal for udev
journalctl -f | grep -i datalocker
```

**Test X11 environment:**
```bash
# Verify DISPLAY is set
echo $DISPLAY  # Should be :0 or :1

# Test Rofi
rofi -dmenu -p "Test" <<< "Hello"
```

**Manually trigger udev:**
```bash
sudo udevadm trigger --action=add --name-match=/dev/sr1
```

### Password Fails / Unlock Doesn't Work

**Check expect is installed:**
```bash
which expect
expect --version
```

**Run unlock script with debug:**
```bash
# Edit the script to add set -x at the top
bash -x ~/.local/bin/datalocker-unlock 2>&1 | tee /tmp/unlock-debug.log
```

**Check unlocker binary permissions:**
```bash
ls -l /run/media/rafael/UNLOCKER/linux/Unlocker_64.exe
# Should be executable: -r-xr-xr-x
```

**Test unlocker manually (without expect):**
```bash
/run/media/rafael/UNLOCKER/linux/Unlocker_64.exe
# Enter password when prompted
# Check if it unlocks successfully
```

### Device Not Detected

**Check if device is recognized:**
```bash
# USB device should show up
lsusb | grep -i "DL\|Sentry\|230a"

# Block device should show up
lsblk | grep -i "DL\|Sentry"
```

**Check udev attributes:**
```bash
# Find the device path
ls -l /dev/disk/by-id/ | grep -i DL

# Check udev info
udevadm info --query=all --name=/dev/sr1
```

**Verify udev rule is installed:**
```bash
cat /etc/udev/rules.d/90-datalocker.rules
```

### Notifications Don't Appear

**Check if notification daemon is running:**
```bash
ps aux | grep -i dunst
# or
ps aux | grep notification
```

**Test notifications:**
```bash
notify-send "Test" "This is a test notification"
```

### Permission Denied Errors

**Check if script is executable:**
```bash
ls -l ~/.local/bin/datalocker-unlock
chmod +x ~/.local/bin/datalocker-unlock
```

**Check sudo works without password for your user:**
```bash
# User rafael should be in wheel group
groups rafael | grep wheel
```

## Log Files

- **Unlock script log**: `/tmp/datalocker-unlock.log`
- **System journal**: `journalctl -f | grep -i datalocker`
- **Udev events**: `udevadm monitor`

## Security Considerations

### Password Handling

- Password is entered via Rofi (secure input, masked)
- Password is passed to expect via command-line (visible in process list briefly)
- Password is NOT logged to files
- Expect script clears password from memory after use

### Improvements for Production

If you want more security:

1. **Use a password file with restricted permissions:**
   ```bash
   # Store encrypted password
   gpg --encrypt -o ~/.datalocker-pass.gpg <<< "your-password"
   chmod 600 ~/.datalocker-pass.gpg

   # Modify script to decrypt and use it
   PASSWORD=$(gpg --decrypt ~/.datalocker-pass.gpg 2>/dev/null)
   ```

2. **Use a dedicated secrets manager:**
   - Store password in KeePassXC or similar
   - Retrieve via CLI when needed

3. **Audit password exposure:**
   ```bash
   # Password briefly visible in ps output during expect execution
   # Consider using a named pipe or file descriptor instead
   ```

## Device Information

- **Vendor**: DL (230a)
- **Model**: Sentry ONE-M (1550)
- **Serial**: 80000BAC6CA9B621700066A0 (your specific device)
- **UNLOCKER Partition**: 300MB ISO9660 filesystem (appears as /dev/sr1)
- **Data Partition**: Encrypted, appears as /dev/sda after unlock

## Files Modified

- `entelequia/system/machines/einstein.scm` - Added datalocker udev service
- `entelequia/system/machines/datalocker-udev-rules.scm` - New udev rule service
- `entelequia/home/profiles/base.scm` - Added expect package
- `dotfiles/.local/bin/datalocker-unlock` - New unlock script

## Future Enhancements

Possible improvements:

1. **Auto-lock on unmount**: Use udev rule to lock when data partition is unmounted
2. **Multi-device support**: Handle multiple DataLocker devices
3. **Read-only mode**: Add option to unlock as read-only (-ro flag)
4. **Better password management**: Integrate with GPG or secrets manager
5. **GUI application**: Replace Rofi with a custom GUI dialog
6. **Timeout handling**: Auto-lock after period of inactivity
7. **Notification improvements**: Show progress during unlock

## References

- DataLocker Linux README: `/run/media/rafael/UNLOCKER/linux/Linux_Readme.txt`
- Guix udev documentation: `info guix "Base Services"`
- Rofi documentation: `man rofi`
- Expect documentation: `man expect`
