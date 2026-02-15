# Email Configuration Setup Guide

## Overview

Your email system uses:
- **isync (mbsync)**: Syncs email from IMAP servers
- **msmtp**: Sends email via SMTP
- **mu/mu4e**: Indexes and reads email in Emacs
- **OAuth2**: Authenticates with Microsoft/Google using `mutt_oauth2.py`
- **GPG**: Encrypts and signs emails

## Current Status

✓ GPG keys configured (Key ID: 507FA72F)
✓ Email packages installed (isync, msmtp, mu, mutt_oauth2.py)
✓ .mbsyncrc configured for NTNU and OUS accounts
✓ .msmtprc created for sending email
✓ mu4e configured in emacs.org
✗ OAuth2 tokens not yet registered
✗ Configuration mismatch between mbsync and mu4e
✗ mu4e load-path needs fixing for Guix

## Configuration Issues to Fix

### Issue 1: OUS-Research Maildir Path Mismatch

**Problem**: The maildir path in `.mbsyncrc` doesn't match the path in `emacs.org`

- `.mbsyncrc` (line 38): `/home/rafael/.local/share/mail/rafael.palomar@ous-research.no/`
- `emacs.org` (line 1456-1459): `/rafaelpa@ous-research.no/`

**Solution**: Choose one of these options:

**Option A (Recommended)**: Update emacs.org to match mbsyncrc:
```elisp
;; In emacs.org, around line 1456-1459, change:
(mu4e-drafts-folder     . "/rafael.palomar@ous-research.no/Drafts")
(mu4e-sent-folder       . "/rafael.palomar@ous-research.no/Sent")
(mu4e-trash-folder      . "/rafael.palomar@ous-research.no/Trash")
(mu4e-refile-folder     . "/rafael.palomar@ous-research.no/Archive")
```

**Option B**: Update .mbsyncrc to use rafaelpa@ous-research.no:
```
# In .mbsyncrc, change lines 36-39:
MaildirStore rafael.palomar@ous-research.no-local
Subfolders Verbatim
Path /home/rafael/.local/share/mail/rafaelpa@ous-research.no/
Inbox /home/rafael/.local/share/mail/rafaelpa@ous-research.no/INBOX
```

### Issue 2: mu4e Load Path for Guix

**Problem**: emacs.org line 1358 has a hardcoded path for non-Guix systems:
```elisp
:load-path "/usr/share/emacs/site-lisp/mu4e"
```

**Solution**: In Guix, mu4e is provided by the `mu` package and should be automatically available. Either:

1. Remove the `:load-path` line entirely (Guix will find it)
2. Or use: `:load-path (expand-file-name "share/emacs/site-lisp" (getenv "GUIX_ENVIRONMENT"))`

### Issue 3: Simplified mbsync Command

**Problem**: emacs.org line 1365 has an overly complex mbsync command:
```elisp
mu4e-get-mail-command  "guix shell -L ~/.dotfiles cyrus-sasl-xoauth2 -- mbsync -a"
```

**Solution**: Since packages are in your home profile, simplify to:
```elisp
mu4e-get-mail-command  "mbsync -a"
```

## Bootstrap Process

### Step 1: Fix Configuration Issues

Edit `emacs.org` and fix the three issues above.

### Step 2: Run Bootstrap Script

```bash
cd ~/.dotfiles
./scripts/bootstrap-email-oauth2.sh
```

This script will:
- Create `~/.password-store/email/` directory
- Create mail directories for each account
- Guide you through OAuth2 app registration
- Create initial token files

### Step 3: Register OAuth2 Applications

#### For Microsoft Accounts (NTNU, OUS)

1. Go to https://portal.azure.com/#blade/Microsoft_AAD_RegisteredApps/ApplicationsListBlade
2. Click "New registration"
3. Name: "Email Client - Personal"
4. Supported account types: "Accounts in any organizational directory and personal Microsoft accounts"
5. Redirect URI: "Web" → `http://localhost:8080/`
6. Click "Register"
7. Note the "Application (client) ID"
8. Go to "Certificates & secrets" → "New client secret"
9. Note the secret value
10. Go to "API permissions" → "Add a permission" → "Microsoft Graph" → "Delegated permissions"
11. Add: `IMAP.AccessAsUser.All`, `SMTP.Send`, `offline_access`, `User.Read`
12. Click "Grant admin consent"

**Note**: You need to create ONE app registration that can be used for BOTH NTNU and OUS accounts.

#### For Google (Gmail) - If Enabled

1. Go to https://console.cloud.google.com/
2. Create new project or select existing
3. Enable Gmail API
4. Create credentials → OAuth 2.0 Client IDs → Desktop app
5. Download credentials JSON

### Step 4: Authorize Each Account

For each account, run:

```bash
# NTNU
mutt_oauth2.py ~/.password-store/email/ntnu.no --authorize

# OUS/UiO
mutt_oauth2.py ~/.password-store/email/uio.no --authorize

# Gmail (if enabled)
mutt_oauth2.py ~/.password-store/email/rafaelpalomaravalos_gmail.com --authorize
```

The script will:
1. Prompt for client ID and secret
2. Open browser for authentication
3. Save refresh token for automatic renewal

### Step 5: Apply Home Configuration

```bash
cd ~/.dotfiles
guix home reconfigure entelequia/home/home-config.scm
```

This will symlink `.mbsyncrc` and `.msmtprc` to your home directory.

### Step 6: Initialize mu Database

```bash
mu init --maildir=~/.local/share/mail \
    --my-address=rafael.palomar@ntnu.no \
    --my-address=rafael.palomar@ous-research.no
```

### Step 7: Initial Email Sync

```bash
# Sync all accounts (may take a while for first sync)
mbsync -a

# Index email
mu index
```

### Step 8: Test in Emacs

```bash
# Restart Emacs daemon to pick up new configuration
herd restart emacs

# Or manually restart
emacsclient -e '(kill-emacs)' && emacs --daemon

# Open mu4e
emacsclient -c -e '(mu4e)'
```

## Keybindings in Emacs

Your mu4e is accessible via `SPC m` (Space + m):

- `SPC m m` - Open mu4e
- `SPC m q` - Quit mu4e
- `SPC m c c` - Compose new email (plain text)
- `SPC m c C` - Compose with Org-mode
- `SPC m r r` - Reply (plain text)
- `SPC m r R` - Reply with Org-mode

## Testing OAuth2 Tokens

To test if tokens are working:

```bash
# Test NTNU
mutt_oauth2.py ~/.password-store/email/ntnu.no --test

# Test OUS/UiO
mutt_oauth2.py ~/.password-store/email/uio.no --test

# Test sending email with msmtp
echo "Test email" | msmtp --debug --account=ntnu rafael.palomar@ntnu.no
```

## Troubleshooting

### Check msmtp logs

```bash
tail -f ~/.cache/msmtp.log
```

### Manually sync one account

```bash
mbsync rafael.palomar@ntnu.no
```

### Re-index email

```bash
mu index --rebuild
```

### Check mu database

```bash
mu find from:someone@example.com
mu cfind rafael  # Find contacts
```

### Verify GPG integration

```bash
# List GPG keys
gpg --list-keys

# Test encryption
echo "test" | gpg --encrypt --recipient rafael@palomar.no | gpg --decrypt
```

## Signing and Encrypting Email

With your GPG keys properly set up, mu4e will automatically offer to sign/encrypt emails.

To configure default signing:

```elisp
;; Add to mu4e configuration in emacs.org
(setq mml-secure-openpgp-sign-with-sender t
      mml-secure-openpgp-encrypt-to-self t)
```

In a composition buffer:
- `C-c C-m C-s` - Sign message
- `C-c C-m C-e` - Encrypt message
- `C-c C-m C-c` - Sign and encrypt

## Regular Maintenance

### Update email periodically

mu4e will auto-update every 5 minutes (configured in emacs.org), or manually:
- In mu4e: `U` to update mail

### Renew OAuth2 tokens

Tokens refresh automatically. If they expire, re-run:
```bash
mutt_oauth2.py ~/.password-store/email/<account> --authorize
```

### Clean old email

```bash
# Delete emails older than 30 days in Trash
mu find maildir:/rafael.palomar@ntnu.no/Trash date:..30d --format=links | xargs rm
mu index
```

## Security Notes

1. **OAuth2 tokens**: Stored in `~/.password-store/email/` (encrypted with GPG)
2. **GPG passphrase**: Cached for 8 hours (configured in `entelequia/systems/desktop.scm`)
3. **msmtp logs**: May contain sensitive info, periodically clean `~/.cache/msmtp.log`
4. **Backup**: Remember to backup `~/.password-store/` and `~/.local/share/mail/`

## References

- [Microsoft OAuth2 for IMAP/SMTP](https://github.com/UvA-FNWI/M365-IMAP)
- [mutt_oauth2.py documentation](https://github.com/neomutt/neomutt/blob/main/contrib/oauth2/mutt_oauth2.py.README)
- [mu4e manual](https://www.djcbsoftware.nl/code/mu/mu4e/)
- [mbsync manual](https://isync.sourceforge.io/mbsync.html)
