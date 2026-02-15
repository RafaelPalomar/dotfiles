#!/usr/bin/env bash
# Bootstrap script for email OAuth2 configuration
# This script helps set up OAuth2 tokens for institutional email accounts

set -e

PASS_DIR="$HOME/.password-store/email"
MAIL_DIR="$HOME/.local/share/mail"

echo "=== Email OAuth2 Bootstrap Script ==="
echo ""
echo "This script will help you set up OAuth2 authentication for your email accounts."
echo "You'll need to register applications with Microsoft/Google and obtain OAuth2 credentials."
echo ""

# Create password-store directory if it doesn't exist
if [ ! -d "$PASS_DIR" ]; then
    echo "Creating password-store email directory: $PASS_DIR"
    mkdir -p "$PASS_DIR"
fi

# Create mail directories if they don't exist
if [ ! -d "$MAIL_DIR" ]; then
    echo "Creating mail directory: $MAIL_DIR"
    mkdir -p "$MAIL_DIR"
fi

# Function to set up an OAuth2 token file
setup_oauth2_account() {
    local account_name=$1
    local email=$2
    local provider=$3
    local token_file="$PASS_DIR/$account_name"

    echo ""
    echo "--- Setting up: $email ($provider) ---"

    if [ -f "$token_file" ]; then
        echo "Token file already exists: $token_file"
        read -p "Overwrite? (y/N): " overwrite
        if [[ ! "$overwrite" =~ ^[Yy]$ ]]; then
            echo "Skipping $account_name"
            return
        fi
    fi

    echo ""
    echo "To set up OAuth2 for $email, you need:"
    if [ "$provider" = "microsoft" ]; then
        echo "1. Register an app at: https://portal.azure.com/#blade/Microsoft_AAD_RegisteredApps/ApplicationsListBlade"
        echo "2. Set redirect URI to: http://localhost:8080/"
        echo "3. Grant permissions: IMAP.AccessAsUser.All, SMTP.Send, offline_access"
        echo "4. Copy the Application (client) ID and create a client secret"
        echo ""
        echo "For detailed steps, see: https://github.com/UvA-FNWI/M365-IMAP"
    elif [ "$provider" = "google" ]; then
        echo "1. Create project at: https://console.cloud.google.com/"
        echo "2. Enable Gmail API"
        echo "3. Create OAuth 2.0 credentials (Desktop app)"
        echo "4. Download credentials JSON"
    fi

    echo ""
    read -p "Have you completed the app registration? (y/N): " ready
    if [[ ! "$ready" =~ ^[Yy]$ ]]; then
        echo "Skipping $account_name - run this script again when ready"
        return
    fi

    echo ""
    echo "Running mutt_oauth2.py to authorize $email..."
    echo "This will open a browser for authentication."
    echo ""

    # Create initial token file with basic structure and secure permissions
    cat > "$token_file" <<EOF
{
  "access_token": "",
  "token_type": "Bearer",
  "expires_in": 0,
  "scope": "",
  "refresh_token": "",
  "id_token": ""
}
EOF
    chmod 600 "$token_file"

    echo "Please run the following command manually to complete authorization:"
    echo ""
    echo "  mutt_oauth2.py $token_file --authorize"
    echo ""
    echo "The token file has been created at: $token_file"

    # Create mail directory for this account
    local mail_subdir="$MAIL_DIR/$email"
    if [ ! -d "$mail_subdir" ]; then
        echo "Creating mail directory: $mail_subdir"
        mkdir -p "$mail_subdir/INBOX"
    fi
}

# Set up NTNU account
setup_oauth2_account "ntnu.no" "rafael.palomar@ntnu.no" "microsoft"

# Set up OUS/UiO account
setup_oauth2_account "uio.no" "rafaelpa@uio.no" "microsoft"

# Optional: Gmail account
read -p "Do you want to set up Gmail account (rafaelpalomaravalos@gmail.com)? (y/N): " setup_gmail
if [[ "$setup_gmail" =~ ^[Yy]$ ]]; then
    setup_oauth2_account "rafaelpalomaravalos_gmail.com" "rafaelpalomaravalos@gmail.com" "google"
fi

echo ""
echo "=== Next Steps ==="
echo ""
echo "1. For each account, run the authorization command shown above:"
echo "   mutt_oauth2.py ~/.password-store/email/<account> --authorize"
echo ""
echo "2. Initialize the mu database:"
echo "   mu init --maildir=~/.local/share/mail --my-address=rafael.palomar@ntnu.no --my-address=rafael.palomar@ous-research.no"
echo ""
echo "3. Sync your email:"
echo "   mbsync -a"
echo ""
echo "4. Index your email:"
echo "   mu index"
echo ""
echo "5. Open Emacs and run: M-x mu4e"
echo ""
echo "6. Fix the configuration issue in emacs.org:"
echo "   The OUS-Research maildir paths don't match between .mbsyncrc and emacs.org"
echo "   Either change .mbsyncrc to use 'rafaelpa@ous-research.no' or update emacs.org"
echo ""
echo "For troubleshooting, check:"
echo "  - ~/.cache/msmtp.log (sending errors)"
echo "  - mutt_oauth2.py ~/.password-store/email/<account> --test (test OAuth2 tokens)"
echo ""
