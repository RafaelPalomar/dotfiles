---
name: mail-draft
description: Compose a mail draft and write it as an RFC-5322 file into the appropriate Maildir Drafts folder. The user reviews and sends from mu4e (or any IMAP client) — agent NEVER sends, never touches SMTP, OAuth, or mbsync. Use for "draft a reply to that thread", "draft a new email to X about Y", "prepare a response I can review".
---

# mail-draft

Creates a draft in the local Maildir. On the user's next `sync-mail`,
`mbsync` pushes it to the IMAP Drafts folder; the human reviews and
sends from mu4e or Outlook.

## When to invoke

- "Draft a reply to the thread about the grant review"
- "Write an email to <person> thanking them for X"
- "Prepare a polite decline for this meeting invite"

**Do NOT invoke for**: searching mail (that's `mail-triage`), sending,
triggering a sync, or authorising OAuth.

## Hard rules

1. **NEVER** call `msmtp`, `sendmail`, `smtpmail`, or any command that
   opens a socket to a mail *SMTP* server. Send authority belongs on
   a human keystroke in mu4e / Outlook — never the agent.
2. **NEVER** call `mutt_oauth2.py` / `auth-email-*` or touch
   `~/.password-store/**` / `~/.dotfiles/sops/**` — auth is off-limits.
3. **NEVER** call `mbsync`, `sync-mail`, `sync-mail-agent-index`, or
   raw `notmuch`. All four are denylisted. Syncing the draft to IMAP
   is a human-initiated step; the agent's job ends at writing the
   Maildir file.
4. **ALWAYS** use the `mail-draft` helper script (see below). Do not
   hand-construct RFC-5322 in shell; the helper handles encoding,
   Message-ID generation, Maildir flags, and atomic move via `tmp/`.
5. **Always confirm** the draft contents with the user before writing
   if the body exceeds a couple of sentences. A wrong draft can be
   edited; a surprise draft is friction.
6. **Never draft to / quote from / reference** anything in the
   Sensitive folder. The agent's DB doesn't index Sensitive, so this
   should be automatic — but if the user asks you to reply to a
   thread you can't see, surface that rather than guessing.

## The helper

```
mail-draft --account ous|ntnu \
           --to "addr@example.com[,addr2@…]" \
           [--cc "…"] [--bcc "…"] \
           --subject "…" \
           [--in-reply-to '<msg-id>'] \
           [--references '<id1> <id2>'] \
           [--no-signature]
```

Body is read from stdin. The helper:
- Picks the right `From:` and signature for the account.
- Sets a fresh `Message-ID`.
- Writes to `~/.local/share/mail/<account>/Drafts/cur/<unique>:2,DS`
  atomically via `Drafts/tmp/`.
- Prints the written path on stdout.

## Workflow (reply)

1. Use `mail-triage` first to find the thread:
   `notmuch-agent show --format=json --entire-thread 'thread:<id>'`
2. From the thread JSON, extract:
   - The **last** message's `Message-ID` header → `--in-reply-to`.
   - The `References` header of the last message, appended with its
     `Message-ID`, → `--references`.
   - The original `From:` → becomes `--to`.
   - The original `Subject:` → prepend `Re: ` if not already present
     → `--subject`.
3. Which account? Look at the maildir path in the thread JSON:
   `/rafael.palomar@ous-research.no/**` → `--account ous`;
   `/rafael.palomar@ntnu.no/**` → `--account ntnu`.
4. Compose the body. Quote the message you're replying to with `> `
   prefixes; add your reply above.
5. Pipe body into `mail-draft`:

   ```bash
   mail-draft \
     --account ous \
     --to "sender@example.com" \
     --subject "Re: roadmap review" \
     --in-reply-to '<abc123@example.com>' \
     --references '<prev@example.com> <abc123@example.com>' \
     <<'BODY'
   Hi Jane,

   Thanks for the notes. I agree with points 1 and 3; on point 2
   I'd like to discuss further — how's Tuesday afternoon?

   > Jane wrote:
   > [quoted excerpt]
   BODY
   ```

6. **Tell the user** the draft path and that their next `sync-mail`
   will upload it to IMAP Drafts. Do not run `sync-mail` yourself —
   it is denylisted. One-line reminder is enough:

   > Draft written to `~/.local/share/mail/ous/Drafts/cur/…:2,DS`.
   > Run `sync-mail` in your shell to upload it to Outlook Drafts;
   > review there or in mu4e, then `C-c C-c` / Send when ready.

## Workflow (new message)

1. Pick the account (OUS vs NTNU). If ambiguous, ask. Default: OUS
   for external correspondence, NTNU only when writing from that role.
2. Construct subject, recipients, body.
3. Skip `--in-reply-to` / `--references`.
4. Same helper invocation.
5. Same reminder to the user about `sync-mail`.

## Format guidance

- **Plain text**, no HTML. Our `mu4e-compose-format-flowed` ensures
  the plaintext reflows cleanly on mobile and institutional clients.
  If the user specifically asks for HTML: don't — instead, advise
  them to compose in mu4e and use `C-c M-o` (`org-mime-htmlize`)
  interactively.
- **Quoting**: 1–3 lines of the relevant fragment, prefixed with
  `> `, below your reply. Don't full-quote.
- **Signature**: the helper appends the correct per-account signature
  automatically. Do not duplicate. Pass `--no-signature` only if the
  user explicitly asked for none.
- **Language**: mirror the language of the thread. Norwegian ↔
  Norwegian, English ↔ English. If in doubt, ask.

## Failure modes

- Helper not on PATH → user hasn't redeployed the updated home
  profile. Do NOT substitute a shell-constructed fallback; ask them
  to run `guix home reconfigure`.
- Account's Drafts directory doesn't exist → helper creates it. If it
  still fails, maildir root is wrong; inspect `~/.local/share/mail/`.
- `notmuch-agent` doesn't see the draft immediately after writing →
  expected. The draft is in Maildir, but the agent's index is only
  refreshed when the user runs `sync-mail`. The draft is already
  safely on disk.

## What NOT to do

- Do not invoke `emacsclient --eval '(mu4e-compose-new ...)'` — that
  opens a compose buffer in the user's Emacs; useful interactively but
  side-steps the review step.
- Do not write files directly to `Drafts/new/` or `Drafts/cur/` by
  hand — Maildir's atomicity requires writing to `tmp/` first, and
  the Write/Edit tools are denylisted on `~/.local/share/mail/**`
  regardless.
- Do not set `X-Mailer: Claude` or similar identifiers that leak
  agent authorship to the recipient. The draft is yours after review.
- Do not run `sync-mail` yourself "to close the loop". It is
  denylisted by design; the human does it.
