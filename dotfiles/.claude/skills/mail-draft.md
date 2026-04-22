---
name: mail-draft
description: Compose a mail draft and write it as an RFC-5322 file into the appropriate Maildir Drafts folder. The user reviews and sends from mu4e (or any IMAP client) — agent NEVER sends, never touches SMTP, OAuth, or mbsync. Use for "draft a reply to that thread", "draft a new email to X about Y", "prepare a response I can review".
---

# mail-draft

Creates a draft in the local Maildir. `mbsync` (on next `sync-mail`)
pushes it to the IMAP Drafts folder; the human sends it from mu4e.

## When to invoke

- "Draft a reply to the thread about the grant review"
- "Write an email to <person> thanking them for X"
- "Prepare a polite decline for this meeting invite"

**Do NOT invoke for**: searching mail (that's `mail-triage`), sending,
triggering a sync, or authorising OAuth.

## Hard rules

1. **NEVER** call `msmtp`, `sendmail`, `smtpmail`, or any command that
   opens a socket to a mail *SMTP* server.  Send authority belongs on
   a human keystroke in mu4e / Outlook — never the agent.
2. **NEVER** call `mutt_oauth2.py` / `auth-email-*` or touch
   `~/.password-store/**` / `~/.dotfiles/sops/**` — auth is off-limits.
3. **ALWAYS** use the `mail-draft` helper script (see below). Do not
   hand-construct RFC-5322 in shell; the helper handles encoding,
   Message-ID generation, Maildir flags, and atomic move via `tmp/`.
4. **Always confirm** the draft contents with the user before writing
   if the body exceeds a couple of sentences. A wrong draft can be
   edited; a surprise draft is friction.
5. **Always close the loop with `sync-mail`** after writing a draft
   (see Workflow below).  `mbsync` uploads the draft to Exchange's
   IMAP Drafts folder — this is self-publication to your own Drafts,
   not a send — so Outlook on phone/web/desktop sees it within
   seconds.

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
   `notmuch show --format=json --entire-thread 'thread:<id>'`
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

6. **Run `sync-mail`** to push the draft to IMAP Drafts + reindex
   notmuch:

   ```bash
   sync-mail     # = mbsync -a && notmuch new
   ```

   Typical output: `Far: +1 *0 #0 -0` on the relevant channel = your
   draft was uploaded.  This is self-publication, not a send — safe
   and expected after every `mail-draft`.

7. Report the returned path + a one-line summary to the user.  Mention
   where they'll see it (Outlook Drafts on the matching account, or
   `SPC m m` → Drafts in mu4e).  Remind them the draft is *not sent*
   until they hit Send / `C-c C-c` in the client.

## Workflow (new message)

1. Pick the account (OUS vs NTNU). If ambiguous, ask. Default: OUS
   for external correspondence, NTNU only when writing from that role.
2. Construct subject, recipients, body.
3. Skip `--in-reply-to` / `--references`.
4. Same helper invocation.
5. **Run `sync-mail`** to push (same as step 6 in the reply workflow).
6. Report path + where the user will see it.

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
- `notmuch` doesn't see the draft immediately after writing → that's
  expected. `notmuch new` picks it up; the user's mu4e indexes it on
  next `u`. The draft is already safely in Maildir.

## What NOT to do

- Do not invoke `emacsclient --eval '(mu4e-compose-new ...)'` — that
  opens a compose buffer in the user's Emacs; useful interactively but
  side-steps the review step.
- Do not write files directly to `Drafts/new/` or `Drafts/cur/` by
  hand — Maildir's atomicity requires writing to `tmp/` first.
- Do not set `X-Mailer: Claude` or similar identifiers that leak
  agent authorship to the recipient. The draft is yours after review.
