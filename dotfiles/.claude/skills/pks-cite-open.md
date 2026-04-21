---
name: pks-cite-open
description: For a citation key, return the paths to the associated PDF (under ~/pks/library/papers) and the literature note (under ~/pks/literature). Read-only — returns paths so Claude can Read them or the user can open them; does not launch an external viewer.
---

# pks-cite-open

Resolve a citekey to on-disk artefacts.

## Flow

1. Verify citekey exists:

   ```
   rg -c '@\w+\{<citekey>,' ~/pks/library/references.bib
   ```

   If zero matches, report "citekey not found in bibliography" and
   stop.

2. Find the PDF:

   ```
   ls ~/pks/library/papers/<citekey>.* 2>/dev/null
   ls ~/pks/library/books/<citekey>.* 2>/dev/null
   ```

   If neither matches, report "PDF not attached — use
   `citar-add-file-to-library` in Emacs to attach one".

3. Find the literature note:

   ```
   rg -l '^#\+reference:\s*@<citekey>' ~/pks/literature
   ```

   If none, report "no literature note yet — use Emacs citar
   `citar-open-notes` to create one".

4. Return a structured response:

   ```
   Citekey: @<key>
   Bibentry: ~/pks/library/references.bib (line <N>)
   PDF:      ~/pks/library/papers/<citekey>.pdf  (or Absent)
   Note:     ~/pks/literature/<id>--...__lit.org (or Absent)
   ```

5. If the user wants to read the note in context, use `pks-read <ID>`
   next. If they want to read the PDF, suggest opening in Zathura (the
   user's preferred PDF reader):

   ```
   zathura ~/pks/library/papers/<citekey>.pdf &
   ```

## Safety

Read-only. Never copy, move, or rename PDFs from this skill.
