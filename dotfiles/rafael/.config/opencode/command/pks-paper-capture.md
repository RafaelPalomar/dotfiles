---
description: "Capture a paper into the PKS — fetch the PDF via `paper-fetch`, append a BibTeX stanza to `~/pks/library/references.bib`, and create a structured Denote literature note in `~/pks/literature/{work,personal}/` with org-noter binding. Use when the user gives a DOI / arXiv ID / paper URL and says 'add this', 'capture this', 'make a lit note for X', 'import this paper', or similar. Triple confirmation: confirms before fetch, before bib append, before note creation. Single-paper only — never bulk."
---
# pks-paper-capture

Single-paper capture pipeline.  Lands a paper in three places atomically:

1. **PDF** at `~/pks/library/<doi-slug>.pdf` (via `paper-fetch`)
2. **BibTeX** stanza appended to `~/pks/library/references.bib`
3. **Literature note** at `~/pks/literature/{work,personal}/<denote-id>--<slug>__lit_<keywords>.org` (via `denotecli` + `Write`)

These three are coupled: the bib citekey, the lit note's `#+reference:` front matter, and the PDF filename all point at the same paper.  This skill exists to keep them in sync — `research-papers.md` only handles search + fetch.

## When to use

Trigger phrases:
- "add this paper / DOI", "capture this", "import <DOI>"
- "make a lit note for <paper>", "register this in my PKS"
- after a `paper-search` result: "grab the third one and write it up"

Do NOT use for:
- *Searching* — that's `research-papers.md`.
- *Promoting* a lit note to permanent — that's `pks-promote.md`.
- *Bulk* capture — refuse and ask the user to pick one at a time.  BibTeX collisions and citekey disambiguation get out of hand fast at scale.

## Preconditions

1. `ntnu-vpn-status` — for paywalled fetch.  Same VPN rules as `research-papers.md`: never run `ntnu-vpn-up` yourself.
2. `~/pks/library/references.bib` exists (may be empty — that's fine).
3. The user has named a domain — `work` or `personal`.  Default `work` if the paper is research / clinical / OUS / NTNU; `personal` if explicitly personal reading.  Confirm if ambiguous.

## Step 1 — Resolve metadata

Extract the DOI from whatever the user gave (bare `10.x/y`, `doi:…`, URL, arXiv ID).  Fetch metadata:

```bash
paper-search --doi <DOI> --pretty
```

You need: `title`, `authors` (list, last-name first-initial form), `year`, `venue` (journal / proceedings / preprint server), `volume`, `issue`, `pages`, `doi`, optional `url`.  arXiv IDs: hit OpenAlex or Crossref via `paper-search` — the wrapper handles both.

If metadata fetch fails (no record, VPN down, malformed DOI), ask the user for the missing fields rather than guessing.  Do not fabricate authors / years.

## Step 2 — Generate the citekey

Convention: `<lastname><year><titleword>`, all lowercase, ASCII-only.

- `lastname`: first author's last name, stripped of accents, hyphens removed.
- `year`: 4-digit publication year.
- `titleword`: first content word of the title (skip articles "a", "an", "the"; skip prepositions).  Lowercased.

Examples: `wurmus2018pigx`, `courtes2015reproducible`, `palomar2024vessel`.

**Collision check** before committing:

```bash
grep -E "^@\w+\{${citekey}," ~/pks/library/references.bib
```

If hit, append `b`, `c`, … to the citekey (`wurmus2018pigxb`).  Show the user the resolved key before any writes.

## Step 3 — Fetch the PDF

```bash
paper-fetch <DOI>
```

If `paper-fetch` reports the file already exists, accept that and move on.  Capture the destination path it prints — you need it for `NOTER_DOCUMENT` and the BibTeX `file =` field.

## Step 4 — Append BibTeX (confirm first)

Show the user the proposed stanza and ask "Append to `references.bib`? (y/n)".  On yes, append with a single newline separator:

```bibtex

@article{<citekey>,
  author    = {<Last1, F. and Last2, F. and ...>},
  title     = {<title>},
  journal   = {<venue>},
  year      = {<year>},
  volume    = {<volume>},
  number    = {<issue>},
  pages     = {<pages>},
  doi       = {<doi>},
  file      = {<absolute pdf path>}
}
```

Entry type by venue:
- Journal article → `@article`
- Conference paper → `@inproceedings` (use `booktitle =` instead of `journal =`)
- Preprint (arXiv, bioRxiv, medRxiv) → `@misc` with `howpublished = {arXiv:<id>}` or `howpublished = {bioRxiv}` plus `eprint =` and `eprinttype =`
- Book chapter → `@incollection`
- Book → `@book`

Use the `Edit` tool to append (read the file first if non-empty), or `Write` if the file is empty / brand new.  Never use `echo >>` — the `Edit` tool tracks state and avoids races with the user's editor.

## Step 5 — Create the literature note (confirm first)

Show the user the proposed note plan:

```
Silo:     ~/pks/literature/<domain>
Title:    <Lastname YYYY — <short title>>
Keywords: lit, <domain-keyword e.g. ous|ntnu>, <topic keywords>
Citekey:  @<citekey>
PDF:      ~/pks/library/<doi-slug>.pdf
```

Domain keyword convention: `ous` / `ntnu` for institutional research, plus topic tags from the closed vocabulary (`research`, `code`, `learn`, `lit`).  Warn if a new keyword is needed.

On confirmation, create the skeleton via denotecli:

```bash
denotecli create \
  --title "<Lastname YYYY — <short title>>" \
  --tags lit,<domain>,<topic> \
  --dir ~/pks/literature/<work|personal>
```

denotecli returns the created file's JSON.  Capture its `path`.  Then **overwrite** the file with the full skeleton via the `Write` tool (denotecli's `--content` is awkward for multi-section bodies, and the entelequia stdin patch is not portable to all hosts):

```org
#+title:      <Lastname YYYY — <short title>>
#+date:       [<current date>]
#+filetags:   :lit:<domain>:<topic>:
#+identifier: <denote-id>
#+reference:  @<citekey>
#+export_file_name: <denote-id>.md

* Citation
<Authors>.  /<title>./  <venue> <volume>(<issue>):<pages>, <year>.
DOI: [[https://doi.org/<doi>][<doi>]].
PDF: [[file:~/pks/library/<doi-slug>.pdf]].

* Abstract
<paste from metadata if available — otherwise leave the heading empty for the user to fill via org-noter>

* Distilled claims (Tier-1)
# Atomic claims live here as subheadings.  Each one is a candidate for
# promotion to permanent/ via pks-promote.

* Open questions / followups

* Annotations (org-noter)
:PROPERTIES:
:NOTER_DOCUMENT: ~/pks/library/<doi-slug>.pdf
:END:
```

Use `~/pks/library/...` literally in `NOTER_DOCUMENT` — org expands the tilde.

## Step 6 — Report back

Show the user:
- Citekey + bib entry path
- Literature note path
- PDF path
- A one-liner: "Open in org-noter via `C-c C-x C-v` on the Annotations heading, or `M-x org-noter` from inside the note."

## Idempotency / re-runs

If the user asks to capture a paper already in their PKS:
- `paper-fetch` skips and reports the existing PDF.
- Check `grep -E "doi.*=.*${doi}" ~/pks/library/references.bib` — if hit, do not duplicate the bib entry; report the existing citekey.
- Check `rg -l '^#\+reference:\s*@<citekey>' ~/pks/literature` — if hit, surface the existing note path; do not create a duplicate.

Capture is a single-shot operation per paper.  If the user wants to *update* an existing entry (e.g., the preprint got a journal DOI), that's a different ask — make the edits explicitly, do not re-run this skill.

## Safety rules

1. **Triple confirmation**: fetch, bib append, note creation are three separate confirmations.  The user can stop at any step.
2. **Never bulk**.  One paper per skill invocation.
3. **Never regenerate denote IDs** — denotecli generates one on create; that's the only one.
4. **Never delete** a bib entry or lit note here — this skill only adds.  Removal is a manual user step.
5. **Citekey is load-bearing**: it appears in `references.bib`, in the lit note's `#+reference:` header, and (via citar) in any future manuscript draft.  Once committed, do not rename casually.
6. **Domain is physical** (directory), per `CLAUDE.md`.  Pick `work/` or `personal/`; do not invent a `_personal` keyword.

## See also

- `research-papers.md` — discovery + fetch (read-only on the bib).
- `pks-cite-search.md` — query the bib + correlate with lit notes (read-only).
- `pks-promote.md` — lit note → permanent note (the Phase 3 step).
- `pks-create.md` — generic note creation; this skill does not delegate to it because the literature template is specialised.
