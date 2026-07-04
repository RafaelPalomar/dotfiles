---
name: pks-cite-search
description: Search the user's bibliography at ~/pks/library/references.bib for citekeys by author, year, title, or keyword. Optionally cross-references the associated literature note in ~/pks/literature if one exists. Read-only.
---

# pks-cite-search

Query the central bibliography and correlate with literature notes.

## Commands

### Basic query on .bib

```
rg -i --no-heading --line-number '<pattern>' ~/pks/library/references.bib
```

Patterns to try in order of specificity:
- `@article\{<word>` — citekey prefix
- `author = \{[^}]*<name>` — author name
- `year = \{<year>\}` — year
- `title = \{[^}]*<keyword>` — title substring

### Structured parsing (preferred when available)

```
bibtool -r ~/pks/library/references.bib -- -q '<query>'
```

If `bibtool` is absent, fall back to `rg`.

### Correlate with literature notes

For each citekey found, check whether a literature note exists:

```
denotecli search "<citekey>" \
  --dirs ~/pks/literature \
  --title-only \
  --max 1
```

Or search by `#+reference: @<citekey>` front-matter:

```
rg -l '^#\+reference:\s*@<citekey>' ~/pks/literature
```

## Output shape

```
@authorYearTitle
  Title:    ...
  Authors:  ...
  Year:     ...
  PDF:      ~/pks/library/papers/<citekey>.pdf (present/absent)
  Note:     ~/pks/literature/<denote-id>--...__lit.org (present/absent)
```

## Safety

Read-only. For creating a literature note from a citekey, hand off to
the user's Emacs citar workflow (`C-c n r n` / `citar-open-notes`).
denotecli can also create one (`pks-create --dir ~/pks/literature
--tags lit`) but citar-denote handles the `#+reference:` front-matter
automatically — preferred.
