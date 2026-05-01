---
name: research-papers
description: Search academic literature and download paper PDFs via `paper-search` / `paper-fetch`. Use when the user asks to find papers on a topic, look up works by an author, pull down a DOI/paper, add a paper to their library, or list what's in the library. Primary sources are Scopus (needs NTNU VPN) and OpenAlex (always available); PDFs come from Unpaywall first, then the NTNU VPN publisher fallback. Always surface Scopus quota use sparingly; never redistribute Scopus data.
---

# research-papers

Search and fetch academic papers through the user's research-infrastructure CLI wrappers.  The tooling is local-first:

- **Discovery**: `paper-search` hits Scopus (authoritative, curated) when the NTNU VPN is up and `pass show apis/scopus` yields a key, and OpenAlex (open citation graph, no auth) always.  Output is JSON lines by default, `--pretty` for human reading.
- **Fetch**: `paper-fetch <doi>` tries Unpaywall (legal OA copies) first, then the publisher via the doi.org resolver through the NTNU VPN.  PDFs land in `~/pks/library/<doi-slug>.pdf` (the user's Zettelkasten library silo).

## When to use

Trigger phrases:
- "find papers on …", "search for research on …", "what's been written about …"
- "look up papers by <author>", "what has <author> published on …"
- "get / pull / download paper <DOI or URL>", "add this paper to my library"
- "is DOI X in my library?", "what do I have on topic Y in my library?"

Do NOT use this skill for general web search about an author or concept — that's `WebSearch`.  Use this skill specifically for *academic literature retrieval*.

## Precondition checks before calling

Run `ntnu-vpn-status`.  If it exits 0, Scopus + paywalled-publisher fetch are available.  If it exits 1:
- Scopus search is skipped — `paper-search` will fall back to OpenAlex automatically, mention that to the user.
- `paper-fetch` will only succeed for OA material (Unpaywall path).  Tell the user: "this DOI is paywalled — running `ntnu-vpn-up` first will let me grab it".  **Do not offer to run `ntnu-vpn-up` yourself** — the Feide 2FA OTP is a human-only step.

## Search flow

1. Translate the user's ask into `paper-search` flags:
   - Free-text topic → positional arg.  e.g. `paper-search "vessel proximity segmentation" --limit 20`.
   - Specific author → `--author "Palomar R"`.  Name format is Scopus-style: "Lastname F".
   - Known DOI → `--doi 10.xxx/yyy`.
2. Run with `--pretty` if the user wants to read the list; with default JSON output if the result will be piped into `paper-fetch` or `jq`.
3. Summarize in chat: count of hits, top 3-5 with year / venue / citation count / DOI.  Highlight obviously relevant ones based on the user's stated goal.
4. If both Scopus and OpenAlex returned records, note that duplicates have been DOI-deduped.

## Fetch flow

1. Extract the DOI — may be given as a bare DOI (`10.xxx/yyy`), a `doi:…` URI, or a `https://doi.org/…` URL.  `paper-fetch` normalizes these itself; just pass whatever the user gave.
2. If `~/pks/library/<slug>.pdf` already exists, `paper-fetch` will say so and exit 0 — relay that, do not re-fetch.
3. On success, the script prints the destination path on stdout; confirm the file exists and report its path.
4. On failure (publisher not cooperative, no VPN, bad DOI), surface the script's stderr verbatim — its messages are specific ("non-PDF content-type", "NTNU VPN is down", etc.).

## Combining search + fetch

Common pipelines the user may ask for:

```bash
# First page of matches → download those the user picks
paper-search "liver vessel" --limit 10 --pretty
# user points at a few → paper-fetch each DOI one at a time

# Bulk download from a search (only do this after an explicit user ask)
paper-search --author "Palomar R" | jq -r '.doi // empty' \
  | while read d; do paper-fetch "$d"; done
```

Do not bulk-fetch without explicit "get them all" / "download everything matching" — each request hits publisher infrastructure and (for Scopus) counts against quota.

## Library queries

`~/pks/library/` holds the PDFs and `references.bib` (user's BibTeX).  To answer "do I have X in my library":

```bash
ls ~/pks/library/ | grep -i <doi-fragment-or-keyword>
# or search the bib
grep -iE 'title|author|doi' ~/pks/library/references.bib | grep -i <term>
```

Don't modify `references.bib` unless the user explicitly asks — the user maintains it manually for now.

## Scopus-specific rules

- Scopus key lives in `pass show apis/scopus`.  `paper-search` reads it itself; do not exfiltrate, echo, or paste the key anywhere.
- Scopus API is IP-bound to NTNU's range — the VPN is a hard prerequisite.
- Scopus ToS forbids bulk caching / redistribution of results.  Interactive user queries are fine; long-running scrapes are not.  If the user asks for something that looks like a scrape ("fetch all papers citing X since 2020"), warn once and cap the batch.
- Default quota on an institutional key is ~20k ScopusSearch / week.  If the user asks repeatedly in one session, quietly prefer OpenAlex after a few Scopus calls.

## Links for the user to take action

- Bring the VPN up: `ntnu-vpn-up` (prompts for NTNU password + Feide OTP — agent cannot do this).
- Check VPN state: `ntnu-vpn-status`.
- Register a Scopus key (one-time, on NTNU IP): https://dev.elsevier.com/ → Create API Key, then `pass insert apis/scopus` (paste key as first line).

## Integration with PKS

Downloaded PDFs sit in the user's PKS library silo (`~/pks/library/`).  After a fetch, it is often appropriate to offer to create a literature note:

> "Want me to create a literature note in `~/pks/literature/` referencing this paper?  (denote ID link + 2-3 line summary is the convention.)"

Use `pks-create` (not this skill) for the actual note creation.  Do not create literature notes automatically — the user wants to decide which papers merit one.
