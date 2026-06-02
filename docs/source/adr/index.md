# Architecture Decision Records

This is the decision ledger for entelequia's **key/secret management,
fleet topology, and family-agent architecture**. It adopts the same ADR
discipline already in use in
`~/src/ivs-infrastructure/Docs/adr/` — a numbered, append-only record of
*why* each structural choice was made, so future contributors (and
future-self) pattern-match on the reasoning rather than re-deliberating.

Each ADR is admitted to the build individually as it is written. New
ADRs follow [`0000-template.md`](0000-template.md).

```{toctree}
:maxdepth: 1

0000-template
0001-trust-roots-and-identities
0002-pass-vs-sops-boundary
0003-ssh-key-pinning-identitiesonly
0004-scripted-ceremony-vs-master-in-pass
0005-home-work-separation-by-role
0006-family-assistant-agent-identities
0007-ops-deploy-approval-gate
0008-personal-nextcloud-work-onedrive
```

## Ledger

| ADR | Title | Status |
|---|---|---|
| 0001 | Trust roots & identities | Accepted |
| 0002 | The pass-vs-SOPS boundary | Accepted |
| 0003 | SSH key pinning with `IdentitiesOnly` | Accepted |
| 0004 | Scripted IronKey ceremony, not master-in-pass | Accepted |
| 0005 | Separate home from work by per-machine role | Accepted |
| 0006 | Family-assistant agent identities | Accepted |
| 0007 | Ops-deploy approval gate (scoped deviation) | Accepted |
| 0008 | Storage split: NextCloud native photos (personal) / OneDrive (work) | Accepted |
