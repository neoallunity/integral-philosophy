# Integral Philosophy (monorepo)

[![CI](https://github.com/neoallunity/integral-philosophy/actions/workflows/ci.yml/badge.svg)](https://github.com/neoallunity/integral-philosophy/actions)
[![License: CC-BY-4.0](https://img.shields.io/badge/License-CC--BY--4.0-blue.svg)](LICENSE)
[![GitLab Mirror](https://img.shields.io/badge/mirror-GitLab-orange)](https://gitlab.com/dominicusin/integral-philosophy)

Unified framework integrating philosophy, web, and API subsystems into a single coherent monorepo.

## Structure

| Path | Description |
|------|-------------|
| `/` (root) | Graph build + orchestration |
| `/subprojects/integral-philosophy-core` | Core logic (Python) |
| `/subprojects/integral-philosophy-web` | Web frontend |
| `/modules/*` | Reusable modules (cli, core, docs, web) |
| `/web` | Web assets |
| `/legacy` | Historical snapshots |
| `/tests` | Test suites (incl. performance) |

## Development

```bash
nix develop        # or: pip install -e ./subprojects/integral-philosophy-core
pytest tests/
```

## Mirrors

Canonical source: **GitHub** (`neoallunity/integral-philosophy`).
A read-only mirror is maintained on **GitLab**.

## License

Content and code are licensed under [CC-BY-4.0](LICENSE) unless a submodule states otherwise.
