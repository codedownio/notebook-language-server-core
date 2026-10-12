# notebook-language-server-core

Shared plumbing for the notebook language server proxies: rust-, cpp-, go- and
haskell-notebook-language-server.

Each of those wraps a language server so it can serve a notebook, projecting cells into a
document the server accepts and rewriting requests and responses between the two coordinate
spaces. The projection is per-language; everything around it is not, and lives here.
