# 🌳 tree-sitter-clingo [![ci-badge]][ci] [![pypi-version-badge]][pypi] [![npm-version-badge]][npm] [![crates-version-badge]][crates] [![rust-doc-badge]][rust-doc]

This repository provides the [tree-sitter] grammar for [clingo] language, a
system for Answer Set Programming (ASP) developed by the [Potassco][potassco]
group.

## 📦 Installation

- **Python:** `pip install tree-sitter-clingo` ([PyPI][pypi])
- **Node.js:** `npm install tree-sitter-clingo` ([npm][npm])
- **Rust:** `cargo add tree-sitter-clingo` ([crates.io][crates])
- **C:** Build with `tree-sitter build`

## 🔗 Related Projects

- [clingo-syntax.nvim]: Neovim plugin for clingo syntax highlighting using this project.

## 📋 Release Checklist

When preparing a new release, ensure the version is updated consistently in the
following files:

- `package.json`
- `package-lock.json` (run `tree-sitter generate` to update)
- `Cargo.toml`
- `Cargo.lock` (run `cargo update` to update)
- `pyproject.toml`
- `Makefile`

[tree-sitter]: https://tree-sitter.github.io/tree-sitter/
[clingo]: https://github.com/potassco/clingo
[potassco]: https://potassco.org/
[clingo-syntax.nvim]: https://github.com/rkaminsk/clingo-syntax.nvim
[ci-badge]: https://github.com/potassco/tree-sitter-clingo/workflows/CI%20test/badge.svg
[ci]: https://github.com/potassco/tree-sitter-clingo/actions/workflows/ci-test.yml
[crates-version-badge]: https://img.shields.io/crates/v/tree-sitter-clingo.svg
[crates]: https://crates.io/crates/tree-sitter-clingo
[rust-doc-badge]: https://img.shields.io/badge/api-rustdoc-blue.svg
[rust-doc]: https://docs.rs/tree-sitter-clingo
[pypi-version-badge]: https://img.shields.io/pypi/v/tree-sitter-clingo.svg
[pypi]: https://pypi.org/project/tree-sitter-clingo/
[npm-version-badge]: https://img.shields.io/npm/v/@potassco/tree-sitter-clingo.svg
[npm]: https://www.npmjs.com/package/@potassco/tree-sitter-clingo
