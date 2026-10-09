# Changelog for rust-notebook-language-server

## Unreleased changes

* Support textDocument/formatting and textDocument/onTypeFormatting, which were passed through
  with the notebook's URI and so always failed with "file not found"

## 0.2.2.0

* Add debounced textDocument/didSave command and --did-save-period-ms argument

## 0.2.1.0

* Fix an issue with `untransformPosition`
