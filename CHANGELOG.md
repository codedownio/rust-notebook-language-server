# Changelog for rust-notebook-language-server

## Unreleased changes

* Map code action edits back to the notebook, instead of returning the shadow file's uri and lines

## 0.2.5.0

* Support textDocument/formatting, textDocument/onTypeFormatting and textDocument/inlayHint

## 0.2.2.0

* Add debounced textDocument/didSave command and --did-save-period-ms argument

## 0.2.1.0

* Fix an issue with `untransformPosition`
