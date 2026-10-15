# Changelog for rust-notebook-language-server

## 0.2.6.1

* Untransform workspace/applyEdit, which named the shadow document and so was refused by the client

## 0.2.6.0

* Map code action edits back to the notebook instead of the shadow file
* Transform codeLens/resolve, so lens titles that the server defers resolve correctly

## 0.2.5.0

* Support textDocument/formatting, textDocument/onTypeFormatting and textDocument/inlayHint

## 0.2.2.0

* Add debounced textDocument/didSave command and --did-save-period-ms argument

## 0.2.1.0

* Fix an issue with `untransformPosition`
