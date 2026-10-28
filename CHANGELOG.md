# Changelog for rust-notebook-language-server

## 0.2.11.0

* Transform the call and type hierarchies, whose answers named the shadow document and so couldn't be followed
* Replace a document symbol we can't place with its children, instead of dropping both

## 0.2.10.0

* Recognize the shadow document in an answer however the server escaped its path
* Rewrite the shadow document's name and line inside a hover's own text, which until now kept the shadow document's name

## 0.2.9.0

* Transform workspace/symbol answers, which named the shadow document instead of the notebook

## 0.2.8.0

* Transform textDocument/rename, prepareRename, references, signatureHelp and declaration, which reached the server naming the notebook

## 0.2.7.1

* Fix textDocument/selectionRange answering nothing, by cutting a selection range chain where it leaves the cell

## 0.2.7.0

* Transform textDocument/selectionRange, textDocument/semanticTokens and the goto family, which reached the client in shadow coordinates

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
