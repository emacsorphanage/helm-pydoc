# helm-pydoc.el

## Introduction
`helm-pydoc.el` is pydoc helm interface


## Screenshot

![helm-pydoc](image/helm-pydoc1.png)

![helm-pydoc-action](image/helm-pydoc2.png)


## Requirements

* Emacs 24 or higher
* helm 1.0 or higher


## Basic Usage

#### `helm-pydoc`

`pydoc` with helm interface


## Actions

* Show module document
* View source code
* Import module statement
    * import module(Insert marked candidates)
    * from module import identifier
    * from module import identifier


## Sample Configuration

`git clone` helm-pydoc.el and add it to load-path.

```elisp
(require 'helm-config)
(add-to-list 'load-path "your cloned directory")
(require 'helm-pydoc)
```
