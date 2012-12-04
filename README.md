# helm-pydoc.el

## Introduction
`helm-pydoc.el` is pydoc helm interface

## Screenshot

![helm-pydoc](https://github.com/syohex/emacs-helm-pydoc/raw/master/image/helm-pydoc.png)


## Requirements

* Emacs 22.1 or higher
* helm 1.0 or higher


## Basic Usage

`pydoc` with helm interface

    M-x helm-pydoc


## Sample Configuration

`git clone` helm-pydoc.el and add it to load-path.

```` elisp
(require 'helm-config)
(add-to-list 'load-path "your cloned directory")
(require 'helm-pydoc)
````
