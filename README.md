English | [简体中文](./README.zh-CN.md)

### Voyager
Voyager is an Emacs debugging plugin based on the [DAP](https://microsoft.github.io/debug-adapter-protocol//) protocol.

The main advantage of Voyager is that it is developed based on Python multithreading technology, it never freezes Emacs, and at the same time, it aims to achieve the goal of being ready to use out of the box, reducing the time users spend on tinkering.

### Installation
1. Install Python dependencies: epc, sexpdata, six: `pip3 install epc sexpdata six`
2. Use `git clone` to download this repository, and replace the load-path path in the configuration below
3. Add the following code to your configuration file ~/.emacs:

```elisp
(add-to-list 'load-path "<path-to-voyager>")

(require 'voyager)
(voyager-enable)
```

