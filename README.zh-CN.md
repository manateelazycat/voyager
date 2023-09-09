[English](./README.md) | 简体中文

### Voyager
Voyager 是一个基于 [DAP](https://microsoft.github.io/debug-adapter-protocol//) 协议的 Emacs 调试插件。

Voyager 主要的优势是基于 Python 多线程技术开发， 永远不卡顿 Emacs， 同时希望实现开箱即用的目标， 减少用户折腾的时间。

### 安装
1. 安装 Python 依赖: epc, sexpdata, six: `pip3 install epc sexpdata six`
2. 用 `git clone` 下载此仓库， 并替换下面配置中的 load-path 路径
3. 把下面代码加入到你的配置文件 ~/.emacs 中：

```elisp
(add-to-list 'load-path "<path-to-voyager>")

(require 'voyager)
(voyager-enable)
```

