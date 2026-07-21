# Zhihu On Emacs

在 Emacs 中使用 Markdown/Typst/Org 作为源文件来撰写、存档并发布知乎回答和文章，并可以指定文章专栏。

> [!WARNING]
> 本项目使用知乎非公开的网页接口，知乎可能随时改变接口行为。


## 依赖

- Emacs 29.1 或更高版本，并启用 SQLite 和 libxml 支持
- [`yaml.el`](https://github.com/zkry/yaml.el)
- [`pandoc`](https://pandoc.org/)
- 可选：如果使用 Typst 源文件，[`typst`](https://typst.app/)和`typst-ts-mode` 或 `typst-mode`

## 配置

请先在 Firefox 中登录知乎。本包会自动发现原生 Linux 或 macOS 的 Firefox
profiles，也会读取 `profiles.ini` 中配置的自定义路径；存在多个 profile 时
自动选择 `cookies.sqlite` 最近更新的一个，无需配置 Cookie 路径。

本包默认不占用任何键位，有需要的情况下请自行绑定。

## 工作流

在某种情况下，

提供问题ID或者URL，写新回答：

```text
M-x zhihu-new-answer
```

写新文章：

```text
M-x zhihu-new-article
```

在发布之后，为了让源稿和知乎文章同步，我们会写回知乎回答/文章ID和图床缓存数据入源文件。

已有的文件可以通过添加对应元数据发布。

更新已有知乎文章/回答，或者把已有文件发布到知乎
```text
M-x zhihu-publish
```

## 元数据示例

**Typst**:

```typst
#metadata((
  question-id: "123456",
  answer-id: none,
)) <zhihu>
```

```typst
#metadata((
  article-id: none,
)) <zhihu>

#set document(title: "示例标题")
```

**Markdown**

```yaml
---
title: 示例标题
zhihu:
  question-id: "123456"
  answer-id: "789012"
---
```


```yaml
---
title: 示例文章
zhihu:
  article-id: null
---
```

需要加入专栏时，在 `zhihu` 下额外填写 `column-id`。发布文章后会检查当前
专栏，尚未收录时才发起收录。Typst 也使用相同字段。

**Org:**


- `#+ZHIHU_QUESTION_ID`：问题 ID。
- `#+ZHIHU_ANSWER_ID`：回答 ID；新回答首次发布前可以没有。
- `#+ZHIHU_ARTICLE_ID`：文章 ID；空值表示尚未发布的新文章。
- `#+ZHIHU_COLUMN_ID`：文章要加入的专栏 ID；发布后会检查并收录。
- `#+ZHIHU_IMAGE_CACHE`：非空的图片上传缓存，值为单行 JSON；空缓存不写。

## 致谢

- [`zhihu.nvim`](https://github.com/pxwg/zhihu.nvim)：发布 payload、浏览器
  Cookie 读取和图片上传协议的主要参考实现。
- [`zhihu-sign-kt`](https://github.com/zly2006/zhihu-sign-kt)：ZSE v4 签名
  算法的 MIT 许可实现。
