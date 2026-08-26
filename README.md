# Zhihu On Emacs

在 Emacs 中使用 Markdown 或 Typst 作为源文件来撰写、存档并发布知乎
回答、文章和想法，并可以指定文章专栏。

> [!WARNING]
> 本项目使用知乎非公开的网页接口，知乎可能随时改变接口行为。同时此项目处于开发早期，由于作者仅使用 Firefox，在别的浏览器上的 cookie 读取未经过测试。

## 依赖

- Emacs 31.1，并启用 SQLite、libxml 和 GnuTLS 支持
- [`markdown-mode`](https://jblevins.org/projects/markdown-mode/) 2.7 或更新版本
- [`plz.el`](https://github.com/alphapapa/plz.el) master（`0.10-pre`；）
- [`yaml.el`](https://github.com/zkry/yaml.el)
- [`pandoc`](https://pandoc.org/) 3.1.10 或更高版本
- 可选：使用 Typst 源文件，或转换本地路径 / `data:` URL 中的 SVG 时，
  需要 PATH 中的 [`typst`](https://typst.app/) 0.15.0 或更高版本；编辑
  Typst 源文件可另装 `typst-ts-mode` 或 `typst-mode`
- 可选：如果 Markdown 使用 Mermaid 图，
  [`@mermaid-js/mermaid-cli`](https://github.com/mermaid-js/mermaid-cli)
  提供的 `mmdc`

## 配置

请先在受支持的浏览器中登录知乎。本包支持 Firefox、Chromium、Google
Chrome 和 Microsoft Edge；默认读取 Firefox。浏览器及其实际
profile 根目录都需要显式设置给`zhihu-cookie-profile-directory`。本包不会扫描或猜。

| 浏览器 | `zhihu-cookie-profile-directory` 的语义 | 固定读取的相对路径 |
| --- | --- | --- |
| Firefox | 例如 `xxxxxxxx.default-release/` | `cookies.sqlite` |
| Chromium / Chrome / Edge | 例如 `Default/` 或 `Profile 1/` | `Network/Cookies` |


本包仅支持 GNU/Linux，浏览器版本只支持各项目的最新稳定版。

回答/文章没有单独指定转载权限时，使用
`zhihu-publish-default-reprint-permission`（默认 `allowed`）；回答、文章或
想法没有单独指定评论权限时，使用
`zhihu-publish-default-comment-permission`（默认 `all`）。

Markdown 正文标题默认整体下移一级，为知乎页面标题保留 `h1`。如需保留
Markdown 源稿的自然标题层级，可将
`zhihu-enable-markdown-heading-level-shift` 设为 `nil`；Typst 不读取该选项。

如需在文章末尾自动追加 Creative Commons 许可引用，可通过`zhihu-article-cc-statement` 自定义，这一选项将会适用于所有文章。

## 工作流

提供问题ID或者URL，写新回答：

```text
M-x zhihu-new-answer
```

命令创建并打开回答源稿后会显式启用 `zhihu-mode`。

申请创建新的普通知乎专栏：

```text
M-x zhihu-new-column
```

专栏名称必填且最多 20 个字符，简介可空且最多 1000 个字符。封面是可选的，
本命令不会提示或上传封面。

写新文章或想法无需专用的新建命令：创建 `.typ`、`.md` 或 `.markdown`
源稿即可。保留空 `thought-id` 时作为新想法，保留空 `article-id` 时作为
新文章；文章需要文档标题，想法标题可选。需要指定专栏、话题或其它发布设置时，
再添加相应 metadata。

更新已有知乎文章、回答或想法，或者把已有文件首次发布到知乎：

```text
M-x zhihu-publish
```

发布过程中，本包会把服务端返回的知乎回答、文章或想法 ID 写回源文件；
其中空 `article-id` 或 `thought-id` 表示首次发布，非空值表示更新已有内容。

`zhihu-mode` 是不占用键位的编辑辅助 minor mode。本包不会扫描或自动识别
Markdown 和 Typst 文件；需要编辑辅助时手动运行 `M-x zhihu-mode`。
在 Markdown `zhihu:` mapping 或 Typst `<zhihu>`
dictionary 尚未写入冒号的直接字段名位置运行 `M-x completion-at-point`，
会按当前稿件类型和已有字段补全 `question-id:`、`article-id:`、
`thought-id:` 等合法键名，并排除重复字段。在 `column-id` 值槽运行
`M-x completion-at-point`，候选会显示当前账号可投稿的专栏名称和 ID，
选择后只把实际 ID 写入源稿。`topics` 的字符串元素也支持同样的
就地补全。

## 语法

在文章和回答中，数学公式、表格、加粗、斜体、标题层级、超链接、代码块和
脚注均受支持。采用各个源稿格式自带的语法。

### Typst HTML 导出的上游限制

Typst 源稿会先经过 Typst 官方 HTML 导出器。该功能目前仍是实验功能，重点是
生成语义 HTML，尚不输出用于还原分页版式的 CSS；总体进度见
[Typst HTML 文档](https://typst.app/docs/reference/html/)、
[HTML 导出跟踪 issue #5512](https://github.com/typst/typst/issues/5512) 和
[官方 roadmap](https://typst.app/docs/roadmap/)。因此，下面这些限制属于
Typst 在 HTML / MathML 阶段没有保留相应信息，并非 HTML 元素或知乎编辑器本身
不能表达。本项目将它们标记为等待上游，不使用 Pandoc Lua、TeX `array`、
推断 CSS 等方式猜测并重建已经丢失的信息。

- `table` 会保留表格的语义结构，但 `fill`、`gutter`、`align` 等样式目前被
  上游明确忽略，横线、竖线及其它依赖 CSS 的边框效果也没有完整映射；见已合并的
  [Typst PR #5666](https://github.com/typst/typst/pull/5666)。列宽、`stroke`、
  `inset` 等视觉参数同样不应视为当前 HTML 输出所承诺的能力。
- `grid` 尚无稳定的通用 HTML 表示，可能在导出时被忽略；总体进度由
  [#5512](https://github.com/typst/typst/issues/5512) 跟踪，CSS Grid 映射仍只是
  [开放提案 #721](https://github.com/typst/typst/issues/721)。
- 普通数学矩阵的结构可以随 MathML 转换；但 `math.mat` 的 `augment` 增广线
  当前会被静默忽略，见 [#8253](https://github.com/typst/typst/issues/8253)；
  `mat`、`vec`、`cases` 的行列间距也尚未保留，见
  [#8260](https://github.com/typst/typst/issues/8260)。这些细节同样等待上游实现。

上游 HTML / MathML 输出真正保留这些信息后，本项目再接入相应转换；在此之前，
Typst 发布路径不承诺上述视觉效果与分页输出一致。

### 文章内章节链接

文章可以用源格式原生的 fragment 链接跳到同一篇文章的标题。必须同时设置
该格式的目录请求，目标在最终 HTML 中必须是 `h2` 或 `h3`。

Markdown 用 heading attribute 声明稳定 ID：

```markdown
[跳到结论](#conclusion)

## 结论 {#conclusion}
```

Typst 用 label：

```typst
#link(<conclusion>)[跳到结论]

== 结论 <conclusion>
```

若已经用 `#set heading(numbering: "1.")` 给标题编号，也可以写
`参见 @conclusion`。这是 Typst 的 `ref` 简写，会自动生成“Section 1.1”
一类引用文字和链接；未编号标题应继续使用 `#link`。

### 链接卡片

卡片直接使用 Microformats2 的 [`h-cite`](https://microformats.org/wiki/h-cite)
HTML，必须独占一个段落。一个卡片必须恰好包含一个 `u-url` 和一个非空
`p-name`；`u-url` 必须是带 host 的 HTTP(S) 链接：

```html
<div class="h-cite">
  <a class="u-url p-name" href="https://github.com/">GitHub</a>
</div>
```

Markdown 可以直接嵌入这段 HTML。Typst 可用 `html.elem` 输出同样的结构；博客
也可直接用 `.h-cite`、`.u-url` 和 `.p-name` 编写 CSS。`p-author`、
`p-publication`、`dt-published` 等标准 h-cite 属性可以留给博客使用，发布知乎时
只读取 URL 和标题。链接卡片目前只用于回答和文章；想法发布会明确拒绝它。

### @ 知乎用户

在源稿中把光标移到插入位置，然后运行：

```text
M-x zhihu-insert-user-mention
```

输入搜索词并选择候选后，命令会插入当前源格式的链接。启用
`zhihu-mode` 的 Markdown 源稿还可以直接在正文键入 `@搜索词`，再运行
`M-x completion-at-point` 选择用户；YAML front matter、代码、已有链接和
raw HTML 中不会触发该补全。

## 话题

直接在 `topics` 的某个字符串元素内输入关键词，然后运行：

```text
M-x completion-at-point
```

补全会按当前元素的文字调用知乎话题搜索，保留远端相关性顺序，并在候选中
显示简介和话题 ID；提交后源稿只保留规范的话题名称。两种源格式的写法分别是：

```yaml
topics:
  - "Emacs"
```

```typst
topics: (
  "Emacs",
  "Typst",
),
```

把光标放在引号内即可补全，空查询不会请求网络。这是按关键词的话题自动补全；
知乎网页编辑器根据已上传正文生成的“推荐话题”不是本地补全的一部分。

发布文章时，本地列表是完整事实来源：远端缺少的话题会绑定，多出的话题会
解绑；源稿没有 `topics` 时表示空集合，会清除远端全部话题。

想法也使用 `topics`，但目前直接编辑 metadata：最多十个，发布时随想法整体
提交。回答不支持该字段。源稿只保存话题名称；发布时再从知乎的名称完全匹配
候选取得对应 ID。

## 知乎想法

新想法保留一个空 `thought-id`；发布成功后会把服务端 ID 写入同一字段。已有非空
`thought-id` 的源稿会更新对应想法。

想法正文最多 2000 个字符，可选标题最多 50 个字符；正文和图片不能同时为空。
想法只承诺基本文字、段落、换行和图片，不具备回答或文章的富文本能力。
知乎不会把可选标题保存为独立字段，发布后会把它呈现为正文开头的
`标题 | ` 前缀。

所有富文本效果都不受支持。


想法的图片和回答/文章不同：知乎把它们保存成正文之外的媒体列表。源稿中的
图片会按出现顺序从 HTML 正文抽出，使用 `source=pin` 上传，再写入
`media.medias`。目前最多 18 张，只接受本地路径或 `data:` URL；HTTP(S)
外链图片会在上传前报错。




## 脚注（知乎引用）


知乎引用只能保存一段纯文本和一个可选 URL。因此脚注目前必须是单段，最多
包含一个带 host 的 HTTP(S) 链接；强调、粗体、删除线和行内代码会转成纯文本。
多段、多链接、列表、代码块、图片、公式或 raw 内容会在发布前报错，避免静默
丢失信息；嵌套脚注不在支持范围内。纯文字脚注会生成空的引用 URL。


## 分割线

Typst 0.15 起使用原生语义分割线；在 PDF 等分页输出中渲染为水平线，在
HTML 中输出 `<hr>`：

```typst
#divider()
```

## 元数据示例

**Typst**:

```typst
#metadata((
  question-id: "123456",
)) <zhihu>
```

新文章：

```typst
#metadata((
  article-id: none,
)) <zhihu>

#set document(title: "示例标题")

#outline()
```

想法：

```typst
#metadata((
  thought-id: none,
  topics: ("Emacs",),
)) <zhihu>
```

`banner` 是独立的通用文档 metadata，不属于 `<zhihu>`：

```typst
#metadata("./images/banner.jpg") <banner>
```

**Markdown**

```yaml
---
title: 示例标题
zhihu:
  question-id: "123456"
---
```


```yaml
---
title: 示例文章
banner: "./images/banner.jpg"
toc: true
zhihu:
  article-id:
  topics:
    - "Emacs"
    - "org-mode"
---
```

想法：

```yaml
---
title: 可选的想法标题
zhihu:
  thought-id:
  topics:
    - "Emacs"
---
```

Typst 对应写作 `topics: ("Emacs", "org-mode",)`；单元素 tuple 也保留尾逗号。

每篇源稿必须且只能包含 `question-id`、`article-id` 或 `thought-id` 之一，分别
表示回答、文章或想法。类型按字段是否存在判定，而不是按 ID 是否非空判定；
即使 ID 槽仍为空也算存在。

Markdown 的 `zhihu:` 必须单独占一行，字段写在后续缩进行；不能写成
`zhihu: {...}` 单行形式。顶层 `zhihu` 和其中每个已知字段都只能出现一次。

`article-id` 和 `thought-id` 是仅有的空值例外：首次发布前保留空槽，发布成功后
自动在原位写入服务端 ID。Markdown 写作空值，Typst 写作 `none`。尚未取得的
`answer-id` 仍应整个省略。

需要加入专栏时，在含 `article-id` 的文章 metadata 下额外填写 `column-id`；
`column-id` 不能单独标记文章。发布文章后会检查当前专栏，尚未收录时才发起
收录。Typst 也使用相同字段。启用 `zhihu-mode` 后，可在两种格式的该字段
值槽调用 `completion-at-point`，按专栏名称选择并写入 ID；候选列表在当前
buffer 中懒加载并缓存，revert 后重新读取。

`banner` 与 `title` 一样属于通用文档 metadata：Markdown 使用 YAML 顶层
`banner`，Typst 使用 `#metadata("...") <banner>`；它不写进 `zhihu`
mapping。值是非空的本地图片路径，相对路径以源稿所在目录为基。
知乎文章发布会把它用作题图，字段缺失会清除知乎上的现有封面；回答和想法发布
不会读取或使用它。

目录请求也属于通用文档结构，不属于知乎渠道字段。Markdown 在 YAML front
matter 顶层写 `toc: true`；Typst 使用原生 `#outline()`，也可用
`#outline(depth: 2)` 限制深度。只有文章
发布会读取目录请求，并把它映射为知乎原生文章目录；回答和想法会忽略它。
Typst 中，zhihu.el 只把以 `heading`（包括 `heading.where(...)`）为 target
的 outline 当作这个开关，并在编译 HTML 前通过临时 Typst wrapper 的 show
rule 隐藏该 heading outline，避免与知乎原生目录重复；原稿不会被修改。以
`figure` 等非 heading 元素为 target 的 outline 不会启用文章目录，也会保留
在发布正文中。没有目录请求，或 Markdown 将 `toc` 设为 `false` 时，文章目录
关闭。

以下知乎设置都属于单篇稿件。Typst 和 Markdown 使用表中的 metadata 字段。

| 设置 | Typst / Markdown 字段 | 可用值 | 未填写时 |
| --- | --- | --- | --- |
| 创作声明 | `creation-statement` | `spoiler`、`medical_advice`、`fictional_creation`、`contain_finance`、`ai_creation`；想法不支持 | 无创作声明 |
| 内容来源 | `content-source` | `officialWebsite`（官方网站）、`newsReport`（新闻报道）、`TVMedia`（电视媒体）、`printMedia`（纸质媒体）；想法不支持 | 不标注来源 |
| 话题 | `topics` | 文章至多三个，想法至多十个；回答不支持 | 文章会清空远端话题；想法不提交话题 |
| 转载权限 | `reprint-permission` | `allowed`、`disallowed`、`need_payment`；想法不支持 | 使用转载权限默认值 |
| 评论权限 | `comment-permission` | `all`、`censor`、`followee`、`nobody`；想法还支持 `follower_n_days` | 使用评论权限默认值 |

`content-source` 对应“内容信息来源”渠道；知乎同一面板中的自行拍摄时间和地点
不是这个标量字段的一部分，目前不写入。

上表中的可选发布设置缺失时使用对应默认值；一旦出现，就必须是非空且类型、
取值有效的值。

## Roadmap

- [ ] 等待 [Fletcher 0.6](https://github.com/Jollywatt/typst-fletcher/issues/134)
  正式发布后，基于其届时的最新稳定 API 实现交换图转换。适配器由
  `zhihu.el` 在 Typst HTML 编译阶段植入，源稿无需为每个 `diagram` 手动指定
  `render`；本项目不为 Fletcher 0.5.x 维护兼容层。
  转换只接受知乎 `amscd` 能无损表达的矩形交换图子集，包括网格节点、相邻的
  水平或垂直直线箭头以及简单标签，并生成 TeX `CD` 公式。斜线、曲线、自环、
  多重边、自定义节点形状和其它无法可靠映射的 Fletcher 功能不得静默降级为
  错误的交换图，应明确诊断或继续使用 SVG / 图像路径。

## 致谢

- [`zhihu.nvim`](https://github.com/pxwg/zhihu.nvim)：发布 payload、浏览器
  Cookie 读取和图片上传协议的主要参考实现。
- [`zhihu_obsidian`](https://github.com/zimya/zhihu_obsidian)：知乎用户
  autocomplete、源稿标记和发布 HTML 的参考实现。
- [`zhihu-sign-kt`](https://github.com/zly2006/zhihu-sign-kt)：ZSE v4 签名
  算法的 MIT 许可实现。
