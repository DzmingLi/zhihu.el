# 知乎发布与 Cookie 流程

本文说明 `zhihu.el` 当前实现的完整发布链路：源稿如何变成知乎 HTML，图片
如何上传，回答与文章分别怎样发布，以及浏览器 Cookie、`_xsrf` 和 ZSE
签名各自负责什么。

这里描述的是本项目对知乎网页端非公开接口的实现，不是知乎公开、稳定的
协议。接口、字段和校验方式都可能随知乎网页端更新而变化。

## 一、先建立整体认识

`zhihu.el` 没有自己登录知乎，也不保存账号密码。它把四类事情串在一起：

1. 源稿保存正文和本地同步状态。
2. 用户选择的浏览器提供已经登录的知乎 Cookie。
3. Emacs 负责编译正文、上传图片和组织请求。
4. 知乎与图床分别接收正文、发布状态和图片字节。

```mermaid
flowchart TD
    A[Typst / Markdown 源稿] --> B[读取知乎 metadata]
    B --> C[编译并转换为知乎 HTML]
    C --> D[扫描本地 / data 图片]
    D --> E[每张图片都调用知乎预取]
    E --> F[state=1 跳过 OSS；state=2 上传]
    F --> G[轮询取得 picx URL]
    G --> H{回答还是文章}
    H -->|回答| I[/content/publish]
    H -->|文章| J[创建或 PATCH 文章草稿]
    J --> K[严格同步文章话题]
    K --> L[/content/publish]
    L --> M{配置了 column-id?}
    M -->|是| N[查询当前专栏]
    N -->|尚未收录| O[加入专栏]
    N -->|已经收录| P[结束]
    M -->|否| P
    I --> Q[写回 answer-id]
    O --> P
```

源稿是本项目的本地事实来源，但远端状态并不只靠源稿猜测。例如专栏收录
状态会在发布后查询文章接口，而不是写一个 `column-added` 标记。

## 二、Cookie、XSRF 和 ZSE 是三件不同的事

这三者经常同时出现在浏览器请求里，但用途不同：

| 机制 | 当前实现中的数据来源 | 主要用途 | 发送形式 |
| --- | --- | --- | --- |
| 登录 Cookie | 所选浏览器的 Cookie 存储 | 让知乎把请求识别为已登录用户 | `Cookie: k=v; ...` |
| XSRF | 知乎响应下发的 `_xsrf` session cookie | 保护部分修改接口 | Cookie 中的 `_xsrf`，以及同值的 `x-xsrftoken` |
| ZSE | 请求 URL、`d_c0` 和可选 JSON body | 知乎部分网页 API 的请求签名 | `x-zse-93`、`x-zse-96` |

它们不是彼此替代的关系：

- 有 Cookie 不代表请求一定需要 XSRF。
- 有 `_xsrf` 不代表请求要带 ZSE。
- ZSE 使用 Cookie 里的 `d_c0` 参与计算，但 ZSE 本身不是登录凭据。
- 当前代码逐端点决定是否使用 XSRF 和 ZSE，不能简单概括成“所有 POST”
  或“所有 JSON 请求”都一样。

## 三、浏览器 Cookie 的完整读取逻辑

### 3.1 选择浏览器和 profile

`zhihu-cookie-browser` 是 Customize 选项，默认值为 `firefox`；还可选择
`chromium`、`chrome` 或 `edge`。
`zhihu-cookie-profile-directory` 必须显式指向所选浏览器的实际 profile
根目录。本包不读取 profile 列表、不扫描标准目录，也不按修改时间猜测账号。

可以直接设置：

```elisp
(setq zhihu-cookie-browser 'chromium
      zhihu-cookie-profile-directory "~/.config/chromium/Default")
```

也可以依次运行：

```text
M-x customize-option RET zhihu-cookie-browser RET
M-x customize-option RET zhihu-cookie-profile-directory RET
```

profile 根目录与 Cookie 文件之间只有以下固定映射：

| 浏览器 | profile 根目录示例或语义 | 固定相对路径 |
| --- | --- | --- |
| Firefox | `~/.mozilla/firefox/xxxxxxxx.default-release/` | `cookies.sqlite` |
| Chromium | `~/.config/chromium/Default/` | `Network/Cookies` |
| Google Chrome | `~/.config/google-chrome/Default/` | `Network/Cookies` |
| Microsoft Edge | `~/.config/microsoft-edge/Default/` | `Network/Cookies` |

Chromium 系必须配置 `Default`、`Profile 1` 等实际 profile，不能只配置包含
多个 profile 的 user-data 根目录。固定路径不存在或不可读时会直接报错，不会
尝试其它 profile 或旧的 `Cookies` 位置。

旧版的 `zhihu-cookie-database-file` 填写的是 Cookie 文件，现已移除。手工
迁移时，Firefox 将 `.../cookies.sqlite` 改为其父目录；Chromium 系将
`.../<profile>/Network/Cookies` 改为 `.../<profile>/`。代码不会自动转换
旧值，也不会把旧配置作为 fallback。

### 3.2 两种 Cookie 存储格式

Firefox backend 查询默认容器（`originAttributes = ""`）中 `.zhihu.com`
或请求精确 host 的记录；过期时间兼容数据库中常见的秒和毫秒表示。如果只在
Firefox Container Tabs 等隔离容器里登录知乎，这些 Cookie 不会被读取。

Chromium、Chrome 和 Edge backend 读取 `cookies` 表，并排除非空
`top_frame_site_key` 的分区 Cookie。`expires_utc = 0` 作为 session Cookie
保留，其它过期时间从 Chromium 的 1601 微秒时间转换为 Unix 时间。

Chromium 系 Cookie 的 `encrypted_value` 由 Emacs 直接解密，不经过 Python：

- GNU/Linux `v10` 使用 Chromium 的固定 basic-store 密钥；`v11` 从桌面
  Secret Service 或 KWallet 取得 Safe Storage secret；
- `v11` 按 Chromium 参数执行 PBKDF2-HMAC-SHA1；`v10`/`v11` 都使用
  AES-128-CBC 和严格的 PKCS#7 去填充；
- 数据库 `meta.version >= 24` 时，还会核验并移除明文开头的
  `SHA256(host_key)`；不匹配的值不会被当作 Cookie 发送；
- 未实现的密文版本会明确失败，不会退化为明文。

### 3.3 按完整 URL 匹配

统一入口接收完整请求 URL，而不是直接遍历数据库结果。读取出的记录还要满足：

- host-only Cookie 只匹配精确 host，带前导点的 domain Cookie 按域名标签
  边界匹配；
- Cookie Path 按 RFC path-match 规则匹配 URL path，query 不参与；
- `Secure` Cookie 只用于 HTTPS；
- 持久 Cookie 尚未过期，session Cookie 仍有效。

更长 Path 的记录排在前面；Path 相同时按创建时间排序。同名但 Path 不同的
Cookie 可以同时存在。

代码不会只挑某几个“已知登录 Cookie”。匹配 URL 的整组 Cookie 都会被拼成：

```http
Cookie: name1=value1; name2=value2; ...
```

其中只有两个名字会被代码额外解释：

- `d_c0`：用来生成 ZSE 签名；
- `_xsrf`：在一次文章发布链中由独立 state 管理；相关修改接口还会用它生成
  XSRF 请求头。

`z_c0` 通常是知乎登录状态中的重要 Cookie，但代码没有硬编码“必须存在
`z_c0` 才能发请求”。找不到可读的 Cookie 存储、读取失败或密文无法安全
解密时，当前操作会报错；存储可读、但其中没有有效登录 Cookie 时，
请求仍会发出，服务端通常会把它当成未登录请求。

### 3.4 浏览器正在运行时怎样读 Cookie 存储

Firefox 和 Chromium 系先创建内存 SQLite 主库，再用
`file:...?mode=ro&cache=private` 只读
`ATTACH` 原数据库，让 SQLite 从主库和 WAL 得到一致视图，查询后立即关闭。

只读 `ATTACH` 或查询失败时，代码把数据库与现存的 `-wal` 复制到临时目录后
查询副本；`-shm` 是可重建的协调状态，不复制。无论成功还是失败，连接和
临时目录都会清理。只有原库与临时副本都失败时操作才停止；读取错误不会降级
成空 Cookie 后继续请求。

### 3.5 每个请求只读一次 Cookie

每个启用知乎 Cookie 的 HTTP 请求，在发出前把完整 URL 交给所选浏览器
backend 读取一次，并把匹配结果固定为本次请求的 `Cookie` header。随后：

1. 如果调用方传入文章发布操作的 XSRF 状态，把其中的 `_xsrf` 加入该
   header；
2. 如果该端点需要 ZSE，从该 header 取出 `d_c0`；
3. 用这个 `d_c0` 生成 ZSE；
4. 发出已经固定的 `Cookie` header。

这样 ZSE 使用的 `d_c0` 与网络请求携带的 `d_c0` 不会来自两次不同的数据库
读取。

所以一次普通知乎请求读取一次 Cookie 存储；第一次 XSRF 修改操作总共会读两次，
是因为它先发一个首页 bootstrap 请求、再发真正的修改请求，这仍然是两个
请求各读一次。同一次文章发布中后续已有 `_xsrf` 时，修改请求本身只读
一次。新的一次文章发布会创建空状态，需要重新 bootstrap。

图片 OSS 的目标域名是 `zhihu-pics-upload.zhimg.com`，不是
`zhihu.com`。以该地址为初始目标的请求不会读取或携带知乎 Cookie。

## 四、`_xsrf` session cookie 的生命周期

### 4.1 为什么不能只读 SQLite

当前实测中，首页下发的 `_xsrf` 没有 `Expires` 或 `Max-Age`，因而是
session Cookie。Firefox 不会把它写入 `cookies.sqlite`；其它浏览器持久存储
即使存在同名旧记录，也不能代替本次发布过程中由服务器下发和轮换的值。

所以代码把持久 Cookie 与 `_xsrf` 分开处理：

- 登录 Cookie 每个知乎请求都从所选浏览器存储现读；
- `_xsrf` 从知乎响应取得，只保存在本次文章发布的 `zhihu--xsrf-state`
  对象中；
- `_xsrf` 不写入源稿，也不写回浏览器存储。

### 4.2 第一次需要 XSRF 时

```mermaid
sequenceDiagram
    participant P as 发布流程
    participant E as zhihu.el HTTP 层
    participant B as 浏览器 Cookie 存储
    participant Z as www.zhihu.com

    P->>P: 创建空 XSRF state
    P->>E: 确保 state 中有 token
    alt state 已有非空 _xsrf
        E-->>P: 返回已有 token
    else state 中尚无 _xsrf
        E->>B: 按完整首页 URL 读取一次 Cookie
        B-->>E: 返回匹配的 Cookie
        E->>Z: GET /，携带这些 Cookie
        Z-->>E: HTTP 200 + Set-Cookie: _xsrf=TOKEN
        E->>P: 把 TOKEN 写入本次操作的 state
        E-->>P: 返回 TOKEN
    end
    P->>E: 发出真正的修改请求
    E->>B: 按完整请求 URL 读取一次 Cookie
    B-->>E: 返回匹配的 Cookie
    E->>E: 加入 _xsrf=TOKEN
    E->>Z: Cookie: ...; _xsrf=TOKEN<br/>x-xsrftoken: TOKEN
```

`GET https://www.zhihu.com/` 必须返回 HTTP 200，而且响应必须下发非空
`_xsrf`，否则修改请求不会继续。

### 4.3 刷新、覆盖和清空

文章发布把 XSRF state 传给 HTTP 层后，相应的知乎响应会扫描
`Set-Cookie`：

- 收到非空 `_xsrf=TOKEN` 时，覆盖 state 中的旧值；
- 收到空的 `_xsrf=` 时，清除 state；
- 没有 `_xsrf` 时，不改变 state。

构造下一次带 XSRF state 的文章请求时，代码先移除 SQLite 结果中可能存在
的旧 `_xsrf`，再加入 state 中的值。相关修改接口的 Cookie 与
`x-xsrftoken` 因此使用同一个 token；文章发布和专栏查询也会携带并刷新
state 中的 Cookie，但不附加 XSRF 请求头。

这份状态的生命周期仅是一次文章发布。下一次调用 `zhihu-publish` 会创建新
状态，不会复用上次发布或另一个账号留下的 token。图片上传、问题标题查询和
回答发布不接收 XSRF state，也不会为了以后可能的修改请求保存 `_xsrf`。

### 4.4 curl 的 Cookie jar 没有参与

底层通过 `plz.el` 调用 curl。每次请求都把 `--disable` 放在 curl 参数首位，
因此不会读取 `~/.curlrc`；代码也没有使用 `--cookie` 或 `--cookie-jar`。
所以 curl 不会从用户配置补充 Cookie，也不会把响应中的 `Set-Cookie` 持久化
到 Cookie jar。

本项目只手动读取所选浏览器的 Cookie。文章发布期间，HTTP 层只把响应中的
`_xsrf` 更新到该次操作的 state；其它 `Set-Cookie` 不会被保存。下一次请求
仍以浏览器 Cookie 存储的现有内容和调用方传入的 XSRF state 为准。

## 五、ZSE 签名

默认 JSON 请求会尝试生成 ZSE，但只有同时满足下面两个条件才真正附加：

1. 初始请求 URL 属于 `zhihu.com` 或其子域；
2. 本次请求固定的 Cookie header 中存在非空 `d_c0`。

没有 `d_c0` 时，请求仍会发出，只是不带 ZSE。

当前使用：

```text
x-zse-93 = 101_3_3.0

source = x-zse-93
         + "+"
         + 原样的 path 和 query
         + "+"
         + d_c0
         [ + "+" + 实际发送的 JSON 字符串 ]

x-zse-96 = "2.0_" + ZSE-v4(MD5(source))
```

这里有两个一致性约束：

- JSON body 只序列化一次；签名和实际发送使用同一个字符串；
- ZSE 读取的 `d_c0` 与实际 `Cookie` header 来自同一次 SQLite 读取。

并非所有 JSON 请求都应该签名。创建文章草稿、PATCH 草稿、正式发布和加入
专栏都会在调用点明确关闭 ZSE。尤其当前实现验证到
`/api/v4/content/publish` 混入 ZSE 可能得到 HTTP 200 空响应，却没有完成
发布，所以该端点必须单独处理。

## 六、远端端点对照表

下表的“XSRF”专指代码是否附加 `x-xsrftoken`、`Origin` 和 `Referer`。
表中的知乎 Cookie 可能包含文章发布 state 显式加入的 `_xsrf`。

| 方法与端点 | 用途 | 知乎 Cookie | XSRF | ZSE |
| --- | --- | --- | --- | --- |
| `GET https://www.zhihu.com/` | 缺少 `_xsrf` 时 bootstrap | 是 | 否 | 否 |
| `GET https://www.zhihu.com/api/v4/questions/{qid}` | 新建回答源稿时读取问题标题 | 是 | 否 | 默认开启 |
| `POST https://api.zhihu.com/images` | 按图片 MD5 预取上传信息 | 是 | 否 | 默认开启 |
| `PUT https://zhihu-pics-upload.zhimg.com/{object_key}` | 上传原始图片字节到 OSS | 否 | 否 | 否，使用 OSS STS/HMAC |
| `GET https://api.zhihu.com/images/{image_id}` | 轮询图片处理结果 | 是 | 否 | 否 |
| `POST https://www.zhihu.com/api/v4/columns/request` | 申请创建普通知乎专栏 | 是 | 是 | 默认开启 |
| `POST https://zhuanlan.zhihu.com/api/articles/drafts` | 创建文章草稿 | 是 | 是 | 明确关闭 |
| `PATCH https://zhuanlan.zhihu.com/api/articles/{aid}/draft` | 更新文章草稿正文和标题 | 是 | 是 | 明确关闭 |
| `GET https://zhuanlan.zhihu.com/api/autocomplete/topics?...` | 搜索文章话题 | 否 | 否 | 否 |
| `GET https://zhuanlan.zhihu.com/api/articles/{aid}/draft` | 读取草稿的现有话题 | 是 | 否 | 明确关闭 |
| `POST https://zhuanlan.zhihu.com/api/articles/{aid}/topics` | 绑定一个文章话题 | 是 | 是 | 明确关闭 |
| `DELETE https://zhuanlan.zhihu.com/api/articles/{aid}/topics/{tid}` | 解绑一个文章话题 | 是 | 是 | 明确关闭 |
| `POST https://www.zhihu.com/api/v4/content/publish` | 创建/更新回答，或正式发布文章 | 是 | 否 | 明确关闭 |
| `GET https://www.zhihu.com/api/v4/articles/{aid}` | 查询文章当前所在专栏 | 是 | 否 | 默认开启 |
| `POST https://www.zhihu.com/api/v4/columns/{cid}/items` | 把已发布文章加入专栏 | 是 | 是 | 明确关闭 |

这些 XSRF 修改接口使用的来源页面如下：

| 接口 | `Referer` | `Origin` |
| --- | --- | --- |
| 申请创建专栏 | `https://www.zhihu.com/column/request` | `https://www.zhihu.com` |
| 创建文章草稿 | `https://zhuanlan.zhihu.com/write` | `https://zhuanlan.zhihu.com` |
| PATCH 文章草稿 | `https://zhuanlan.zhihu.com/p/{aid}/edit` | `https://zhuanlan.zhihu.com` |
| 绑定或解绑文章话题 | `https://zhuanlan.zhihu.com/p/{aid}/edit` | `https://zhuanlan.zhihu.com` |
| 加入专栏 | `https://zhuanlan.zhihu.com/p/{aid}/edit` | `https://zhuanlan.zhihu.com` |

通用请求头还包括模拟浏览器的 `User-Agent`、`Accept-Language` 和
`x-requested-with: fetch`；JSON 请求加
`Content-Type: application/json`。当前 HTTP 请求是同步的，单次超时为
30 秒。

这些是当前实现实际发送的 header，不代表已经证明每个字段都是服务端协议
上不可缺少的字段。

### 6.1 新建专栏申请

`M-x zhihu-new-column` 调用
`POST https://www.zhihu.com/api/v4/columns/request`，请求体固定为：

```text
{
  "title": "...",
  "intro": "...",
  "intro_type": "plain"
}
```

专栏名称去掉两侧空白后必须非空、最多 20 个 Unicode code point，并且不能
包含单行输入框无法表达的换行或控制字符；简介允许空字符串和换行，最多 1000
个 code point，但非空输入不能只由空白组成。两者都按当前网页表单规则拒绝
U+1F300–U+1F64F。网页接口的封面可选，本命令不提示封面，请求中也不发送
`images`。

命令为这次申请建立独立的 XSRF state，在发送 POST 前取得 `_xsrf`，再同时
发送同值的 `x-xsrftoken`、上表中的 `Origin` 与 `Referer`。该 www.zhihu.com
端点保留 JSON 请求默认的 ZSE 签名。

HTTP 2xx 后仍要检查顶层 `payload` 中明确存在的 `manualCensor`（兼容
`manual_censor`）：true 表示申请已提交人工审核，命令返回 `pending`；false
时还必须取得非空、无控制字符的 `payload.id`，命令才返回该专栏 ID。字段
缺失或畸形不能被当成创建成功。

新建专栏是非幂等操作，不会自动重试。HTTP 400–499（408 除外）是服务端明确
拒绝；401/403 会额外提示检查登录和验证。POST 的传输中断、HTTP 408/5xx、
其它异常状态或畸形 2xx 响应都通过
`zhihu-create-result-unknown` 提示“可能已经提交”，用户应先到知乎检查，
不要直接重试。发生在 POST 之前的 XSRF bootstrap 失败可以确定尚未提交，
因此保留原始错误。

## 七、源稿怎样变成知乎正文

### 7.1 metadata 与标题

两种源稿最终都会被读成统一状态：

- `question-id`、`answer-id`；
- `article-id`、`column-id`；
- `thought-id`；
- 文章或想法话题 `topics`；
- 按篇发布设置 `creation-statement`、`reprint-permission` 和
  `comment-permission`；
- 文档标题、通用题图源路径 `banner` 和通用目录请求。

回答字段与文章字段不能混用。通用 `banner` 不参与稿件类型判定，因此也可以
存在于回答源稿中；`toc` 同样不参与类型判定。知乎回答与想法发布流程不会
使用这两个字段。

所有知乎 metadata 字段都严格区分“缺失”和“显式空值”：只有字段缺失才采用
默认语义；字段一旦出现，就必须是非空且类型、取值有效的值。Typst 的 `none`、
YAML 的 `null` 和空字符串都是格式错误。布尔值 `false` 是有效值，不会与
null 混为一谈。

`question-id`、`article-id` 与 `thought-id` 是三个互斥的类型判据，必须且只能
出现一个。`question-id` 不能为空；新文章和新想法则分别保留空的
`article-id` 或 `thought-id` 槽，首次发布成功后写回服务端 ID。回答的
`answer-id` 不参与类型判定，只在已有回答或首次发布成功后出现。

标题、题图和目录设置属于文档本身，不属于 `<zhihu>` 发布状态：

| 格式 | 文档标题 | 通用题图 | 通用目录设置 | 知乎同步状态 |
| --- | --- | --- | --- | --- |
| Typst | `#set document(title: ...)` | `#metadata("path") <banner>` | `#outline()` | `#metadata((...)) <zhihu>` |
| Markdown | YAML 顶层 `title` | YAML 顶层 `banner` | YAML 顶层 `toc: true` | YAML 的 `zhihu` mapping |

三个按篇设置的语义如下：

| 设置 | 可用值 | 字段缺失时 |
| --- | --- | --- |
| `creation-statement` | API 类型 `spoiler`、`medical_advice`、`fictional_creation`、`contain_finance`、`ai_creation` | 无创作声明，即 `disclaimer_status = close`、`disclaimer_type = none` |
| `reprint-permission` | API 值 `allowed`、`disallowed`、`need_payment` | `zhihu-publish-default-reprint-permission` |
| `comment-permission` | API 值 `all`、`censor`、`followee`、`nobody` | `zhihu-publish-default-comment-permission` |

`banner` 是通用文档字段，值必须是非空字符串。知乎仅在发布文章时把它解释为
本地图片路径；相对路径以源稿目录为基准，HTTP(S)、协议相对 URL 和 data URL
都会报错。它采用声明式语义：字段存在时设置或替换文章封面，字段缺失时通过
`titleImage = ""` 明确清除知乎上的现有封面。旧的 Markdown 顶层 `cover` 和
Typst `<cover>` 不会被读取；`zhihu.banner` 和 Typst `<zhihu>` dictionary
内的 `banner` 也完全忽略。
不存在兼容或回退优先级。

目录请求属于通用文档结构。Markdown 使用 YAML 顶层布尔值 `toc: true`；
Typst 使用原生 `#outline()`，也可写成 `#outline(depth: 2)`。只有文章发布流程
读取目录请求，并映射为知乎原生文章
目录；回答和想法会忽略它。Typst 中，只有以 `heading`（包括
`heading.where(...)`）为 target 的 outline 才作为文章目录开关；编译 HTML
前，临时 Typst wrapper 会通过 show rule 隐藏该 heading outline，以免与知乎
原生目录重复，原稿本身不会修改。以 `figure` 等非 heading 元素为 target 的
outline 不会启用文章目录，并会保留在发布正文中。没有目录请求，或 Markdown
的 `toc` 为 `false` 时关闭目录。

`topics` 允许用于文章和想法，值是互不重复的话题名称 sequence；
文章至多三个，想法至多十个。Markdown 使用 YAML sequence，Typst 使用
string tuple。文章字段缺失表示本地空集合，
发布时会解绑远端全部话题；想法字段缺失时不发送话题 payload。
显式空 sequence 仍是格式错误，清空时应删除整个字段。

Typst 和 Markdown 使用上表中的原字段名。
`creation-statement` 没有全局默认值，填写时直接使用 API 类型，不经过一层
包内名称映射。

回答发布不会向知乎发送任何 title 字段。`zhihu-new-answer` 读取问题标题，只
是为了生成一份初始本地文档标题；之后可以把源稿标题改成任意文字，它不会
覆盖知乎问题标题，也不会被当成“回答标题”。

文章必须有标题：

- Typst 从编译后 HTML 的 `<title>` 读取，也就是由
  `#set document(title: ...)` 统一管理；
- Markdown 从 YAML 顶层 `title` 读取；

新文章源稿必须填写文档标题；发布时标题为空会明确报错。

文章标题会在创建和 PATCH 文章草稿时发送。最终
`/content/publish` 请求本身不再携带标题或正文。

### 7.2 两种格式的编译路径

Typst：

1. 自动推断 `--root`。代码收集 `/...` 形式的全部绝对 import/include，
   向上查找能同时解析所有路径的最近祖先；找不到时直接报错。没有绝对引用
   时使用源稿目录。
2. 执行 `typst compile --features=html ...`，得到包含
   `<head>` 的完整 HTML。
3. 同一份 HTML 的 `<title>` 用作文章标题。
4. 再用 Pandoc 把 HTML 规范化成 HTML5 fragment，不改变 Typst 已经生成的
   heading 层级。Typst 的 HTML 导出已为文档标题保留 `h1`，一级正文
   heading 从 `h2` 开始。

Markdown：

1. 先剥掉 YAML frontmatter；
2. 用 Pandoc 从显式启用 `alerts` 的 `gfm` 转为 HTML5；默认把正文 heading
   整体下移一级。

每次 Pandoc 转换都会加载由包内代码生成的临时 Lua filter，用于引用节点以及
Markdown alerts。临时 filter 在转换完成或报错后都会删除，不要求安装额外的
Lua 文件。

链接卡片直接使用 Microformats2 `h-cite` HTML：根节点带 `h-cite` class，
其中恰好包含一个 `u-url` 和一个非空 `p-name`。`u-url` 必须是带 host 的
HTTP(S) `<a>`。卡片必须是文档顶层的独立内容；转换器把它改为知乎原生 block
`link-card`。Markdown 可嵌入 raw HTML，Typst 可用 `html.elem` 输出同一结构。

分割线不需要 Lua filter。Markdown 正文的 `---` 由 Pandoc 转成 `<hr>`。
Typst 0.15 起使用原生 `divider()`：在
PDF 等分页输出中渲染为水平线，在 HTML 中直接输出 `<hr>`。

Markdown 的偏移由 Customize 选项
`zhihu-enable-markdown-heading-level-shift` 控制；设为 `nil` 时保留源稿的
自然标题层级。Typst 的 HTML 导出已经区分文档标题与正文 heading，不读取
该选项。

之后两种格式都进入同一套知乎 HTML 方言转换：

- Pandoc 的数学 `.math` span 变成知乎 equation `<img>`；
- 代码块归一成带 `lang` 的 `<pre>`；
- 删除 `style` 属性；
- 为 table 增加知乎编辑器需要的 `data-draft-*` 属性；
- 把严格校验后的 `h-cite` 转成 block `link-card` 属性；

默认设置下，两种源稿都把各自的一级语法（Typst `=`、Markdown `#`）作为正文
顶层章节，发布后成为 `h2`；下一级成为 `h3`。关闭 Markdown
标题偏移后，只有 Markdown 分别保留为 `h1`、`h2`。标题层级在进入通用知乎
HTML 方言转换前已经确定，通用转换不再单独改写 `h1`。

## 八、图片上传

HTML 转换完成后，代码扫描所有 `<img>`：

- `data:` URL 会解码为图片字节；
- 本地路径和相对路径按源稿所在目录读取；
- `http://`、`https://` 和 `//` 开头的协议相对外链保持原样；
- `<img>` 缺少非空 `src` 时立即报错；
- 需要上传的 `data:` URL 无法解析，或本地路径不可读时，立即报错，不会
  静默保留原地址继续发布。

每次发布都会对每一张需要上传的图片计算原始字节的 MD5，并完整执行以下流程：

1. 向 `POST https://api.zhihu.com/images` 发送 MD5 和
   `source=answer|article`。
2. `upload_file.state = 1`：服务端已经有相同 hash，不做 OSS PUT。
3. `upload_file.state = 2`：使用返回的临时 STS 信息，把原始图片字节 PUT
   到 OSS。
4. 无论服务端返回 `state = 1` 还是 `state = 2`，随后都最多轮询图片状态
   10 次，每次间隔 0.5 秒；得到
   `status=success` 且存在非空 `src` 后返回。`status=failed` 或
   `status=error` 立即报错；只有 `status=pending` 会继续轮询，第 10 次
   仍未成功则报超时。非 2xx、非法 JSON、缺少有效 `status`、未知状态，
   以及成功响应缺少 `src` 也会立即报错。
5. 把 HTML 中的 `src` 替换为轮询得到的远端 URL。

OSS 请求使用 prefetch 返回的临时 `access_id`、`access_key` 和
`access_token`。签名是 HMAC-SHA1，`Authorization` 形如：

```http
Authorization: OSS access_id:signature
```

这不是知乎登录 Cookie，也不会把 `z_c0`、`d_c0` 或 `_xsrf` 发给 OSS。

文章的通用 `banner` 不进入正文 HTML，但复用完全相同的本地文件读取、MD5、
`source=article` 预取、按需上传和轮询流程。封面上传返回的 URL 会作为
article draft PATCH 的 `titleImage`；
`isTitleImageFullScreen` 固定为 `false`。没有 `banner` 时仍会发送
`titleImage = ""`，因此可以通过删除通用题图字段清除远端封面。

## 九、回答发布

回答流程是：

1. 从 metadata 取得必需的 `question-id` 和可选的 `answer-id`。
2. 编译正文并上传、重写图片。
3. 直接调用 `/api/v4/content/publish`。
4. 严格确认服务端返回的发布 ID。
5. 新回答把返回的 `answer-id` 写回源稿。

回答没有文章草稿 API。
已有回答发布成功后不需要写回 metadata；只有新回答需要在成功后追加服务端
返回的 `answer-id`。

发布 payload 的关键差异：

- `action = "answer"`；
- `data.hybrid.html` 直接携带回答 HTML；
- `extra_info.question_id` 携带问题 ID；
- 更新已有回答时使用 `draft.contentId = answer-id`；
- 创建新回答时必须完全省略 `draft.contentId`；
- 新回答的 `draft.isPublished = false`，更新已有回答时为 `true`。

这里的 `isPublished` 表达请求所针对对象的既有状态，不应直译成“这一次是否
执行发布”。新建内容仍然是在发布，只是该字段为 `false`。

回答流程不会使用或发送解析到的源稿标题。Markdown frontmatter 会在正文
转换前剥掉；Typst 的 document title 位于 HTML head。二者都不会成为知乎
API 的回答 title 字段。

如果服务端实际创建了新回答，但客户端没有收到可验证的成功响应，源稿中仍
没有 `answer-id`。当前代码没有足够的远端状态来自动找回这次创建，直接重试
存在再次创建回答的可能。

## 十、文章发布

文章不是一次请求完成的。完整顺序是：

```text
编译与图片处理
  → 新文章才创建远端草稿
  → 所有文章都 PATCH 草稿
  → 严格同步文章话题
  → /content/publish 正式发布
  → 可选：查询专栏
  → 可选：加入专栏
```

具体过程：

1. 取得文章标题、`article-id`、可选 `column-id`、可选 `banner` 和按篇发布
   设置。
2. 编译、转换正文，处理正文图片，并在配置时上传封面。
3. 如果没有 `article-id`，调用文章草稿创建接口。
4. 一拿到草稿 ID，就立即只写回 `article-id`。
5. 无论新旧文章，都 PATCH 一次远端草稿，保存最新标题、正文和封面状态。
6. 读取远端草稿话题，严格同步为本地 `topics` 集合。
7. 调用 `/content/publish` 正式发布文章。
8. 如果配置了 `column-id`，再独立处理专栏收录。

创建文章草稿发送标题、正文和部分编辑设置；PATCH 发送标题、正文、目录开关、
`titleImage`、`isTitleImageFullScreen = false` 和编辑设置。配置 `banner` 时
`titleImage` 是上传后的 URL，缺失时是用于清除封面的空字符串。正式发布
payload 的关键差异是：

- `action = "article"`；
- `draft.id = article-id`，而不是回答使用的 `contentId`；
- 没有 `question_id`；
- 没有 `hybrid.html`，因为正文已经通过文章草稿接口保存；
- 本轮从空 `article-id` 开始、刚创建远端草稿时使用
  `draft.isPublished = false`；
- 本轮开始时已有 `article-id` 则使用 `draft.isPublished = true`。

### 10.1 `article-id` 是唯一的本地状态

源稿没有额外的文章草稿标记。发布分支只看 `article-id`：

```text
article-id 为空   → 创建远端草稿
article-id 非空 → 复用该 ID，PATCH 并发布
```

创建接口明确返回 ID 后，代码会在 PATCH、话题同步和正式发布之前立即
把它写回。因此这些后续步骤中断时，下次运行会复用同一 ID，不会再创建一篇文章。
如果服务端已经创建草稿、但创建响应本身在客户端拿到 ID 前丢失，任何本地事后
标记都无法找回那个 ID；当前实现不能自动消除这个窗口。

### 10.2 文章话题采用严格集合同步

启用 `zhihu-mode` 后，在 Markdown `zhihu.topics` 的 sequence item，或 Typst `<zhihu>` dictionary
中 `topics` tuple 的字符串元素内运行 `completion-at-point`。补全表按输入
查询公开 autocomplete 端点，候选保留知乎的相关性顺序并显示简介与话题 ID；
最终只把话题名写入当前元素，不改动外层引号、逗号和注释。空查询不请求网络，
同一 buffer 内会按查询词缓存结果。

这个功能是关键词 autocomplete，不是知乎网页编辑器在正文上传后生成的
内容推荐。本包不会为了话题补全而创建草稿或上传当前正文。

正文 PATCH 成功后，代码读取 `/api/articles/{aid}/draft` 顶层的 `topics`
array。同步流程是：

1. 读取远端已有话题；
2. 对本地存在而远端缺少的名称调用 autocomplete，并取得名称完全相等的
   候选；找不到时停止；
3. 在任何解绑之前解析完全部待绑定候选；
4. 先逐个 DELETE 远端多余话题，为三个话题的上限腾出位置；
5. 再逐个 POST 缺少话题的完整 autocomplete object；
6. 发生修改后重新 GET 草稿，同时按 ID 集合和名称集合校验远端与本地完全
   相等。

话题顺序不参与比较。任何解析、解绑、绑定或最终校验失败都会发生在正式发布
之前；源稿和 `article-id` 保持不变，下次发布重新读取远端现状并继续收敛。
补全只修改当前 buffer，不会立即修改知乎。缺失 `topics` 仍明确表示
目标空集合。

### 10.3 正式发布与加入专栏是两件事

文章正式发布成功后，才处理 `column-id`：

1. `GET /api/v4/articles/{aid}`；
2. 比较响应顶层 `column.id` 与源稿中的 `column-id`；
3. 相同：报告“已收录”，不再发写请求；
4. 不同或没有 column：调用
   `POST /api/v4/columns/{cid}/items`。

GET 返回相同 `column.id` 时，当前的“已收录状态”来自知乎文章接口，而
不是靠重复 POST 的 400，也不是靠本地 `column-added`。如果本轮确实执行了
收录 POST，代码要求 HTTP 2xx。POST 后不会紧接着再 GET；这次收录的远端
可读状态会在下次发布的 GET 中再次确认。

`column-id` 表示用户期望文章属于哪个专栏，成功收录后也会保留。如果专栏
查询或收录失败：

- 文章已经正式发布；
- `article-id` 和 `column-id` 都会保留；
- 下次发布会更新文章，然后再次查询并尝试完成专栏步骤。

本项目不会因为专栏步骤失败而回滚已经发布的文章。

## 十一、`/content/publish` 的公共 payload 与成功确认

回答和文章共用一套公共骨架，其中包括：

- `action`；
- `data.extra_info`；
- `data.draft`；
- 转载、创作声明、目录、评论和赞赏等设置。

这些字段的来源并不相同：

- 创作声明来自每篇稿件的 `creation-statement`。未填写时关闭声明并发送
  `disclaimer_type = none`；填写时打开声明并直接发送该 API 类型；
- 目录来自源稿的通用目录请求，未填写时关闭；回答和想法忽略该请求；
- 转载和评论可以由稿件的 `reprint-permission`、`comment-permission`
  覆盖；未填写时分别使用两个独立的 typed `defcustom`：
  `zhihu-publish-default-reprint-permission` 和
  `zhihu-publish-default-comment-permission`；
- `zhihu-publish-defaults` 目前只保留 `draft_type`、`delta_time` 和
  `can_reward`；
- “感谢邀请”没有配置入口，固定为关闭。

同一个设置快照会同时写入外层设置与
`extra_info.pc_business_params`，避免两处不一致；`pc_business_params` 会
先单独 JSON 序列化，再作为字符串放进外层 JSON，并不是普通的嵌套 object。

服务端的 `data.result` 反而可能是 JSON 字符串，也可能已经是 object，代码
会兼容两种响应形式。

“HTTP 请求成功”还不足以确认内容已经发布。当前校验要求：

1. HTTP 状态必须是 2xx；
2. 响应必须能解析为 JSON；
3. `code = 0`；
4. 必须存在 `data.result.publish.id`。

响应中的 `message` 可用于展示错误信息，但不参与成功判定，不要求它精确等于
`"success"`。

只有全部满足，才会把新回答 ID 视为可写回结果；新文章的 ID 已在草稿创建
接口明确返回时提前写回。HTTP 403 且
响应包含 `need_login: true` 时，会明确提示浏览器登录状态不可用。

## 十二、写回时机与失败恢复

| 失败位置 | 源稿中已经保存的状态 | 下次重试 |
| --- | --- | --- |
| 新文章草稿创建前失败 | 没有 `article-id` | 重新走创建 |
| 远端草稿已创建，但客户端没拿到 ID | 没有新的 `article-id` | 无法识别远端草稿，重试可能再创建一个 |
| 新文章草稿创建后失败 | `article-id` | 复用该 ID，继续 PATCH/发布 |
| 文章话题同步失败 | 本地 `topics`、已有 `article-id` | 重新读取远端话题，继续增删并校验 |
| 文章已正式发布，但响应无法严格确认 | 已有 `article-id` | 复用同一文章 ID 再发布，不会新建文章 |
| 文章正式发布已确认，专栏查询或收录失败 | `article-id`、`column-id` | 更新已发布文章，再重试专栏步骤 |
| 新回答服务端成功但响应无法确认 | 没有新的 `answer-id` | 无法自动找回，重试可能再次创建 |
| 新建专栏申请的结果无法确认 | 无本地状态 | 先到知乎检查已有专栏或待审申请，不要直接重试 |

元数据写回是有针对性的：

- Typst 只重写标准的 `#metadata((...)) <zhihu>` 块，不接管
  `#set document(title: ...)` 或 `#metadata("...") <banner>`；
- Markdown 只替换 YAML frontmatter 中的 `zhihu` mapping。

因此服务端 ID 的 checkpoint 不会新增、移动、改写或删除通用题图
字段；即使运行时 plist 中的 `:banner` 为 nil，知乎状态 writer 也不会清除
源稿题图。

没有任何需要持久化的知乎状态时，相应的 Typst metadata 块或 Markdown
`zhihu` mapping 会完全缺席，而不是保留空容器或空值。

源稿中不会保存浏览器 Cookie、XSRF token 或 OSS 临时密钥。

## 十三、HTTP 层的安全边界与限制

### 13.1 认证请求与原始请求分开

JSON 请求封装专用于知乎 API，会按完整目标 URL 读取所选浏览器的 Cookie；
获取 XSRF 的首页请求也显式开启这项行为。底层原始 HTTP 请求默认不读取
Cookie，因此 OSS PUT 不会携带 `z_c0`、`d_c0` 或 `_xsrf`。代码中的端点都是
固定地址，不再额外维护一套域名后缀白名单。

`plz.el` 默认让 curl 跟随重定向。curl 不会把显式的 `Cookie` 或
`Authorization` header 带到不同 origin，除非使用了本项目没有启用的
`--location-trusted`；但其它自定义 header 会继续用于重定向后的请求。
这不是本项目自己实现的逐 hop 白名单；新增认证请求时仍应使用固定的知乎
端点。

### 13.2 其它限制

- 必须显式配置 `zhihu-cookie-profile-directory`；本包不发现或猜测 profile。
- Firefox 只读取默认容器；Chromium 系不读取分区 Cookie。
- Chromium 系当前只支持这里说明的 `v10`/`v11` 密文格式。
- 每次只读取显式配置的一个 profile，不合并不同 profile。
- 所有请求都是同步请求，网络慢时会阻塞当前 Emacs 操作。
- 图片状态轮询有固定次数，服务端处理太慢会超时。
- 知乎接口是非公开接口，网页端变化可能导致 Cookie、签名、payload 或响应
  校验失效。

Cookie 相当于所选浏览器的当前登录权限，不应把请求 header、Cookie 存储或
其中的值贴到 issue、日志或聊天中。

## 十四、从入口到实现函数

如果要对照源码阅读，可以按下面顺序：

| 层次 | 主要函数 |
| --- | --- |
| 命令入口 | `zhihu-new-answer`、`zhihu-new-column`、`zhihu-publish` |
| 编辑补全 | `zhihu-mode`、`zhihu-completion-at-point`、`zhihu--topic-capf` |
| metadata | `zhihu--read-zhihu-meta`、`zhihu--write-zhihu-meta` |
| 编译转换 | `zhihu--source-to-html`、`zhihu--zhihuify-html` |
| 图片重写 | `zhihu--rewrite-img-srcs`、`zhihu--upload-bytes` |
| 浏览器 Cookie 配置 | `zhihu-cookie-browser`、`zhihu-cookie-profile-directory`、`zhihu--cookie-store-file` |
| 浏览器 Cookie 入口 | `zhihu--read-browser-cookies` |
| Firefox backend | `zhihu--read-firefox-cookies` |
| Chromium backend | `zhihu--read-chromium-cookies` |
| session XSRF | `zhihu--make-xsrf-state`、`zhihu--xsrf-token-after-headers`、`zhihu--ensure-xsrf-token` |
| ZSE | `zhihu--zse-request-headers`、`zhihu--zse96-header` |
| HTTP | `zhihu--http`、`zhihu--http-json` |
| 回答发布 | `zhihu--publish-answer-file`、`zhihu--publish-answer` |
| 文章发布 | `zhihu--publish-article-file`、`zhihu--publish-article`、`zhihu--create-article-draft` |
| 文章话题 | `zhihu--search-article-topics`、`zhihu--get-article-topics`、`zhihu--sync-article-topics` |
| 新建专栏 | `zhihu--create-column`、`zhihu--column-manual-censor-result` |
| 专栏状态 | `zhihu--article-in-column-p`、`zhihu--add-article-to-column` |

最重要的阅读主线是：

```text
zhihu-publish
  → 读取 metadata
  → 转换 HTML
  → 重写图片
  → 回答流程或文章流程
  → zhihu--http-json
  → zhihu--http
  → 所选浏览器 Cookie / 可选 XSRF state / 可选 ZSE
```
