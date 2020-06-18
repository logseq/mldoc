# Markdown 语法参考

## 0. 简述

Markdown 是一种 Lightweight 标记语言，易读、易写、易改，主要为了方便在 Web 端快速书写文档，由转换器转换为 HTML 呈现在 Web 页面上，最初的 Markdown 转换程序，是一个 Perl 脚本。Markdown 可以使用简单的几个字符，如 `#`, `*`, 等，编写出格式丰富的整齐化一的文档来。

现在，越来越多的 Web 程序支持 Markdown 的在线编辑和展示，如：

* [GitHub](https://github.com/)
* [Stack Overflow](http://stackoverflow.com/)
* [Reddit](https://www.reddit.com/)
* [简书](http://www.jianshu.com/)
* [作业部落](https://www.zybuluo.com/)

专用于 Downdown 写作或支持该格式编写的编辑器很多，Windows 平台有 Typora, MarkdownPad, MarkPad 等。OSX 平台可选的就太多了，有 Mou, Ulysses, iA Writer, Typed, MacDown, Typora 等一系列好用的工具软件。除此之外，还有一大批如 GNU Emacs, Vim, Sublime Tex, Atom 等猿类编辑器也支持这种格式。

Markdown 文件一般用 `.md` 或 `.markdown` 作为扩展名。


## 1. 标准 Markdown

### 1.1 加粗和强调

```
*emphasize* **strong**

_emphasize_ __strong__

_强调是可以**嵌套**的_
```

*emphasize* **emphasize**

_emphasize_ __strong__

_强调是可以**嵌套**的_

### 1.2 链接和Email

```
链接到 [Github](https://github.com/ "Github").
```

链接到 [Github](https://github.com/ "Github").

```
定义链接的ID后 [example][id]，可以在文档下方再定义链接的目标地址：
```

```
    [id]: http://example.com/  "Title"
```

定义链接的ID后 [example][id]，可以在文档下方再定义链接的目标地址：

[id]: http://example.com/  "Title"

电子邮件链接：

```
电子邮件链接实例 <example@example.com>
```

电子邮件链接实例 <example@example.com>

### 1.3 图片

```
![图片替换文本（禁止显示图片时会显示此文本）](/path/img.jpg "图片标题（鼠标放到图片上时会显示此文本）")
```

图片也可以先插入到正文，之后再根据 id 定义图片的路径和显示文本：

```
![图片替换文本（禁止显示图片时会显示此文本）][id]
```

```
    [id]: /url/to/img.jpg "图片标题（鼠标放到图片上时会显示此文本）"
```

### 1.4 标题

Markdown 支持两种标题的语法，类 Setext 和类 Atx 形式。

Atx 形式最多支持6级标题：

```
# Header 1 #
## Header 2 ##
...
###### Header 6
```

类 Setext 形式是用底线的形式，利用 = （一级标题）和 - （二级标题）

```
Header 1
========
Header 2
--------
```

这种方式不推荐，统一使用类 Atx 形式即可。

### 1.5 列表

Markdown 支持有序列表和无序列表。

`<ul>` 无序列表使用星号`*`、加号`+`或是减号`-`-作为列表标记：

```
* Item 1
* Item 2
  * Item 2a
  * Item 2b
```

* Item 1
* Item 2
  * Item 2a
  * Item 2b

`<ol>` 有序列表则使用数字接着一个英文句点：

有序列表和无序列表可以混合嵌套：

```
1. Item 1
2. Item 2
3. Item 3
   * Item 3a
   * Item 3b
```

1. Item 1
2. Item 2
3. Item 3
   * Item 3a
   * Item 3b

需要说明的一点是，你在列表标记上使用的数字并不会影响输出的 HTML 结果，上面的列表所产生的 HTML 标记为：

    <ol>
    <li>Bird</li>
    <li>McHale</li>
    <li>Parish</li>
    </ol>

如果你的列表标记写成：

```
1.  Bird
1.  McHale
1.  Parish
```

甚至是：

```
3. Bird
1. McHale
8. Parish
```

你都会得到完全相同的 HTML 输出。重点在于，你可以让 Markdown 文件的列表数字和输出的结果相同，或是你懒一点，你可以完全不用在意数字的正确性。

如果你使用懒惰的写法，建议第一个项目最好还是从 1. 开始，因为 Markdown 未来可能会支持有序列表的 start 属性。

当然，项目列表很可能会不小心产生，像是下面这样的写法

```
1986. What a great season.
```

换句话说，也就是在行首出现数字-句点-空白，要避免这样的状况，你可以在句点前面加上反斜杠。

```
1986\. What a great season.
```

### 1.6 引用

```
> Email-style angle brackets
> are used for blockquotes.
> > And, they can be nested.
> #### Headers in blockquotes
>
> * You can quote a list.
> * Etc.
```

> Email-style angle brackets
> are used for blockquotes.
> > And, they can be nested.
>
> #### Headers in blockquotes
>
> * You can quote a list.
> * Etc.

### 1.7 代码

#### 1.7.1 行内代码

```
行内代码 `<code>`，也可以放在两对反引号之间：`` <code> ``。
```

行内代码 `<code>`，也可以放在两对反引号之间：`` <code> ``。

#### 1.7.2 代码块

代码块每行前添加 缩进 4个空格 或 1个制表符：

    #!/usr/bin/perl
    use strict;
    use warnings;

    # first, create your message
    use Email::MIME;
    my $message = Email::MIME->create(
      header_str => [
        From    => 'you@example.com',
        To      => 'friend@example.com',
        Subject => 'Happy birthday!',
      ],
      attributes => {
        encoding => 'quoted-printable',
        charset  => 'ISO-8859-1',
      },
      body_str => "Happy birthday to you!\n",
    );

    # send the message
    use Email::Sender::Simple qw(sendmail);
    sendmail($message);

下文也会提到 GitHub 支持的可指定编程语言的代码块，带语法高亮：

    ```<lang-name>
    ```
示例输出（注意，这是 GitHub 支持的 Markdown 格式，用其他 Markdown 编辑器要能无法正常解析改代码块）：

```perl
#!/usr/bin/perl
use strict;
use warnings;

# first, create your message
use Email::MIME;
my $message = Email::MIME->create(
  header_str => [
    From    => 'you@example.com',
    To      => 'friend@example.com',
    Subject => 'Happy birthday!',
  ],
  attributes => {
    encoding => 'quoted-printable',
    charset  => 'ISO-8859-1',
  },
  body_str => "Happy birthday to you!\n",
);

# send the message
use Email::Sender::Simple qw(sendmail);
sendmail($message);
```

### 1.8 换行

插入一个空白行即可

### 1.9 水平线

3个以上短线或*号：

```
---
* * *
- - - -
```

### 1.10 反斜杠

Markdown 可以利用反斜杠来插入一些在语法中有其它意义的符号，例如：如果你想要用星号加在文字旁边的方式来做出强调效果（但不用 `<em>` 标签），你可以在星号的前面加上反斜杠：

```
\*literal asterisks\*
```

Markdown 支持以下这些符号前面加上反斜杠来帮助插入普通的符号：

```
\   反斜线
`   反引号
*   星号
_   底线
{}  花括号
[]  方括号
()  括弧
#   井字号
+   加号
-   减号
.   英文句点
!   惊叹号
```

---

## 2. 其他语法

### 2.1 脚注

```
这些文字带有脚注[^1]
```
    [^1]: 我是脚注。

### 2.2 表格

简单表格：

```
First Header | Second Header | Third Header
------------ | ------------- | ------------
Content Cell | Content Cell  | Content Cell
Content Cell | Content Cell  | Content Cell
```

First Header | Second Header | Third Header
------------ | ------------- | ------------
Content Cell | Content Cell  | Content Cell
Content Cell | Content Cell  | Content Cell

也可以在行首和行尾加上 | ，效果一样：

```
| First Header | Second Header | Third Header |
| ------------ | ------------- | ------------ |
| Content Cell | Content Cell  | Content Cell |
| Content Cell | Content Cell  | Content Cell |
```

| First Header | Second Header | Third Header |
| ------------ | ------------- | ------------ |
| Content Cell | Content Cell  | Content Cell |
| Content Cell | Content Cell  | Content Cell |

使用英文冒号可以给列设定对齐方式：

```
First Header | Second Header | Third Header
:----------- | :-----------: | -----------:
Left         | Center        | Right
Left         | Center        | Right
```

First Header | Second Header | Third Header
:----------- | :-----------: | -----------:
Left         | Center        | Right
Left         | Center        | Right

### 2.3 锚点

Markdown 中也可以给使用锚链接，下面这是一个普通的 H2 标题：

```
## H2 标题实例
```

加个 id 属性就可以给标题加上锚点：

```
## [带锚点的 H2 实例](id:anchor1)
```

链接到上面的锚点，我们只需要如下的语法即可：

```
预览时点击 [锚链接](#anchor1)
```

### 2.4 删除线

```
~~Strikethrough~~
```

~~Strikethrough 实例~~

## 3. GitHub 支持的 Markdown 语法

### 3.1 语法高亮

    ```javascript
    function fancyAlert(arg) {
      if(arg) {
        $.facebox({div:'#foo'})
      }
    }
    ```

```javascript
function fancyAlert(arg) {
  if(arg) {
    $.facebox({div:'#foo'})
  }
}
```

GitHub 支持的编程语主高亮列表，请查看 [linguist](https://github.com/github/linguist/blob/master/lib/linguist/languages.yml)。

### 3.2 任务列表

```
- [x] @mentions, #refs, [links](), **formatting**, and <del>tags</del> supported
- [x] list syntax required (any unordered or ordered list supported)
- [x] this is a complete item
- [ ] this is an incomplete item
```

- [x] @mentions, #refs, [links](), **formatting**, and <del>tags</del> supported
- [x] list syntax required (any unordered or ordered list supported)
- [x] this is a complete item
- [ ] this is an incomplete item

### 3.3 SHA 引用

GitHub 上每个提交都有一个 SHA-1 hash，用它在文档中添加一个指向 GitHut 提交的链接：

```
16c999e8c71134401a78d4d46435517b2271d6ac
mojombo@16c999e8c71134401a78d4d46435517b2271d6ac
mojombo/github-flavored-markdown@16c999e8c71134401a78d4d46435517b2271d6ac
```

### 3.4 同一个仓库中的 Issue 引用

类似 SHA 引用，也可以添加指定编码的仓库内 Issue 或 Pull Request 链接：

```
#1
mojombo#1
mojombo/github-flavored-markdown#1
```

### 3.5 @某用户

类似微博，也可以在 GitHub Markdown 文档中添加 `@WisdomFusion` 的提醒。

### 3.6 自动链接

任何光秃秃的链接都会被自动转为链接的，如

https://github.com/

### 3.7 对 emoji 的支持

这个比较炫酷，文档中还支持 emoji！

```
:smile: :exclamation: :thumbsup:
```
:smile: :exclamation: :thumbsup:

emoji列表：http://www.emoji-cheat-sheet.com/

## 4. 参考文档

* https://guides.github.com/features/mastering-markdown/
* https://help.github.com/articles/basic-writing-and-formatting-syntax/
* http://www.markdown.cn/
