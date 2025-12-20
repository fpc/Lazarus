
# MarkDown demo

## Plain text and bullet lists

This is some plain text with a [website link](https://www.freepascal.org) embedded. It will open when clicked.

* **a** - Length of alphabet **A** . This is going to be a very long line to see how line wrappping is handled in bullet lists. It *should* be handled gracefully.
   * indented bullet 1. *Again* a very long line to see line wrappping is handled in bullet lists. It should be handled   gracefully.
   * indented bullet 2. Again a very long line to see line wrappping is handled in bullet lists. It should be handled   gracefully.

* **k** - Count of encoding chars.

# Numbered lists

Numbered lists are also supported:

1. item 1
2. Item 2
3. Item 3.

# Image

This is a sentence with ![markdown](markdown.png) image embedded.

Or a complete paragraph:

![FPC logo](demo.png)

## Block quote

Some normal text

> Quoted text line 1

> Quoted text line 2


# A table

We can also render markdown tables

| Letter | ASCII | ASCII HEX |
|--------|-------|---------| 
| A      | 65 | 41  |
| B      | 66 | 42  |
| C      | 67 | 43  |

# Code

Code is rendered differently from the main text:

```pascal
begin
  Writeln('Hello, world!');
end.
```
