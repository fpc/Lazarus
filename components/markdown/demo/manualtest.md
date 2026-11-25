# Manual test document

Load this file with *Select file...* or start the demo with `demogui manualtest.md`.
Ctrl+C shows the copied text between square brackets.

## 1. Hand cursor over links (#42759)

Hover over [this link](https://www.lazarus-ide.org) and over the autolink
<https://www.freepascal.org>: the cursor becomes a hand. Move off the link: the
normal cursor returns. Start a drag selection on plain text and move over a
link: the cursor does not change while dragging.

Scroll down first, then hover the link at the end of the document: the hand
appears exactly over the link text.

## 2. Horizontal rule (#42762)

The rule below spans the full width. Resize the window: it follows the width.

---

Inside a block quote the rule starts at the quote indent:

> Quoted text above the rule.
>
> ***
>
> Quoted text below the rule.

## 3. Keyboard scrolling (#42764)

The control has the focus at start. Test:

- Down / Up: one line
- PgDn / PgUp: one page
- Home / End and Ctrl+Home / Ctrl+End: top and bottom of the document
- Left / Right: horizontal scroll, only when a horizontal scrollbar is shown
- Arrow keys do not move the focus to the *Select file...* button

## 4. Shift+click selection (#42760)

1. Click before the word *alpha* in the line below, then Shift+click after
   *delta*: alpha to delta is selected.
2. Shift+click after *beta*: the selection shrinks to alpha..beta.
3. Drag from *delta* back to *gamma* (right to left), then Shift+click on
   *alpha*: the selection runs from alpha to the drag start in delta.
4. Shift+click, keep the button down and drag: the selection follows the mouse.
5. Resize the window, then Shift+click: behaves like a plain click.

alpha beta gamma delta epsilon

## 5. Double and triple click (#42765)

Double click each word below, then press Ctrl+C:

- plain: `word`, `under_score`, `123abc`
- formatting inside a word: **bold**plain and *italic*_tail → one word
- punctuation ends a word: hello, world! (double click on the comma: nothing)
- Unicode: naïve café, Ελληνικά, кириллица, l’apostrophe (the ’ ends a word)
- link text: [linked word](https://www.lazarus-ide.org)

Triple click selects one visual line. Make the window narrow so the next
paragraph wraps, then triple click on its second line: only that line is
selected, not the whole paragraph.

Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor
incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis
nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.

## 6. Long code lines (#42761)

With *Wrap code blocks* off, the long line below makes a horizontal scrollbar
appear; scroll right to read its end. The code background extends under
the whole line. Without a long line in the document there is no horizontal
scrollbar.

With *Wrap code blocks* on, the line wraps at the window edge between
characters, no horizontal scrollbar is shown, and spaces are kept. Select the
whole code block and press Ctrl+C: the long line is copied as one line, the
other lines stay separate.

```
short line before
procedure TSomeVeryLongClassName.SomeVeryLongMethodName(const aFirstArgument: string; aSecondArgument: Integer; out aResult: TStringList); // end of the long line
    indented line after
```

## 7. Tight lists (#42758)

The items of this tight list follow each other with no blank space, the nested
list included:

- one
- two
  - nested a
  - nested b
- three

The items of this loose list are separated by paragraph spacing:

- first paragraph

  second paragraph of the same item
- next item

## 8. Spacing properties (#42758)

Toggle *GitHub spacing* and compare:

- headings get extra space above them, except a heading at the top of the
  document
- the items of this list move apart a little
- the code block below gets padding on all sides, with the background
  covering the padding; this also works with *Wrap code blocks* on
- table cells get wider horizontal padding

```
padded code
```

| Padding | Value |
|---------|-------|
| cell    | cell  |

Unchecking the box restores the original look.

## Filler to make the document scroll

1. One
2. Two
3. Three
4. Four
5. Five
6. Six
7. Seven
8. Eight
9. Nine
10. Ten

| Column A | Column B |
|----------|----------|
| cell 1   | cell 2   |
| cell 3   | cell 4   |

```
code line 1
code line 2
```

Paragraph one.

Paragraph two.

Paragraph three.

Paragraph four.

Last line with a [link at the end](https://wiki.freepascal.org).
