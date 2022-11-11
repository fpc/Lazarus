//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qtextcursor_c.h"

QTextCursorH QTextCursor_Create()
{
	return (QTextCursorH) new QTextCursor();
}

void QTextCursor_Destroy(QTextCursorH handle)
{
	delete (QTextCursor *)handle;
}

QTextCursorH QTextCursor_Create2(QTextDocumentH document)
{
	return (QTextCursorH) new QTextCursor((QTextDocument*)document);
}

QTextCursorH QTextCursor_Create3(QTextFrameH frame)
{
	return (QTextCursorH) new QTextCursor((QTextFrame*)frame);
}

QTextCursorH QTextCursor_Create4(const QTextBlockH block)
{
	return (QTextCursorH) new QTextCursor(*(const QTextBlock*)block);
}

QTextCursorH QTextCursor_Create6(const QTextCursorH cursor)
{
	return (QTextCursorH) new QTextCursor(*(const QTextCursor*)cursor);
}

void QTextCursor_swap(QTextCursorH handle, QTextCursorH other)
{
	((QTextCursor *)handle)->swap(*(QTextCursor*)other);
}

bool QTextCursor_isNull(QTextCursorH handle)
{
	return (bool) ((QTextCursor *)handle)->isNull();
}

void QTextCursor_setPosition(QTextCursorH handle, int pos, QTextCursor::MoveMode mode)
{
	((QTextCursor *)handle)->setPosition(pos, mode);
}

int QTextCursor_position(QTextCursorH handle)
{
	return (int) ((QTextCursor *)handle)->position();
}

int QTextCursor_positionInBlock(QTextCursorH handle)
{
	return (int) ((QTextCursor *)handle)->positionInBlock();
}

int QTextCursor_anchor(QTextCursorH handle)
{
	return (int) ((QTextCursor *)handle)->anchor();
}

void QTextCursor_insertText(QTextCursorH handle, PWideString text)
{
	QString t_text;
	copyPWideStringToQString(text, t_text);
	((QTextCursor *)handle)->insertText(t_text);
}

void QTextCursor_insertText2(QTextCursorH handle, PWideString text, const QTextCharFormatH format)
{
	QString t_text;
	copyPWideStringToQString(text, t_text);
	((QTextCursor *)handle)->insertText(t_text, *(const QTextCharFormat*)format);
}

bool QTextCursor_movePosition(QTextCursorH handle, QTextCursor::MoveOperation op, QTextCursor::MoveMode AnonParam2, int n)
{
	return (bool) ((QTextCursor *)handle)->movePosition(op, AnonParam2, n);
}

bool QTextCursor_visualNavigation(QTextCursorH handle)
{
	return (bool) ((QTextCursor *)handle)->visualNavigation();
}

void QTextCursor_setVisualNavigation(QTextCursorH handle, bool b)
{
	((QTextCursor *)handle)->setVisualNavigation(b);
}

void QTextCursor_setVerticalMovementX(QTextCursorH handle, int x)
{
	((QTextCursor *)handle)->setVerticalMovementX(x);
}

int QTextCursor_verticalMovementX(QTextCursorH handle)
{
	return (int) ((QTextCursor *)handle)->verticalMovementX();
}

void QTextCursor_setKeepPositionOnInsert(QTextCursorH handle, bool b)
{
	((QTextCursor *)handle)->setKeepPositionOnInsert(b);
}

bool QTextCursor_keepPositionOnInsert(QTextCursorH handle)
{
	return (bool) ((QTextCursor *)handle)->keepPositionOnInsert();
}

void QTextCursor_deleteChar(QTextCursorH handle)
{
	((QTextCursor *)handle)->deleteChar();
}

void QTextCursor_deletePreviousChar(QTextCursorH handle)
{
	((QTextCursor *)handle)->deletePreviousChar();
}

void QTextCursor_select(QTextCursorH handle, QTextCursor::SelectionType selection)
{
	((QTextCursor *)handle)->select(selection);
}

bool QTextCursor_hasSelection(QTextCursorH handle)
{
	return (bool) ((QTextCursor *)handle)->hasSelection();
}

bool QTextCursor_hasComplexSelection(QTextCursorH handle)
{
	return (bool) ((QTextCursor *)handle)->hasComplexSelection();
}

void QTextCursor_removeSelectedText(QTextCursorH handle)
{
	((QTextCursor *)handle)->removeSelectedText();
}

void QTextCursor_clearSelection(QTextCursorH handle)
{
	((QTextCursor *)handle)->clearSelection();
}

int QTextCursor_selectionStart(QTextCursorH handle)
{
	return (int) ((QTextCursor *)handle)->selectionStart();
}

int QTextCursor_selectionEnd(QTextCursorH handle)
{
	return (int) ((QTextCursor *)handle)->selectionEnd();
}

void QTextCursor_selectedText(QTextCursorH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QTextCursor *)handle)->selectedText();
	copyQStringToPWideString(t_retval, retval);
}

void QTextCursor_selection(QTextCursorH handle, QTextDocumentFragmentH retval)
{
	*(QTextDocumentFragment *)retval = ((QTextCursor *)handle)->selection();
}

void QTextCursor_selectedTableCells(QTextCursorH handle, int* firstRow, int* numRows, int* firstColumn, int* numColumns)
{
	((QTextCursor *)handle)->selectedTableCells(firstRow, numRows, firstColumn, numColumns);
}

void QTextCursor_block(QTextCursorH handle, QTextBlockH retval)
{
	*(QTextBlock *)retval = ((QTextCursor *)handle)->block();
}

void QTextCursor_charFormat(QTextCursorH handle, QTextCharFormatH retval)
{
	*(QTextCharFormat *)retval = ((QTextCursor *)handle)->charFormat();
}

void QTextCursor_setCharFormat(QTextCursorH handle, const QTextCharFormatH format)
{
	((QTextCursor *)handle)->setCharFormat(*(const QTextCharFormat*)format);
}

void QTextCursor_mergeCharFormat(QTextCursorH handle, const QTextCharFormatH modifier)
{
	((QTextCursor *)handle)->mergeCharFormat(*(const QTextCharFormat*)modifier);
}

void QTextCursor_blockFormat(QTextCursorH handle, QTextBlockFormatH retval)
{
	*(QTextBlockFormat *)retval = ((QTextCursor *)handle)->blockFormat();
}

void QTextCursor_setBlockFormat(QTextCursorH handle, const QTextBlockFormatH format)
{
	((QTextCursor *)handle)->setBlockFormat(*(const QTextBlockFormat*)format);
}

void QTextCursor_mergeBlockFormat(QTextCursorH handle, const QTextBlockFormatH modifier)
{
	((QTextCursor *)handle)->mergeBlockFormat(*(const QTextBlockFormat*)modifier);
}

void QTextCursor_blockCharFormat(QTextCursorH handle, QTextCharFormatH retval)
{
	*(QTextCharFormat *)retval = ((QTextCursor *)handle)->blockCharFormat();
}

void QTextCursor_setBlockCharFormat(QTextCursorH handle, const QTextCharFormatH format)
{
	((QTextCursor *)handle)->setBlockCharFormat(*(const QTextCharFormat*)format);
}

void QTextCursor_mergeBlockCharFormat(QTextCursorH handle, const QTextCharFormatH modifier)
{
	((QTextCursor *)handle)->mergeBlockCharFormat(*(const QTextCharFormat*)modifier);
}

bool QTextCursor_atBlockStart(QTextCursorH handle)
{
	return (bool) ((QTextCursor *)handle)->atBlockStart();
}

bool QTextCursor_atBlockEnd(QTextCursorH handle)
{
	return (bool) ((QTextCursor *)handle)->atBlockEnd();
}

bool QTextCursor_atStart(QTextCursorH handle)
{
	return (bool) ((QTextCursor *)handle)->atStart();
}

bool QTextCursor_atEnd(QTextCursorH handle)
{
	return (bool) ((QTextCursor *)handle)->atEnd();
}

void QTextCursor_insertBlock(QTextCursorH handle)
{
	((QTextCursor *)handle)->insertBlock();
}

void QTextCursor_insertBlock2(QTextCursorH handle, const QTextBlockFormatH format)
{
	((QTextCursor *)handle)->insertBlock(*(const QTextBlockFormat*)format);
}

void QTextCursor_insertBlock3(QTextCursorH handle, const QTextBlockFormatH format, const QTextCharFormatH charFormat)
{
	((QTextCursor *)handle)->insertBlock(*(const QTextBlockFormat*)format, *(const QTextCharFormat*)charFormat);
}

QTextListH QTextCursor_insertList(QTextCursorH handle, const QTextListFormatH format)
{
	return (QTextListH) ((QTextCursor *)handle)->insertList(*(const QTextListFormat*)format);
}

QTextListH QTextCursor_insertList2(QTextCursorH handle, QTextListFormat::Style style)
{
	return (QTextListH) ((QTextCursor *)handle)->insertList(style);
}

QTextListH QTextCursor_createList(QTextCursorH handle, const QTextListFormatH format)
{
	return (QTextListH) ((QTextCursor *)handle)->createList(*(const QTextListFormat*)format);
}

QTextListH QTextCursor_createList2(QTextCursorH handle, QTextListFormat::Style style)
{
	return (QTextListH) ((QTextCursor *)handle)->createList(style);
}

QTextListH QTextCursor_currentList(QTextCursorH handle)
{
	return (QTextListH) ((QTextCursor *)handle)->currentList();
}

QTextTableH QTextCursor_insertTable(QTextCursorH handle, int rows, int cols, const QTextTableFormatH format)
{
	return (QTextTableH) ((QTextCursor *)handle)->insertTable(rows, cols, *(const QTextTableFormat*)format);
}

QTextTableH QTextCursor_insertTable2(QTextCursorH handle, int rows, int cols)
{
	return (QTextTableH) ((QTextCursor *)handle)->insertTable(rows, cols);
}

QTextTableH QTextCursor_currentTable(QTextCursorH handle)
{
	return (QTextTableH) ((QTextCursor *)handle)->currentTable();
}

QTextFrameH QTextCursor_insertFrame(QTextCursorH handle, const QTextFrameFormatH format)
{
	return (QTextFrameH) ((QTextCursor *)handle)->insertFrame(*(const QTextFrameFormat*)format);
}

QTextFrameH QTextCursor_currentFrame(QTextCursorH handle)
{
	return (QTextFrameH) ((QTextCursor *)handle)->currentFrame();
}

void QTextCursor_insertFragment(QTextCursorH handle, const QTextDocumentFragmentH fragment)
{
	((QTextCursor *)handle)->insertFragment(*(const QTextDocumentFragment*)fragment);
}

void QTextCursor_insertHtml(QTextCursorH handle, PWideString html)
{
	QString t_html;
	copyPWideStringToQString(html, t_html);
	((QTextCursor *)handle)->insertHtml(t_html);
}

void QTextCursor_insertImage(QTextCursorH handle, const QTextImageFormatH format, QTextFrameFormat::Position alignment)
{
	((QTextCursor *)handle)->insertImage(*(const QTextImageFormat*)format, alignment);
}

void QTextCursor_insertImage2(QTextCursorH handle, const QTextImageFormatH format)
{
	((QTextCursor *)handle)->insertImage(*(const QTextImageFormat*)format);
}

void QTextCursor_insertImage3(QTextCursorH handle, PWideString name)
{
	QString t_name;
	copyPWideStringToQString(name, t_name);
	((QTextCursor *)handle)->insertImage(t_name);
}

void QTextCursor_insertImage4(QTextCursorH handle, const QImageH image, PWideString name)
{
	QString t_name;
	copyPWideStringToQString(name, t_name);
	((QTextCursor *)handle)->insertImage(*(const QImage*)image, t_name);
}

void QTextCursor_beginEditBlock(QTextCursorH handle)
{
	((QTextCursor *)handle)->beginEditBlock();
}

void QTextCursor_joinPreviousEditBlock(QTextCursorH handle)
{
	((QTextCursor *)handle)->joinPreviousEditBlock();
}

void QTextCursor_endEditBlock(QTextCursorH handle)
{
	((QTextCursor *)handle)->endEditBlock();
}

bool QTextCursor_isCopyOf(QTextCursorH handle, const QTextCursorH other)
{
	return (bool) ((QTextCursor *)handle)->isCopyOf(*(const QTextCursor*)other);
}

int QTextCursor_blockNumber(QTextCursorH handle)
{
	return (int) ((QTextCursor *)handle)->blockNumber();
}

int QTextCursor_columnNumber(QTextCursorH handle)
{
	return (int) ((QTextCursor *)handle)->columnNumber();
}

QTextDocumentH QTextCursor_document(QTextCursorH handle)
{
	return (QTextDocumentH) ((QTextCursor *)handle)->document();
}

