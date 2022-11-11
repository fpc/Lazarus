//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QTEXTCURSOR_C_H
#define QTEXTCURSOR_C_H

#include <QtGui>
#include "pascalbind.h"

C_EXPORT QTextCursorH QTextCursor_Create();
C_EXPORT void QTextCursor_Destroy(QTextCursorH handle);
C_EXPORT QTextCursorH QTextCursor_Create2(QTextDocumentH document);
C_EXPORT QTextCursorH QTextCursor_Create3(QTextFrameH frame);
C_EXPORT QTextCursorH QTextCursor_Create4(const QTextBlockH block);
C_EXPORT QTextCursorH QTextCursor_Create6(const QTextCursorH cursor);
C_EXPORT void QTextCursor_swap(QTextCursorH handle, QTextCursorH other);
C_EXPORT bool QTextCursor_isNull(QTextCursorH handle);
C_EXPORT void QTextCursor_setPosition(QTextCursorH handle, int pos, QTextCursor::MoveMode mode);
C_EXPORT int QTextCursor_position(QTextCursorH handle);
C_EXPORT int QTextCursor_positionInBlock(QTextCursorH handle);
C_EXPORT int QTextCursor_anchor(QTextCursorH handle);
C_EXPORT void QTextCursor_insertText(QTextCursorH handle, PWideString text);
C_EXPORT void QTextCursor_insertText2(QTextCursorH handle, PWideString text, const QTextCharFormatH format);
C_EXPORT bool QTextCursor_movePosition(QTextCursorH handle, QTextCursor::MoveOperation op, QTextCursor::MoveMode AnonParam2, int n);
C_EXPORT bool QTextCursor_visualNavigation(QTextCursorH handle);
C_EXPORT void QTextCursor_setVisualNavigation(QTextCursorH handle, bool b);
C_EXPORT void QTextCursor_setVerticalMovementX(QTextCursorH handle, int x);
C_EXPORT int QTextCursor_verticalMovementX(QTextCursorH handle);
C_EXPORT void QTextCursor_setKeepPositionOnInsert(QTextCursorH handle, bool b);
C_EXPORT bool QTextCursor_keepPositionOnInsert(QTextCursorH handle);
C_EXPORT void QTextCursor_deleteChar(QTextCursorH handle);
C_EXPORT void QTextCursor_deletePreviousChar(QTextCursorH handle);
C_EXPORT void QTextCursor_select(QTextCursorH handle, QTextCursor::SelectionType selection);
C_EXPORT bool QTextCursor_hasSelection(QTextCursorH handle);
C_EXPORT bool QTextCursor_hasComplexSelection(QTextCursorH handle);
C_EXPORT void QTextCursor_removeSelectedText(QTextCursorH handle);
C_EXPORT void QTextCursor_clearSelection(QTextCursorH handle);
C_EXPORT int QTextCursor_selectionStart(QTextCursorH handle);
C_EXPORT int QTextCursor_selectionEnd(QTextCursorH handle);
C_EXPORT void QTextCursor_selectedText(QTextCursorH handle, PWideString retval);
C_EXPORT void QTextCursor_selection(QTextCursorH handle, QTextDocumentFragmentH retval);
C_EXPORT void QTextCursor_selectedTableCells(QTextCursorH handle, int* firstRow, int* numRows, int* firstColumn, int* numColumns);
C_EXPORT void QTextCursor_block(QTextCursorH handle, QTextBlockH retval);
C_EXPORT void QTextCursor_charFormat(QTextCursorH handle, QTextCharFormatH retval);
C_EXPORT void QTextCursor_setCharFormat(QTextCursorH handle, const QTextCharFormatH format);
C_EXPORT void QTextCursor_mergeCharFormat(QTextCursorH handle, const QTextCharFormatH modifier);
C_EXPORT void QTextCursor_blockFormat(QTextCursorH handle, QTextBlockFormatH retval);
C_EXPORT void QTextCursor_setBlockFormat(QTextCursorH handle, const QTextBlockFormatH format);
C_EXPORT void QTextCursor_mergeBlockFormat(QTextCursorH handle, const QTextBlockFormatH modifier);
C_EXPORT void QTextCursor_blockCharFormat(QTextCursorH handle, QTextCharFormatH retval);
C_EXPORT void QTextCursor_setBlockCharFormat(QTextCursorH handle, const QTextCharFormatH format);
C_EXPORT void QTextCursor_mergeBlockCharFormat(QTextCursorH handle, const QTextCharFormatH modifier);
C_EXPORT bool QTextCursor_atBlockStart(QTextCursorH handle);
C_EXPORT bool QTextCursor_atBlockEnd(QTextCursorH handle);
C_EXPORT bool QTextCursor_atStart(QTextCursorH handle);
C_EXPORT bool QTextCursor_atEnd(QTextCursorH handle);
C_EXPORT void QTextCursor_insertBlock(QTextCursorH handle);
C_EXPORT void QTextCursor_insertBlock2(QTextCursorH handle, const QTextBlockFormatH format);
C_EXPORT void QTextCursor_insertBlock3(QTextCursorH handle, const QTextBlockFormatH format, const QTextCharFormatH charFormat);
C_EXPORT QTextListH QTextCursor_insertList(QTextCursorH handle, const QTextListFormatH format);
C_EXPORT QTextListH QTextCursor_insertList2(QTextCursorH handle, QTextListFormat::Style style);
C_EXPORT QTextListH QTextCursor_createList(QTextCursorH handle, const QTextListFormatH format);
C_EXPORT QTextListH QTextCursor_createList2(QTextCursorH handle, QTextListFormat::Style style);
C_EXPORT QTextListH QTextCursor_currentList(QTextCursorH handle);
C_EXPORT QTextTableH QTextCursor_insertTable(QTextCursorH handle, int rows, int cols, const QTextTableFormatH format);
C_EXPORT QTextTableH QTextCursor_insertTable2(QTextCursorH handle, int rows, int cols);
C_EXPORT QTextTableH QTextCursor_currentTable(QTextCursorH handle);
C_EXPORT QTextFrameH QTextCursor_insertFrame(QTextCursorH handle, const QTextFrameFormatH format);
C_EXPORT QTextFrameH QTextCursor_currentFrame(QTextCursorH handle);
C_EXPORT void QTextCursor_insertFragment(QTextCursorH handle, const QTextDocumentFragmentH fragment);
C_EXPORT void QTextCursor_insertHtml(QTextCursorH handle, PWideString html);
C_EXPORT void QTextCursor_insertImage(QTextCursorH handle, const QTextImageFormatH format, QTextFrameFormat::Position alignment);
C_EXPORT void QTextCursor_insertImage2(QTextCursorH handle, const QTextImageFormatH format);
C_EXPORT void QTextCursor_insertImage3(QTextCursorH handle, PWideString name);
C_EXPORT void QTextCursor_insertImage4(QTextCursorH handle, const QImageH image, PWideString name);
C_EXPORT void QTextCursor_beginEditBlock(QTextCursorH handle);
C_EXPORT void QTextCursor_joinPreviousEditBlock(QTextCursorH handle);
C_EXPORT void QTextCursor_endEditBlock(QTextCursorH handle);
C_EXPORT bool QTextCursor_isCopyOf(QTextCursorH handle, const QTextCursorH other);
C_EXPORT int QTextCursor_blockNumber(QTextCursorH handle);
C_EXPORT int QTextCursor_columnNumber(QTextCursorH handle);
C_EXPORT QTextDocumentH QTextCursor_document(QTextCursorH handle);

#endif
