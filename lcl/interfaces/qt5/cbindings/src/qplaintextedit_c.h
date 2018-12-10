//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QPLAINTEXTEDIT_C_H
#define QPLAINTEXTEDIT_C_H

#include <QtWidgets>
#include "pascalbind.h"

C_EXPORT QPlainTextEditH QPlainTextEdit_Create(QWidgetH parent);
C_EXPORT void QPlainTextEdit_Destroy(QPlainTextEditH handle);
C_EXPORT QPlainTextEditH QPlainTextEdit_Create2(PWideString text, QWidgetH parent);
C_EXPORT void QPlainTextEdit_setDocument(QPlainTextEditH handle, QTextDocumentH document);
C_EXPORT QTextDocumentH QPlainTextEdit_document(QPlainTextEditH handle);
C_EXPORT void QPlainTextEdit_setTextCursor(QPlainTextEditH handle, const QTextCursorH cursor);
C_EXPORT void QPlainTextEdit_textCursor(QPlainTextEditH handle, QTextCursorH retval);
C_EXPORT bool QPlainTextEdit_isReadOnly(QPlainTextEditH handle);
C_EXPORT void QPlainTextEdit_setReadOnly(QPlainTextEditH handle, bool ro);
C_EXPORT void QPlainTextEdit_setTextInteractionFlags(QPlainTextEditH handle, unsigned int flags);
C_EXPORT unsigned int QPlainTextEdit_textInteractionFlags(QPlainTextEditH handle);
C_EXPORT void QPlainTextEdit_mergeCurrentCharFormat(QPlainTextEditH handle, const QTextCharFormatH modifier);
C_EXPORT void QPlainTextEdit_setCurrentCharFormat(QPlainTextEditH handle, const QTextCharFormatH format);
C_EXPORT void QPlainTextEdit_currentCharFormat(QPlainTextEditH handle, QTextCharFormatH retval);
C_EXPORT bool QPlainTextEdit_tabChangesFocus(QPlainTextEditH handle);
C_EXPORT void QPlainTextEdit_setTabChangesFocus(QPlainTextEditH handle, bool b);
C_EXPORT void QPlainTextEdit_setDocumentTitle(QPlainTextEditH handle, PWideString title);
C_EXPORT void QPlainTextEdit_documentTitle(QPlainTextEditH handle, PWideString retval);
C_EXPORT bool QPlainTextEdit_isUndoRedoEnabled(QPlainTextEditH handle);
C_EXPORT void QPlainTextEdit_setUndoRedoEnabled(QPlainTextEditH handle, bool enable);
C_EXPORT void QPlainTextEdit_setMaximumBlockCount(QPlainTextEditH handle, int maximum);
C_EXPORT int QPlainTextEdit_maximumBlockCount(QPlainTextEditH handle);
C_EXPORT QPlainTextEdit::LineWrapMode QPlainTextEdit_lineWrapMode(QPlainTextEditH handle);
C_EXPORT void QPlainTextEdit_setLineWrapMode(QPlainTextEditH handle, QPlainTextEdit::LineWrapMode mode);
C_EXPORT QTextOption::WrapMode QPlainTextEdit_wordWrapMode(QPlainTextEditH handle);
C_EXPORT void QPlainTextEdit_setWordWrapMode(QPlainTextEditH handle, QTextOption::WrapMode policy);
C_EXPORT void QPlainTextEdit_setBackgroundVisible(QPlainTextEditH handle, bool visible);
C_EXPORT bool QPlainTextEdit_backgroundVisible(QPlainTextEditH handle);
C_EXPORT void QPlainTextEdit_setCenterOnScroll(QPlainTextEditH handle, bool enabled);
C_EXPORT bool QPlainTextEdit_centerOnScroll(QPlainTextEditH handle);
C_EXPORT bool QPlainTextEdit_find(QPlainTextEditH handle, PWideString exp, unsigned int options);
C_EXPORT void QPlainTextEdit_toPlainText(QPlainTextEditH handle, PWideString retval);
C_EXPORT void QPlainTextEdit_ensureCursorVisible(QPlainTextEditH handle);
C_EXPORT void QPlainTextEdit_loadResource(QPlainTextEditH handle, QVariantH retval, int type, const QUrlH name);
C_EXPORT QMenuH QPlainTextEdit_createStandardContextMenu(QPlainTextEditH handle);
C_EXPORT void QPlainTextEdit_cursorForPosition(QPlainTextEditH handle, QTextCursorH retval, const QPointH pos);
C_EXPORT void QPlainTextEdit_cursorRect(QPlainTextEditH handle, PRect retval, const QTextCursorH cursor);
C_EXPORT void QPlainTextEdit_cursorRect2(QPlainTextEditH handle, PRect retval);
C_EXPORT void QPlainTextEdit_anchorAt(QPlainTextEditH handle, PWideString retval, const QPointH pos);
C_EXPORT bool QPlainTextEdit_overwriteMode(QPlainTextEditH handle);
C_EXPORT void QPlainTextEdit_setOverwriteMode(QPlainTextEditH handle, bool overwrite);
C_EXPORT int QPlainTextEdit_tabStopWidth(QPlainTextEditH handle);
C_EXPORT void QPlainTextEdit_setTabStopWidth(QPlainTextEditH handle, int width);
C_EXPORT int QPlainTextEdit_cursorWidth(QPlainTextEditH handle);
C_EXPORT void QPlainTextEdit_setCursorWidth(QPlainTextEditH handle, int width);
C_EXPORT void QPlainTextEdit_moveCursor(QPlainTextEditH handle, QTextCursor::MoveOperation operation, QTextCursor::MoveMode mode);
C_EXPORT bool QPlainTextEdit_canPaste(QPlainTextEditH handle);
C_EXPORT void QPlainTextEdit_print(QPlainTextEditH handle, QPagedPaintDeviceH printer);
C_EXPORT int QPlainTextEdit_blockCount(QPlainTextEditH handle);
C_EXPORT void QPlainTextEdit_inputMethodQuery(QPlainTextEditH handle, QVariantH retval, Qt::InputMethodQuery property);
C_EXPORT void QPlainTextEdit_setPlainText(QPlainTextEditH handle, PWideString text);
C_EXPORT void QPlainTextEdit_cut(QPlainTextEditH handle);
C_EXPORT void QPlainTextEdit_copy(QPlainTextEditH handle);
C_EXPORT void QPlainTextEdit_paste(QPlainTextEditH handle);
C_EXPORT void QPlainTextEdit_undo(QPlainTextEditH handle);
C_EXPORT void QPlainTextEdit_redo(QPlainTextEditH handle);
C_EXPORT void QPlainTextEdit_clear(QPlainTextEditH handle);
C_EXPORT void QPlainTextEdit_selectAll(QPlainTextEditH handle);
C_EXPORT void QPlainTextEdit_insertPlainText(QPlainTextEditH handle, PWideString text);
C_EXPORT void QPlainTextEdit_appendPlainText(QPlainTextEditH handle, PWideString text);
C_EXPORT void QPlainTextEdit_appendHtml(QPlainTextEditH handle, PWideString html);
C_EXPORT void QPlainTextEdit_centerCursor(QPlainTextEditH handle);
C_EXPORT void QPlainTextEdit_zoomIn(QPlainTextEditH handle, int range);
C_EXPORT void QPlainTextEdit_zoomOut(QPlainTextEditH handle, int range);
C_EXPORT void QPlainTextEdit_placeholderText(QPlainTextEditH handle, PWideString retval);
C_EXPORT void QPlainTextEdit_setPlaceholderText(QPlainTextEditH handle, PWideString text);

C_EXPORT QPlainTextDocumentLayoutH QPlainTextDocumentLayout_Create(QTextDocumentH document);
C_EXPORT void QPlainTextDocumentLayout_Destroy(QPlainTextDocumentLayoutH handle);
C_EXPORT int QPlainTextDocumentLayout_hitTest(QPlainTextDocumentLayoutH handle, const QPointFH AnonParam1, Qt::HitTestAccuracy AnonParam2);
C_EXPORT int QPlainTextDocumentLayout_pageCount(QPlainTextDocumentLayoutH handle);
C_EXPORT void QPlainTextDocumentLayout_documentSize(QPlainTextDocumentLayoutH handle, QSizeFH retval);
C_EXPORT void QPlainTextDocumentLayout_frameBoundingRect(QPlainTextDocumentLayoutH handle, QRectFH retval, QTextFrameH AnonParam1);
C_EXPORT void QPlainTextDocumentLayout_blockBoundingRect(QPlainTextDocumentLayoutH handle, QRectFH retval, const QTextBlockH block);
C_EXPORT void QPlainTextDocumentLayout_ensureBlockLayout(QPlainTextDocumentLayoutH handle, const QTextBlockH block);
C_EXPORT void QPlainTextDocumentLayout_setCursorWidth(QPlainTextDocumentLayoutH handle, int width);
C_EXPORT int QPlainTextDocumentLayout_cursorWidth(QPlainTextDocumentLayoutH handle);
C_EXPORT void QPlainTextDocumentLayout_requestUpdate(QPlainTextDocumentLayoutH handle);

#endif
