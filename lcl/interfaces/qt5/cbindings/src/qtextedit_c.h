//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QTEXTEDIT_C_H
#define QTEXTEDIT_C_H

#include <QtWidgets>
#include "pascalbind.h"

C_EXPORT QTextEditH QTextEdit_Create(QWidgetH parent);
C_EXPORT void QTextEdit_Destroy(QTextEditH handle);
C_EXPORT QTextEditH QTextEdit_Create2(PWideString text, QWidgetH parent);
C_EXPORT void QTextEdit_setDocument(QTextEditH handle, QTextDocumentH document);
C_EXPORT QTextDocumentH QTextEdit_document(QTextEditH handle);
C_EXPORT void QTextEdit_setTextCursor(QTextEditH handle, const QTextCursorH cursor);
C_EXPORT void QTextEdit_textCursor(QTextEditH handle, QTextCursorH retval);
C_EXPORT bool QTextEdit_isReadOnly(QTextEditH handle);
C_EXPORT void QTextEdit_setReadOnly(QTextEditH handle, bool ro);
C_EXPORT void QTextEdit_setTextInteractionFlags(QTextEditH handle, unsigned int flags);
C_EXPORT unsigned int QTextEdit_textInteractionFlags(QTextEditH handle);
C_EXPORT qreal QTextEdit_fontPointSize(QTextEditH handle);
C_EXPORT void QTextEdit_fontFamily(QTextEditH handle, PWideString retval);
C_EXPORT int QTextEdit_fontWeight(QTextEditH handle);
C_EXPORT bool QTextEdit_fontUnderline(QTextEditH handle);
C_EXPORT bool QTextEdit_fontItalic(QTextEditH handle);
C_EXPORT void QTextEdit_textColor(QTextEditH handle, PQColor retval);
C_EXPORT void QTextEdit_textBackgroundColor(QTextEditH handle, PQColor retval);
C_EXPORT void QTextEdit_currentFont(QTextEditH handle, QFontH retval);
C_EXPORT unsigned int QTextEdit_alignment(QTextEditH handle);
C_EXPORT void QTextEdit_mergeCurrentCharFormat(QTextEditH handle, const QTextCharFormatH modifier);
C_EXPORT void QTextEdit_setCurrentCharFormat(QTextEditH handle, const QTextCharFormatH format);
C_EXPORT void QTextEdit_currentCharFormat(QTextEditH handle, QTextCharFormatH retval);
C_EXPORT unsigned int QTextEdit_autoFormatting(QTextEditH handle);
C_EXPORT void QTextEdit_setAutoFormatting(QTextEditH handle, unsigned int features);
C_EXPORT bool QTextEdit_tabChangesFocus(QTextEditH handle);
C_EXPORT void QTextEdit_setTabChangesFocus(QTextEditH handle, bool b);
C_EXPORT void QTextEdit_setDocumentTitle(QTextEditH handle, PWideString title);
C_EXPORT void QTextEdit_documentTitle(QTextEditH handle, PWideString retval);
C_EXPORT bool QTextEdit_isUndoRedoEnabled(QTextEditH handle);
C_EXPORT void QTextEdit_setUndoRedoEnabled(QTextEditH handle, bool enable);
C_EXPORT QTextEdit::LineWrapMode QTextEdit_lineWrapMode(QTextEditH handle);
C_EXPORT void QTextEdit_setLineWrapMode(QTextEditH handle, QTextEdit::LineWrapMode mode);
C_EXPORT int QTextEdit_lineWrapColumnOrWidth(QTextEditH handle);
C_EXPORT void QTextEdit_setLineWrapColumnOrWidth(QTextEditH handle, int w);
C_EXPORT QTextOption::WrapMode QTextEdit_wordWrapMode(QTextEditH handle);
C_EXPORT void QTextEdit_setWordWrapMode(QTextEditH handle, QTextOption::WrapMode policy);
C_EXPORT bool QTextEdit_find(QTextEditH handle, PWideString exp, unsigned int options);
C_EXPORT void QTextEdit_toPlainText(QTextEditH handle, PWideString retval);
C_EXPORT void QTextEdit_toHtml(QTextEditH handle, PWideString retval);
C_EXPORT void QTextEdit_ensureCursorVisible(QTextEditH handle);
C_EXPORT void QTextEdit_loadResource(QTextEditH handle, QVariantH retval, int type, const QUrlH name);
C_EXPORT QMenuH QTextEdit_createStandardContextMenu(QTextEditH handle);
C_EXPORT QMenuH QTextEdit_createStandardContextMenu2(QTextEditH handle, const QPointH position);
C_EXPORT void QTextEdit_cursorForPosition(QTextEditH handle, QTextCursorH retval, const QPointH pos);
C_EXPORT void QTextEdit_cursorRect(QTextEditH handle, PRect retval, const QTextCursorH cursor);
C_EXPORT void QTextEdit_cursorRect2(QTextEditH handle, PRect retval);
C_EXPORT void QTextEdit_anchorAt(QTextEditH handle, PWideString retval, const QPointH pos);
C_EXPORT bool QTextEdit_overwriteMode(QTextEditH handle);
C_EXPORT void QTextEdit_setOverwriteMode(QTextEditH handle, bool overwrite);
C_EXPORT int QTextEdit_tabStopWidth(QTextEditH handle);
C_EXPORT void QTextEdit_setTabStopWidth(QTextEditH handle, int width);
C_EXPORT int QTextEdit_cursorWidth(QTextEditH handle);
C_EXPORT void QTextEdit_setCursorWidth(QTextEditH handle, int width);
C_EXPORT bool QTextEdit_acceptRichText(QTextEditH handle);
C_EXPORT void QTextEdit_setAcceptRichText(QTextEditH handle, bool accept);
C_EXPORT void QTextEdit_moveCursor(QTextEditH handle, QTextCursor::MoveOperation operation, QTextCursor::MoveMode mode);
C_EXPORT bool QTextEdit_canPaste(QTextEditH handle);
C_EXPORT void QTextEdit_print(QTextEditH handle, QPagedPaintDeviceH printer);
C_EXPORT void QTextEdit_inputMethodQuery(QTextEditH handle, QVariantH retval, Qt::InputMethodQuery property);
C_EXPORT void QTextEdit_setFontPointSize(QTextEditH handle, qreal s);
C_EXPORT void QTextEdit_setFontFamily(QTextEditH handle, PWideString fontFamily);
C_EXPORT void QTextEdit_setFontWeight(QTextEditH handle, int w);
C_EXPORT void QTextEdit_setFontUnderline(QTextEditH handle, bool b);
C_EXPORT void QTextEdit_setFontItalic(QTextEditH handle, bool b);
C_EXPORT void QTextEdit_setTextColor(QTextEditH handle, const QColorH c);
C_EXPORT void QTextEdit_setTextBackgroundColor(QTextEditH handle, const QColorH c);
C_EXPORT void QTextEdit_setCurrentFont(QTextEditH handle, const QFontH f);
C_EXPORT void QTextEdit_setAlignment(QTextEditH handle, unsigned int a);
C_EXPORT void QTextEdit_setPlainText(QTextEditH handle, PWideString text);
C_EXPORT void QTextEdit_setHtml(QTextEditH handle, PWideString text);
C_EXPORT void QTextEdit_setText(QTextEditH handle, PWideString text);
C_EXPORT void QTextEdit_cut(QTextEditH handle);
C_EXPORT void QTextEdit_copy(QTextEditH handle);
C_EXPORT void QTextEdit_paste(QTextEditH handle);
C_EXPORT void QTextEdit_undo(QTextEditH handle);
C_EXPORT void QTextEdit_redo(QTextEditH handle);
C_EXPORT void QTextEdit_clear(QTextEditH handle);
C_EXPORT void QTextEdit_selectAll(QTextEditH handle);
C_EXPORT void QTextEdit_insertPlainText(QTextEditH handle, PWideString text);
C_EXPORT void QTextEdit_insertHtml(QTextEditH handle, PWideString text);
C_EXPORT void QTextEdit_append(QTextEditH handle, PWideString text);
C_EXPORT void QTextEdit_scrollToAnchor(QTextEditH handle, PWideString name);
C_EXPORT void QTextEdit_zoomIn(QTextEditH handle, int range);
C_EXPORT void QTextEdit_zoomOut(QTextEditH handle, int range);
C_EXPORT void QTextEdit_placeholderText(QTextEditH handle, PWideString retval);
C_EXPORT void QTextEdit_setPlaceholderText(QTextEditH handle, PWideString text);

#endif
