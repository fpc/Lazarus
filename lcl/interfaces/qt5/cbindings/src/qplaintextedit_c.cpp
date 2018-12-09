//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qplaintextedit_c.h"

QPlainTextEditH QPlainTextEdit_Create(QWidgetH parent)
{
	return (QPlainTextEditH) new QPlainTextEdit((QWidget*)parent);
}

void QPlainTextEdit_Destroy(QPlainTextEditH handle)
{
	delete (QPlainTextEdit *)handle;
}

QPlainTextEditH QPlainTextEdit_Create2(PWideString text, QWidgetH parent)
{
	QString t_text;
	copyPWideStringToQString(text, t_text);
	return (QPlainTextEditH) new QPlainTextEdit(t_text, (QWidget*)parent);
}

void QPlainTextEdit_setDocument(QPlainTextEditH handle, QTextDocumentH document)
{
	((QPlainTextEdit *)handle)->setDocument((QTextDocument*)document);
}

QTextDocumentH QPlainTextEdit_document(QPlainTextEditH handle)
{
	return (QTextDocumentH) ((QPlainTextEdit *)handle)->document();
}

void QPlainTextEdit_setTextCursor(QPlainTextEditH handle, const QTextCursorH cursor)
{
	((QPlainTextEdit *)handle)->setTextCursor(*(const QTextCursor*)cursor);
}

void QPlainTextEdit_textCursor(QPlainTextEditH handle, QTextCursorH retval)
{
	*(QTextCursor *)retval = ((QPlainTextEdit *)handle)->textCursor();
}

bool QPlainTextEdit_isReadOnly(QPlainTextEditH handle)
{
	return (bool) ((QPlainTextEdit *)handle)->isReadOnly();
}

void QPlainTextEdit_setReadOnly(QPlainTextEditH handle, bool ro)
{
	((QPlainTextEdit *)handle)->setReadOnly(ro);
}

void QPlainTextEdit_setTextInteractionFlags(QPlainTextEditH handle, unsigned int flags)
{
	((QPlainTextEdit *)handle)->setTextInteractionFlags((Qt::TextInteractionFlags)flags);
}

unsigned int QPlainTextEdit_textInteractionFlags(QPlainTextEditH handle)
{
	return (unsigned int) ((QPlainTextEdit *)handle)->textInteractionFlags();
}

void QPlainTextEdit_mergeCurrentCharFormat(QPlainTextEditH handle, const QTextCharFormatH modifier)
{
	((QPlainTextEdit *)handle)->mergeCurrentCharFormat(*(const QTextCharFormat*)modifier);
}

void QPlainTextEdit_setCurrentCharFormat(QPlainTextEditH handle, const QTextCharFormatH format)
{
	((QPlainTextEdit *)handle)->setCurrentCharFormat(*(const QTextCharFormat*)format);
}

void QPlainTextEdit_currentCharFormat(QPlainTextEditH handle, QTextCharFormatH retval)
{
	*(QTextCharFormat *)retval = ((QPlainTextEdit *)handle)->currentCharFormat();
}

bool QPlainTextEdit_tabChangesFocus(QPlainTextEditH handle)
{
	return (bool) ((QPlainTextEdit *)handle)->tabChangesFocus();
}

void QPlainTextEdit_setTabChangesFocus(QPlainTextEditH handle, bool b)
{
	((QPlainTextEdit *)handle)->setTabChangesFocus(b);
}

void QPlainTextEdit_setDocumentTitle(QPlainTextEditH handle, PWideString title)
{
	QString t_title;
	copyPWideStringToQString(title, t_title);
	((QPlainTextEdit *)handle)->setDocumentTitle(t_title);
}

void QPlainTextEdit_documentTitle(QPlainTextEditH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QPlainTextEdit *)handle)->documentTitle();
	copyQStringToPWideString(t_retval, retval);
}

bool QPlainTextEdit_isUndoRedoEnabled(QPlainTextEditH handle)
{
	return (bool) ((QPlainTextEdit *)handle)->isUndoRedoEnabled();
}

void QPlainTextEdit_setUndoRedoEnabled(QPlainTextEditH handle, bool enable)
{
	((QPlainTextEdit *)handle)->setUndoRedoEnabled(enable);
}

void QPlainTextEdit_setMaximumBlockCount(QPlainTextEditH handle, int maximum)
{
	((QPlainTextEdit *)handle)->setMaximumBlockCount(maximum);
}

int QPlainTextEdit_maximumBlockCount(QPlainTextEditH handle)
{
	return (int) ((QPlainTextEdit *)handle)->maximumBlockCount();
}

QPlainTextEdit::LineWrapMode QPlainTextEdit_lineWrapMode(QPlainTextEditH handle)
{
	return (QPlainTextEdit::LineWrapMode) ((QPlainTextEdit *)handle)->lineWrapMode();
}

void QPlainTextEdit_setLineWrapMode(QPlainTextEditH handle, QPlainTextEdit::LineWrapMode mode)
{
	((QPlainTextEdit *)handle)->setLineWrapMode(mode);
}

QTextOption::WrapMode QPlainTextEdit_wordWrapMode(QPlainTextEditH handle)
{
	return (QTextOption::WrapMode) ((QPlainTextEdit *)handle)->wordWrapMode();
}

void QPlainTextEdit_setWordWrapMode(QPlainTextEditH handle, QTextOption::WrapMode policy)
{
	((QPlainTextEdit *)handle)->setWordWrapMode(policy);
}

void QPlainTextEdit_setBackgroundVisible(QPlainTextEditH handle, bool visible)
{
	((QPlainTextEdit *)handle)->setBackgroundVisible(visible);
}

bool QPlainTextEdit_backgroundVisible(QPlainTextEditH handle)
{
	return (bool) ((QPlainTextEdit *)handle)->backgroundVisible();
}

void QPlainTextEdit_setCenterOnScroll(QPlainTextEditH handle, bool enabled)
{
	((QPlainTextEdit *)handle)->setCenterOnScroll(enabled);
}

bool QPlainTextEdit_centerOnScroll(QPlainTextEditH handle)
{
	return (bool) ((QPlainTextEdit *)handle)->centerOnScroll();
}

bool QPlainTextEdit_find(QPlainTextEditH handle, PWideString exp, unsigned int options)
{
	QString t_exp;
	copyPWideStringToQString(exp, t_exp);
	return (bool) ((QPlainTextEdit *)handle)->find(t_exp, (QTextDocument::FindFlags)options);
}

void QPlainTextEdit_toPlainText(QPlainTextEditH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QPlainTextEdit *)handle)->toPlainText();
	copyQStringToPWideString(t_retval, retval);
}

void QPlainTextEdit_ensureCursorVisible(QPlainTextEditH handle)
{
	((QPlainTextEdit *)handle)->ensureCursorVisible();
}

void QPlainTextEdit_loadResource(QPlainTextEditH handle, QVariantH retval, int type, const QUrlH name)
{
	*(QVariant *)retval = ((QPlainTextEdit *)handle)->loadResource(type, *(const QUrl*)name);
}

QMenuH QPlainTextEdit_createStandardContextMenu(QPlainTextEditH handle)
{
	return (QMenuH) ((QPlainTextEdit *)handle)->createStandardContextMenu();
}

void QPlainTextEdit_cursorForPosition(QPlainTextEditH handle, QTextCursorH retval, const QPointH pos)
{
	*(QTextCursor *)retval = ((QPlainTextEdit *)handle)->cursorForPosition(*(const QPoint*)pos);
}

void QPlainTextEdit_cursorRect(QPlainTextEditH handle, PRect retval, const QTextCursorH cursor)
{
	QRect t_retval;
	t_retval = ((QPlainTextEdit *)handle)->cursorRect(*(const QTextCursor*)cursor);
	copyQRectToPRect(t_retval, retval);
}

void QPlainTextEdit_cursorRect2(QPlainTextEditH handle, PRect retval)
{
	QRect t_retval;
	t_retval = ((QPlainTextEdit *)handle)->cursorRect();
	copyQRectToPRect(t_retval, retval);
}

void QPlainTextEdit_anchorAt(QPlainTextEditH handle, PWideString retval, const QPointH pos)
{
	QString t_retval;
	t_retval = ((QPlainTextEdit *)handle)->anchorAt(*(const QPoint*)pos);
	copyQStringToPWideString(t_retval, retval);
}

bool QPlainTextEdit_overwriteMode(QPlainTextEditH handle)
{
	return (bool) ((QPlainTextEdit *)handle)->overwriteMode();
}

void QPlainTextEdit_setOverwriteMode(QPlainTextEditH handle, bool overwrite)
{
	((QPlainTextEdit *)handle)->setOverwriteMode(overwrite);
}

int QPlainTextEdit_tabStopWidth(QPlainTextEditH handle)
{
	return (int) ((QPlainTextEdit *)handle)->tabStopWidth();
}

void QPlainTextEdit_setTabStopWidth(QPlainTextEditH handle, int width)
{
	((QPlainTextEdit *)handle)->setTabStopWidth(width);
}

int QPlainTextEdit_cursorWidth(QPlainTextEditH handle)
{
	return (int) ((QPlainTextEdit *)handle)->cursorWidth();
}

void QPlainTextEdit_setCursorWidth(QPlainTextEditH handle, int width)
{
	((QPlainTextEdit *)handle)->setCursorWidth(width);
}

void QPlainTextEdit_moveCursor(QPlainTextEditH handle, QTextCursor::MoveOperation operation, QTextCursor::MoveMode mode)
{
	((QPlainTextEdit *)handle)->moveCursor(operation, mode);
}

bool QPlainTextEdit_canPaste(QPlainTextEditH handle)
{
	return (bool) ((QPlainTextEdit *)handle)->canPaste();
}

void QPlainTextEdit_print(QPlainTextEditH handle, QPagedPaintDeviceH printer)
{
	((QPlainTextEdit *)handle)->print((QPagedPaintDevice*)printer);
}

int QPlainTextEdit_blockCount(QPlainTextEditH handle)
{
	return (int) ((QPlainTextEdit *)handle)->blockCount();
}

void QPlainTextEdit_inputMethodQuery(QPlainTextEditH handle, QVariantH retval, Qt::InputMethodQuery property)
{
	*(QVariant *)retval = ((QPlainTextEdit *)handle)->inputMethodQuery(property);
}

void QPlainTextEdit_setPlainText(QPlainTextEditH handle, PWideString text)
{
	QString t_text;
	copyPWideStringToQString(text, t_text);
	((QPlainTextEdit *)handle)->setPlainText(t_text);
}

void QPlainTextEdit_cut(QPlainTextEditH handle)
{
	((QPlainTextEdit *)handle)->cut();
}

void QPlainTextEdit_copy(QPlainTextEditH handle)
{
	((QPlainTextEdit *)handle)->copy();
}

void QPlainTextEdit_paste(QPlainTextEditH handle)
{
	((QPlainTextEdit *)handle)->paste();
}

void QPlainTextEdit_undo(QPlainTextEditH handle)
{
	((QPlainTextEdit *)handle)->undo();
}

void QPlainTextEdit_redo(QPlainTextEditH handle)
{
	((QPlainTextEdit *)handle)->redo();
}

void QPlainTextEdit_clear(QPlainTextEditH handle)
{
	((QPlainTextEdit *)handle)->clear();
}

void QPlainTextEdit_selectAll(QPlainTextEditH handle)
{
	((QPlainTextEdit *)handle)->selectAll();
}

void QPlainTextEdit_insertPlainText(QPlainTextEditH handle, PWideString text)
{
	QString t_text;
	copyPWideStringToQString(text, t_text);
	((QPlainTextEdit *)handle)->insertPlainText(t_text);
}

void QPlainTextEdit_appendPlainText(QPlainTextEditH handle, PWideString text)
{
	QString t_text;
	copyPWideStringToQString(text, t_text);
	((QPlainTextEdit *)handle)->appendPlainText(t_text);
}

void QPlainTextEdit_appendHtml(QPlainTextEditH handle, PWideString html)
{
	QString t_html;
	copyPWideStringToQString(html, t_html);
	((QPlainTextEdit *)handle)->appendHtml(t_html);
}

void QPlainTextEdit_centerCursor(QPlainTextEditH handle)
{
	((QPlainTextEdit *)handle)->centerCursor();
}

void QPlainTextEdit_zoomIn(QPlainTextEditH handle, int range)
{
	((QPlainTextEdit *)handle)->zoomIn(range);
}

void QPlainTextEdit_zoomOut(QPlainTextEditH handle, int range)
{
	((QPlainTextEdit *)handle)->zoomOut(range);
}

void QPlainTextEdit_placeholderText(QPlainTextEditH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QPlainTextEdit *)handle)->placeholderText();
	copyQStringToPWideString(t_retval, retval);
}

void QPlainTextEdit_setPlaceholderText(QPlainTextEditH handle, PWideString text)
{
	QString t_text;
	copyPWideStringToQString(text, t_text);
	((QPlainTextEdit *)handle)->setPlaceholderText(t_text);
}


QPlainTextDocumentLayoutH QPlainTextDocumentLayout_Create(QTextDocumentH document)
{
	return (QPlainTextDocumentLayoutH) new QPlainTextDocumentLayout((QTextDocument*)document);
}

void QPlainTextDocumentLayout_Destroy(QPlainTextDocumentLayoutH handle)
{
	delete (QPlainTextDocumentLayout *)handle;
}

int QPlainTextDocumentLayout_hitTest(QPlainTextDocumentLayoutH handle, const QPointFH AnonParam1, Qt::HitTestAccuracy AnonParam2)
{
	return (int) ((QPlainTextDocumentLayout *)handle)->hitTest(*(const QPointF*)AnonParam1, AnonParam2);
}

int QPlainTextDocumentLayout_pageCount(QPlainTextDocumentLayoutH handle)
{
	return (int) ((QPlainTextDocumentLayout *)handle)->pageCount();
}

void QPlainTextDocumentLayout_documentSize(QPlainTextDocumentLayoutH handle, QSizeFH retval)
{
	*(QSizeF *)retval = ((QPlainTextDocumentLayout *)handle)->documentSize();
}

void QPlainTextDocumentLayout_frameBoundingRect(QPlainTextDocumentLayoutH handle, QRectFH retval, QTextFrameH AnonParam1)
{
	*(QRectF *)retval = ((QPlainTextDocumentLayout *)handle)->frameBoundingRect((QTextFrame*)AnonParam1);
}

void QPlainTextDocumentLayout_blockBoundingRect(QPlainTextDocumentLayoutH handle, QRectFH retval, const QTextBlockH block)
{
	*(QRectF *)retval = ((QPlainTextDocumentLayout *)handle)->blockBoundingRect(*(const QTextBlock*)block);
}

void QPlainTextDocumentLayout_ensureBlockLayout(QPlainTextDocumentLayoutH handle, const QTextBlockH block)
{
	((QPlainTextDocumentLayout *)handle)->ensureBlockLayout(*(const QTextBlock*)block);
}

void QPlainTextDocumentLayout_setCursorWidth(QPlainTextDocumentLayoutH handle, int width)
{
	((QPlainTextDocumentLayout *)handle)->setCursorWidth(width);
}

int QPlainTextDocumentLayout_cursorWidth(QPlainTextDocumentLayoutH handle)
{
	return (int) ((QPlainTextDocumentLayout *)handle)->cursorWidth();
}

void QPlainTextDocumentLayout_requestUpdate(QPlainTextDocumentLayoutH handle)
{
	((QPlainTextDocumentLayout *)handle)->requestUpdate();
}

