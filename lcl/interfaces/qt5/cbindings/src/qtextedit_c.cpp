//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qtextedit_c.h"

QTextEditH QTextEdit_Create(QWidgetH parent)
{
	return (QTextEditH) new QTextEdit((QWidget*)parent);
}

void QTextEdit_Destroy(QTextEditH handle)
{
	delete (QTextEdit *)handle;
}

QTextEditH QTextEdit_Create2(PWideString text, QWidgetH parent)
{
	QString t_text;
	copyPWideStringToQString(text, t_text);
	return (QTextEditH) new QTextEdit(t_text, (QWidget*)parent);
}

void QTextEdit_setDocument(QTextEditH handle, QTextDocumentH document)
{
	((QTextEdit *)handle)->setDocument((QTextDocument*)document);
}

QTextDocumentH QTextEdit_document(QTextEditH handle)
{
	return (QTextDocumentH) ((QTextEdit *)handle)->document();
}

void QTextEdit_setTextCursor(QTextEditH handle, const QTextCursorH cursor)
{
	((QTextEdit *)handle)->setTextCursor(*(const QTextCursor*)cursor);
}

void QTextEdit_textCursor(QTextEditH handle, QTextCursorH retval)
{
	*(QTextCursor *)retval = ((QTextEdit *)handle)->textCursor();
}

bool QTextEdit_isReadOnly(QTextEditH handle)
{
	return (bool) ((QTextEdit *)handle)->isReadOnly();
}

void QTextEdit_setReadOnly(QTextEditH handle, bool ro)
{
	((QTextEdit *)handle)->setReadOnly(ro);
}

void QTextEdit_setTextInteractionFlags(QTextEditH handle, unsigned int flags)
{
	((QTextEdit *)handle)->setTextInteractionFlags((Qt::TextInteractionFlags)flags);
}

unsigned int QTextEdit_textInteractionFlags(QTextEditH handle)
{
	return (unsigned int) ((QTextEdit *)handle)->textInteractionFlags();
}

qreal QTextEdit_fontPointSize(QTextEditH handle)
{
	return (qreal) ((QTextEdit *)handle)->fontPointSize();
}

void QTextEdit_fontFamily(QTextEditH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QTextEdit *)handle)->fontFamily();
	copyQStringToPWideString(t_retval, retval);
}

int QTextEdit_fontWeight(QTextEditH handle)
{
	return (int) ((QTextEdit *)handle)->fontWeight();
}

bool QTextEdit_fontUnderline(QTextEditH handle)
{
	return (bool) ((QTextEdit *)handle)->fontUnderline();
}

bool QTextEdit_fontItalic(QTextEditH handle)
{
	return (bool) ((QTextEdit *)handle)->fontItalic();
}

void QTextEdit_textColor(QTextEditH handle, PQColor retval)
{
	*(QColor *)retval = ((QTextEdit *)handle)->textColor();
}

void QTextEdit_textBackgroundColor(QTextEditH handle, PQColor retval)
{
	*(QColor *)retval = ((QTextEdit *)handle)->textBackgroundColor();
}

void QTextEdit_currentFont(QTextEditH handle, QFontH retval)
{
	*(QFont *)retval = ((QTextEdit *)handle)->currentFont();
}

unsigned int QTextEdit_alignment(QTextEditH handle)
{
	return (unsigned int) ((QTextEdit *)handle)->alignment();
}

void QTextEdit_mergeCurrentCharFormat(QTextEditH handle, const QTextCharFormatH modifier)
{
	((QTextEdit *)handle)->mergeCurrentCharFormat(*(const QTextCharFormat*)modifier);
}

void QTextEdit_setCurrentCharFormat(QTextEditH handle, const QTextCharFormatH format)
{
	((QTextEdit *)handle)->setCurrentCharFormat(*(const QTextCharFormat*)format);
}

void QTextEdit_currentCharFormat(QTextEditH handle, QTextCharFormatH retval)
{
	*(QTextCharFormat *)retval = ((QTextEdit *)handle)->currentCharFormat();
}

unsigned int QTextEdit_autoFormatting(QTextEditH handle)
{
	return (unsigned int) ((QTextEdit *)handle)->autoFormatting();
}

void QTextEdit_setAutoFormatting(QTextEditH handle, unsigned int features)
{
	((QTextEdit *)handle)->setAutoFormatting((QTextEdit::AutoFormatting)features);
}

bool QTextEdit_tabChangesFocus(QTextEditH handle)
{
	return (bool) ((QTextEdit *)handle)->tabChangesFocus();
}

void QTextEdit_setTabChangesFocus(QTextEditH handle, bool b)
{
	((QTextEdit *)handle)->setTabChangesFocus(b);
}

void QTextEdit_setDocumentTitle(QTextEditH handle, PWideString title)
{
	QString t_title;
	copyPWideStringToQString(title, t_title);
	((QTextEdit *)handle)->setDocumentTitle(t_title);
}

void QTextEdit_documentTitle(QTextEditH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QTextEdit *)handle)->documentTitle();
	copyQStringToPWideString(t_retval, retval);
}

bool QTextEdit_isUndoRedoEnabled(QTextEditH handle)
{
	return (bool) ((QTextEdit *)handle)->isUndoRedoEnabled();
}

void QTextEdit_setUndoRedoEnabled(QTextEditH handle, bool enable)
{
	((QTextEdit *)handle)->setUndoRedoEnabled(enable);
}

QTextEdit::LineWrapMode QTextEdit_lineWrapMode(QTextEditH handle)
{
	return (QTextEdit::LineWrapMode) ((QTextEdit *)handle)->lineWrapMode();
}

void QTextEdit_setLineWrapMode(QTextEditH handle, QTextEdit::LineWrapMode mode)
{
	((QTextEdit *)handle)->setLineWrapMode(mode);
}

int QTextEdit_lineWrapColumnOrWidth(QTextEditH handle)
{
	return (int) ((QTextEdit *)handle)->lineWrapColumnOrWidth();
}

void QTextEdit_setLineWrapColumnOrWidth(QTextEditH handle, int w)
{
	((QTextEdit *)handle)->setLineWrapColumnOrWidth(w);
}

QTextOption::WrapMode QTextEdit_wordWrapMode(QTextEditH handle)
{
	return (QTextOption::WrapMode) ((QTextEdit *)handle)->wordWrapMode();
}

void QTextEdit_setWordWrapMode(QTextEditH handle, QTextOption::WrapMode policy)
{
	((QTextEdit *)handle)->setWordWrapMode(policy);
}

bool QTextEdit_find(QTextEditH handle, PWideString exp, unsigned int options)
{
	QString t_exp;
	copyPWideStringToQString(exp, t_exp);
	return (bool) ((QTextEdit *)handle)->find(t_exp, (QTextDocument::FindFlags)options);
}

void QTextEdit_toPlainText(QTextEditH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QTextEdit *)handle)->toPlainText();
	copyQStringToPWideString(t_retval, retval);
}

void QTextEdit_toHtml(QTextEditH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QTextEdit *)handle)->toHtml();
	copyQStringToPWideString(t_retval, retval);
}

void QTextEdit_ensureCursorVisible(QTextEditH handle)
{
	((QTextEdit *)handle)->ensureCursorVisible();
}

void QTextEdit_loadResource(QTextEditH handle, QVariantH retval, int type, const QUrlH name)
{
	*(QVariant *)retval = ((QTextEdit *)handle)->loadResource(type, *(const QUrl*)name);
}

QMenuH QTextEdit_createStandardContextMenu(QTextEditH handle)
{
	return (QMenuH) ((QTextEdit *)handle)->createStandardContextMenu();
}

QMenuH QTextEdit_createStandardContextMenu2(QTextEditH handle, const QPointH position)
{
	return (QMenuH) ((QTextEdit *)handle)->createStandardContextMenu(*(const QPoint*)position);
}

void QTextEdit_cursorForPosition(QTextEditH handle, QTextCursorH retval, const QPointH pos)
{
	*(QTextCursor *)retval = ((QTextEdit *)handle)->cursorForPosition(*(const QPoint*)pos);
}

void QTextEdit_cursorRect(QTextEditH handle, PRect retval, const QTextCursorH cursor)
{
	QRect t_retval;
	t_retval = ((QTextEdit *)handle)->cursorRect(*(const QTextCursor*)cursor);
	copyQRectToPRect(t_retval, retval);
}

void QTextEdit_cursorRect2(QTextEditH handle, PRect retval)
{
	QRect t_retval;
	t_retval = ((QTextEdit *)handle)->cursorRect();
	copyQRectToPRect(t_retval, retval);
}

void QTextEdit_anchorAt(QTextEditH handle, PWideString retval, const QPointH pos)
{
	QString t_retval;
	t_retval = ((QTextEdit *)handle)->anchorAt(*(const QPoint*)pos);
	copyQStringToPWideString(t_retval, retval);
}

bool QTextEdit_overwriteMode(QTextEditH handle)
{
	return (bool) ((QTextEdit *)handle)->overwriteMode();
}

void QTextEdit_setOverwriteMode(QTextEditH handle, bool overwrite)
{
	((QTextEdit *)handle)->setOverwriteMode(overwrite);
}

int QTextEdit_tabStopWidth(QTextEditH handle)
{
	return (int) ((QTextEdit *)handle)->tabStopWidth();
}

void QTextEdit_setTabStopWidth(QTextEditH handle, int width)
{
	((QTextEdit *)handle)->setTabStopWidth(width);
}

int QTextEdit_cursorWidth(QTextEditH handle)
{
	return (int) ((QTextEdit *)handle)->cursorWidth();
}

void QTextEdit_setCursorWidth(QTextEditH handle, int width)
{
	((QTextEdit *)handle)->setCursorWidth(width);
}

bool QTextEdit_acceptRichText(QTextEditH handle)
{
	return (bool) ((QTextEdit *)handle)->acceptRichText();
}

void QTextEdit_setAcceptRichText(QTextEditH handle, bool accept)
{
	((QTextEdit *)handle)->setAcceptRichText(accept);
}

void QTextEdit_moveCursor(QTextEditH handle, QTextCursor::MoveOperation operation, QTextCursor::MoveMode mode)
{
	((QTextEdit *)handle)->moveCursor(operation, mode);
}

bool QTextEdit_canPaste(QTextEditH handle)
{
	return (bool) ((QTextEdit *)handle)->canPaste();
}

void QTextEdit_print(QTextEditH handle, QPagedPaintDeviceH printer)
{
	((QTextEdit *)handle)->print((QPagedPaintDevice*)printer);
}

void QTextEdit_inputMethodQuery(QTextEditH handle, QVariantH retval, Qt::InputMethodQuery property)
{
	*(QVariant *)retval = ((QTextEdit *)handle)->inputMethodQuery(property);
}

void QTextEdit_setFontPointSize(QTextEditH handle, qreal s)
{
	((QTextEdit *)handle)->setFontPointSize(s);
}

void QTextEdit_setFontFamily(QTextEditH handle, PWideString fontFamily)
{
	QString t_fontFamily;
	copyPWideStringToQString(fontFamily, t_fontFamily);
	((QTextEdit *)handle)->setFontFamily(t_fontFamily);
}

void QTextEdit_setFontWeight(QTextEditH handle, int w)
{
	((QTextEdit *)handle)->setFontWeight(w);
}

void QTextEdit_setFontUnderline(QTextEditH handle, bool b)
{
	((QTextEdit *)handle)->setFontUnderline(b);
}

void QTextEdit_setFontItalic(QTextEditH handle, bool b)
{
	((QTextEdit *)handle)->setFontItalic(b);
}

void QTextEdit_setTextColor(QTextEditH handle, const QColorH c)
{
	((QTextEdit *)handle)->setTextColor(*(const QColor*)c);
}

void QTextEdit_setTextBackgroundColor(QTextEditH handle, const QColorH c)
{
	((QTextEdit *)handle)->setTextBackgroundColor(*(const QColor*)c);
}

void QTextEdit_setCurrentFont(QTextEditH handle, const QFontH f)
{
	((QTextEdit *)handle)->setCurrentFont(*(const QFont*)f);
}

void QTextEdit_setAlignment(QTextEditH handle, unsigned int a)
{
	((QTextEdit *)handle)->setAlignment((Qt::Alignment)a);
}

void QTextEdit_setPlainText(QTextEditH handle, PWideString text)
{
	QString t_text;
	copyPWideStringToQString(text, t_text);
	((QTextEdit *)handle)->setPlainText(t_text);
}

void QTextEdit_setHtml(QTextEditH handle, PWideString text)
{
	QString t_text;
	copyPWideStringToQString(text, t_text);
	((QTextEdit *)handle)->setHtml(t_text);
}

void QTextEdit_setText(QTextEditH handle, PWideString text)
{
	QString t_text;
	copyPWideStringToQString(text, t_text);
	((QTextEdit *)handle)->setText(t_text);
}

void QTextEdit_cut(QTextEditH handle)
{
	((QTextEdit *)handle)->cut();
}

void QTextEdit_copy(QTextEditH handle)
{
	((QTextEdit *)handle)->copy();
}

void QTextEdit_paste(QTextEditH handle)
{
	((QTextEdit *)handle)->paste();
}

void QTextEdit_undo(QTextEditH handle)
{
	((QTextEdit *)handle)->undo();
}

void QTextEdit_redo(QTextEditH handle)
{
	((QTextEdit *)handle)->redo();
}

void QTextEdit_clear(QTextEditH handle)
{
	((QTextEdit *)handle)->clear();
}

void QTextEdit_selectAll(QTextEditH handle)
{
	((QTextEdit *)handle)->selectAll();
}

void QTextEdit_insertPlainText(QTextEditH handle, PWideString text)
{
	QString t_text;
	copyPWideStringToQString(text, t_text);
	((QTextEdit *)handle)->insertPlainText(t_text);
}

void QTextEdit_insertHtml(QTextEditH handle, PWideString text)
{
	QString t_text;
	copyPWideStringToQString(text, t_text);
	((QTextEdit *)handle)->insertHtml(t_text);
}

void QTextEdit_append(QTextEditH handle, PWideString text)
{
	QString t_text;
	copyPWideStringToQString(text, t_text);
	((QTextEdit *)handle)->append(t_text);
}

void QTextEdit_scrollToAnchor(QTextEditH handle, PWideString name)
{
	QString t_name;
	copyPWideStringToQString(name, t_name);
	((QTextEdit *)handle)->scrollToAnchor(t_name);
}

void QTextEdit_zoomIn(QTextEditH handle, int range)
{
	((QTextEdit *)handle)->zoomIn(range);
}

void QTextEdit_zoomOut(QTextEditH handle, int range)
{
	((QTextEdit *)handle)->zoomOut(range);
}

void QTextEdit_placeholderText(QTextEditH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QTextEdit *)handle)->placeholderText();
	copyQStringToPWideString(t_retval, retval);
}

void QTextEdit_setPlaceholderText(QTextEditH handle, PWideString text)
{
	QString t_text;
	copyPWideStringToQString(text, t_text);
	((QTextEdit *)handle)->setPlaceholderText(t_text);
}

