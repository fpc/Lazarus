//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qlineedit_c.h"

QLineEditH QLineEdit_Create(QWidgetH parent)
{
	return (QLineEditH) new QLineEdit((QWidget*)parent);
}

void QLineEdit_Destroy(QLineEditH handle)
{
	delete (QLineEdit *)handle;
}

QLineEditH QLineEdit_Create2(PWideString AnonParam1, QWidgetH parent)
{
	QString t_AnonParam1;
	copyPWideStringToQString(AnonParam1, t_AnonParam1);
	return (QLineEditH) new QLineEdit(t_AnonParam1, (QWidget*)parent);
}

void QLineEdit_text(QLineEditH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QLineEdit *)handle)->text();
	copyQStringToPWideString(t_retval, retval);
}

void QLineEdit_displayText(QLineEditH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QLineEdit *)handle)->displayText();
	copyQStringToPWideString(t_retval, retval);
}

void QLineEdit_placeholderText(QLineEditH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QLineEdit *)handle)->placeholderText();
	copyQStringToPWideString(t_retval, retval);
}

void QLineEdit_setPlaceholderText(QLineEditH handle, PWideString AnonParam1)
{
	QString t_AnonParam1;
	copyPWideStringToQString(AnonParam1, t_AnonParam1);
	((QLineEdit *)handle)->setPlaceholderText(t_AnonParam1);
}

int QLineEdit_maxLength(QLineEditH handle)
{
	return (int) ((QLineEdit *)handle)->maxLength();
}

void QLineEdit_setMaxLength(QLineEditH handle, int AnonParam1)
{
	((QLineEdit *)handle)->setMaxLength(AnonParam1);
}

void QLineEdit_setFrame(QLineEditH handle, bool AnonParam1)
{
	((QLineEdit *)handle)->setFrame(AnonParam1);
}

bool QLineEdit_hasFrame(QLineEditH handle)
{
	return (bool) ((QLineEdit *)handle)->hasFrame();
}

QLineEdit::EchoMode QLineEdit_echoMode(QLineEditH handle)
{
	return (QLineEdit::EchoMode) ((QLineEdit *)handle)->echoMode();
}

void QLineEdit_setEchoMode(QLineEditH handle, QLineEdit::EchoMode AnonParam1)
{
	((QLineEdit *)handle)->setEchoMode(AnonParam1);
}

bool QLineEdit_isReadOnly(QLineEditH handle)
{
	return (bool) ((QLineEdit *)handle)->isReadOnly();
}

void QLineEdit_setReadOnly(QLineEditH handle, bool AnonParam1)
{
	((QLineEdit *)handle)->setReadOnly(AnonParam1);
}

void QLineEdit_setValidator(QLineEditH handle, const QValidatorH AnonParam1)
{
	((QLineEdit *)handle)->setValidator((const QValidator*)AnonParam1);
}

const QValidatorH QLineEdit_validator(QLineEditH handle)
{
	return (const QValidatorH) ((QLineEdit *)handle)->validator();
}

void QLineEdit_setCompleter(QLineEditH handle, QCompleterH completer)
{
	((QLineEdit *)handle)->setCompleter((QCompleter*)completer);
}

QCompleterH QLineEdit_completer(QLineEditH handle)
{
	return (QCompleterH) ((QLineEdit *)handle)->completer();
}

void QLineEdit_sizeHint(QLineEditH handle, PSize retval)
{
	*(QSize *)retval = ((QLineEdit *)handle)->sizeHint();
}

void QLineEdit_minimumSizeHint(QLineEditH handle, PSize retval)
{
	*(QSize *)retval = ((QLineEdit *)handle)->minimumSizeHint();
}

int QLineEdit_cursorPosition(QLineEditH handle)
{
	return (int) ((QLineEdit *)handle)->cursorPosition();
}

void QLineEdit_setCursorPosition(QLineEditH handle, int AnonParam1)
{
	((QLineEdit *)handle)->setCursorPosition(AnonParam1);
}

int QLineEdit_cursorPositionAt(QLineEditH handle, const QPointH pos)
{
	return (int) ((QLineEdit *)handle)->cursorPositionAt(*(const QPoint*)pos);
}

void QLineEdit_setAlignment(QLineEditH handle, unsigned int flag)
{
	((QLineEdit *)handle)->setAlignment((Qt::Alignment)flag);
}

unsigned int QLineEdit_alignment(QLineEditH handle)
{
	return (unsigned int) ((QLineEdit *)handle)->alignment();
}

void QLineEdit_cursorForward(QLineEditH handle, bool mark, int steps)
{
	((QLineEdit *)handle)->cursorForward(mark, steps);
}

void QLineEdit_cursorBackward(QLineEditH handle, bool mark, int steps)
{
	((QLineEdit *)handle)->cursorBackward(mark, steps);
}

void QLineEdit_cursorWordForward(QLineEditH handle, bool mark)
{
	((QLineEdit *)handle)->cursorWordForward(mark);
}

void QLineEdit_cursorWordBackward(QLineEditH handle, bool mark)
{
	((QLineEdit *)handle)->cursorWordBackward(mark);
}

void QLineEdit_backspace(QLineEditH handle)
{
	((QLineEdit *)handle)->backspace();
}

void QLineEdit_del(QLineEditH handle)
{
	((QLineEdit *)handle)->del();
}

void QLineEdit_home(QLineEditH handle, bool mark)
{
	((QLineEdit *)handle)->home(mark);
}

void QLineEdit_end(QLineEditH handle, bool mark)
{
	((QLineEdit *)handle)->end(mark);
}

bool QLineEdit_isModified(QLineEditH handle)
{
	return (bool) ((QLineEdit *)handle)->isModified();
}

void QLineEdit_setModified(QLineEditH handle, bool AnonParam1)
{
	((QLineEdit *)handle)->setModified(AnonParam1);
}

void QLineEdit_setSelection(QLineEditH handle, int AnonParam1, int AnonParam2)
{
	((QLineEdit *)handle)->setSelection(AnonParam1, AnonParam2);
}

bool QLineEdit_hasSelectedText(QLineEditH handle)
{
	return (bool) ((QLineEdit *)handle)->hasSelectedText();
}

void QLineEdit_selectedText(QLineEditH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QLineEdit *)handle)->selectedText();
	copyQStringToPWideString(t_retval, retval);
}

int QLineEdit_selectionEnd(QLineEditH handle)
{
	return (int) ((QLineEdit *)handle)->selectionEnd();
}

int QLineEdit_selectionLength(QLineEditH handle)
{
	return (int) ((QLineEdit *)handle)->selectionLength();
}

int QLineEdit_selectionStart(QLineEditH handle)
{
	return (int) ((QLineEdit *)handle)->selectionStart();
}

bool QLineEdit_isUndoAvailable(QLineEditH handle)
{
	return (bool) ((QLineEdit *)handle)->isUndoAvailable();
}

bool QLineEdit_isRedoAvailable(QLineEditH handle)
{
	return (bool) ((QLineEdit *)handle)->isRedoAvailable();
}

void QLineEdit_setDragEnabled(QLineEditH handle, bool b)
{
	((QLineEdit *)handle)->setDragEnabled(b);
}

bool QLineEdit_dragEnabled(QLineEditH handle)
{
	return (bool) ((QLineEdit *)handle)->dragEnabled();
}

void QLineEdit_setCursorMoveStyle(QLineEditH handle, Qt::CursorMoveStyle style)
{
	((QLineEdit *)handle)->setCursorMoveStyle(style);
}

Qt::CursorMoveStyle QLineEdit_cursorMoveStyle(QLineEditH handle)
{
	return (Qt::CursorMoveStyle) ((QLineEdit *)handle)->cursorMoveStyle();
}

void QLineEdit_inputMask(QLineEditH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QLineEdit *)handle)->inputMask();
	copyQStringToPWideString(t_retval, retval);
}

void QLineEdit_setInputMask(QLineEditH handle, PWideString inputMask)
{
	QString t_inputMask;
	copyPWideStringToQString(inputMask, t_inputMask);
	((QLineEdit *)handle)->setInputMask(t_inputMask);
}

bool QLineEdit_hasAcceptableInput(QLineEditH handle)
{
	return (bool) ((QLineEdit *)handle)->hasAcceptableInput();
}

void QLineEdit_setTextMargins(QLineEditH handle, int left, int top, int right, int bottom)
{
	((QLineEdit *)handle)->setTextMargins(left, top, right, bottom);
}

void QLineEdit_setTextMargins2(QLineEditH handle, const QMarginsH margins)
{
	((QLineEdit *)handle)->setTextMargins(*(const QMargins*)margins);
}

void QLineEdit_textMargins(QLineEditH handle, QMarginsH retval)
{
	*(QMargins *)retval = ((QLineEdit *)handle)->textMargins();
}

void QLineEdit_setText(QLineEditH handle, PWideString AnonParam1)
{
	QString t_AnonParam1;
	copyPWideStringToQString(AnonParam1, t_AnonParam1);
	((QLineEdit *)handle)->setText(t_AnonParam1);
}

void QLineEdit_clear(QLineEditH handle)
{
	((QLineEdit *)handle)->clear();
}

void QLineEdit_selectAll(QLineEditH handle)
{
	((QLineEdit *)handle)->selectAll();
}

void QLineEdit_undo(QLineEditH handle)
{
	((QLineEdit *)handle)->undo();
}

void QLineEdit_redo(QLineEditH handle)
{
	((QLineEdit *)handle)->redo();
}

void QLineEdit_cut(QLineEditH handle)
{
	((QLineEdit *)handle)->cut();
}

void QLineEdit_copy(QLineEditH handle)
{
	((QLineEdit *)handle)->copy();
}

void QLineEdit_paste(QLineEditH handle)
{
	((QLineEdit *)handle)->paste();
}

void QLineEdit_deselect(QLineEditH handle)
{
	((QLineEdit *)handle)->deselect();
}

void QLineEdit_insert(QLineEditH handle, PWideString AnonParam1)
{
	QString t_AnonParam1;
	copyPWideStringToQString(AnonParam1, t_AnonParam1);
	((QLineEdit *)handle)->insert(t_AnonParam1);
}

QMenuH QLineEdit_createStandardContextMenu(QLineEditH handle)
{
	return (QMenuH) ((QLineEdit *)handle)->createStandardContextMenu();
}

void QLineEdit_inputMethodQuery(QLineEditH handle, QVariantH retval, Qt::InputMethodQuery AnonParam1)
{
	*(QVariant *)retval = ((QLineEdit *)handle)->inputMethodQuery(AnonParam1);
}

bool QLineEdit_event(QLineEditH handle, QEventH AnonParam1)
{
	return (bool) ((QLineEdit *)handle)->event((QEvent*)AnonParam1);
}

bool QLineEdit_clearButtonEnabled(QLineEditH handle)
{
	return (bool) ((QLineEdit *)handle)->isClearButtonEnabled();
}

void QLineEdit_setClearButtonEnabled(QLineEditH handle, bool enabled)
{
  ((QLineEdit *)handle)->setClearButtonEnabled(enabled);
}

void QLineEdit_addAction(QLineEditH handle, QActionH action, QLineEdit::ActionPosition position)
{
  ((QLineEdit *)handle)->addAction((QAction *) action, position);
}

