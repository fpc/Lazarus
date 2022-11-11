//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************

/* TODO QCompleter class */

#ifndef QLINEEDIT_C_H
#define QLINEEDIT_C_H

#include <QtWidgets>
#include "pascalbind.h"

C_EXPORT QLineEditH QLineEdit_Create(QWidgetH parent);
C_EXPORT void QLineEdit_Destroy(QLineEditH handle);
C_EXPORT QLineEditH QLineEdit_Create2(PWideString AnonParam1, QWidgetH parent);
C_EXPORT void QLineEdit_text(QLineEditH handle, PWideString retval);
C_EXPORT void QLineEdit_displayText(QLineEditH handle, PWideString retval);
C_EXPORT void QLineEdit_placeholderText(QLineEditH handle, PWideString retval);
C_EXPORT void QLineEdit_setPlaceholderText(QLineEditH handle, PWideString AnonParam1);
C_EXPORT int QLineEdit_maxLength(QLineEditH handle);
C_EXPORT void QLineEdit_setMaxLength(QLineEditH handle, int AnonParam1);
C_EXPORT void QLineEdit_setFrame(QLineEditH handle, bool AnonParam1);
C_EXPORT bool QLineEdit_hasFrame(QLineEditH handle);
C_EXPORT QLineEdit::EchoMode QLineEdit_echoMode(QLineEditH handle);
C_EXPORT void QLineEdit_setEchoMode(QLineEditH handle, QLineEdit::EchoMode AnonParam1);
C_EXPORT bool QLineEdit_isReadOnly(QLineEditH handle);
C_EXPORT void QLineEdit_setReadOnly(QLineEditH handle, bool AnonParam1);
C_EXPORT void QLineEdit_setValidator(QLineEditH handle, const QValidatorH AnonParam1);
C_EXPORT const QValidatorH QLineEdit_validator(QLineEditH handle);
C_EXPORT void QLineEdit_setCompleter(QLineEditH handle, QCompleterH completer);
C_EXPORT QCompleterH QLineEdit_completer(QLineEditH handle);
C_EXPORT void QLineEdit_sizeHint(QLineEditH handle, PSize retval);
C_EXPORT void QLineEdit_minimumSizeHint(QLineEditH handle, PSize retval);
C_EXPORT int QLineEdit_cursorPosition(QLineEditH handle);
C_EXPORT void QLineEdit_setCursorPosition(QLineEditH handle, int AnonParam1);
C_EXPORT int QLineEdit_cursorPositionAt(QLineEditH handle, const QPointH pos);
C_EXPORT void QLineEdit_setAlignment(QLineEditH handle, unsigned int flag);
C_EXPORT unsigned int QLineEdit_alignment(QLineEditH handle);
C_EXPORT void QLineEdit_cursorForward(QLineEditH handle, bool mark, int steps);
C_EXPORT void QLineEdit_cursorBackward(QLineEditH handle, bool mark, int steps);
C_EXPORT void QLineEdit_cursorWordForward(QLineEditH handle, bool mark);
C_EXPORT void QLineEdit_cursorWordBackward(QLineEditH handle, bool mark);
C_EXPORT void QLineEdit_backspace(QLineEditH handle);
C_EXPORT void QLineEdit_del(QLineEditH handle);
C_EXPORT void QLineEdit_home(QLineEditH handle, bool mark);
C_EXPORT void QLineEdit_end(QLineEditH handle, bool mark);
C_EXPORT bool QLineEdit_isModified(QLineEditH handle);
C_EXPORT void QLineEdit_setModified(QLineEditH handle, bool AnonParam1);
C_EXPORT void QLineEdit_setSelection(QLineEditH handle, int AnonParam1, int AnonParam2);
C_EXPORT bool QLineEdit_hasSelectedText(QLineEditH handle);
C_EXPORT void QLineEdit_selectedText(QLineEditH handle, PWideString retval);
C_EXPORT int QLineEdit_selectionEnd(QLineEditH handle);
C_EXPORT int QLineEdit_selectionLength(QLineEditH handle);
C_EXPORT int QLineEdit_selectionStart(QLineEditH handle);
C_EXPORT bool QLineEdit_isUndoAvailable(QLineEditH handle);
C_EXPORT bool QLineEdit_isRedoAvailable(QLineEditH handle);
C_EXPORT void QLineEdit_setDragEnabled(QLineEditH handle, bool b);
C_EXPORT bool QLineEdit_dragEnabled(QLineEditH handle);
C_EXPORT void QLineEdit_setCursorMoveStyle(QLineEditH handle, Qt::CursorMoveStyle style);
C_EXPORT Qt::CursorMoveStyle QLineEdit_cursorMoveStyle(QLineEditH handle);
C_EXPORT void QLineEdit_inputMask(QLineEditH handle, PWideString retval);
C_EXPORT void QLineEdit_setInputMask(QLineEditH handle, PWideString inputMask);
C_EXPORT bool QLineEdit_hasAcceptableInput(QLineEditH handle);
C_EXPORT void QLineEdit_setTextMargins(QLineEditH handle, int left, int top, int right, int bottom);
C_EXPORT void QLineEdit_setTextMargins2(QLineEditH handle, const QMarginsH margins);
C_EXPORT void QLineEdit_textMargins(QLineEditH handle, QMarginsH retval);
C_EXPORT void QLineEdit_setText(QLineEditH handle, PWideString AnonParam1);
C_EXPORT void QLineEdit_clear(QLineEditH handle);
C_EXPORT void QLineEdit_selectAll(QLineEditH handle);
C_EXPORT void QLineEdit_undo(QLineEditH handle);
C_EXPORT void QLineEdit_redo(QLineEditH handle);
C_EXPORT void QLineEdit_cut(QLineEditH handle);
C_EXPORT void QLineEdit_copy(QLineEditH handle);
C_EXPORT void QLineEdit_paste(QLineEditH handle);
C_EXPORT void QLineEdit_deselect(QLineEditH handle);
C_EXPORT void QLineEdit_insert(QLineEditH handle, PWideString AnonParam1);
C_EXPORT QMenuH QLineEdit_createStandardContextMenu(QLineEditH handle);
C_EXPORT void QLineEdit_inputMethodQuery(QLineEditH handle, QVariantH retval, Qt::InputMethodQuery AnonParam1);
C_EXPORT bool QLineEdit_event(QLineEditH handle, QEventH AnonParam1);

C_EXPORT bool QLineEdit_clearButtonEnabled(QLineEditH handle);
C_EXPORT void QLineEdit_setClearButtonEnabled(QLineEditH handle, bool enabled);
C_EXPORT void QLineEdit_addAction(QLineEditH handle, QActionH action, QLineEdit::ActionPosition position);

#endif
