//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QLINEEDIT_HOOK_C_H
#define QLINEEDIT_HOOK_C_H

#include "qlineedit_hook.h"

C_EXPORT QLineEdit_hookH QLineEdit_hook_Create(QObjectH handle);
C_EXPORT void QLineEdit_hook_Destroy(QLineEdit_hookH handle);
C_EXPORT void QLineEdit_hook_hook_textChanged(QLineEdit_hookH handle, QHookH hook);
C_EXPORT void QLineEdit_hook_hook_textEdited(QLineEdit_hookH handle, QHookH hook);
C_EXPORT void QLineEdit_hook_hook_cursorPositionChanged(QLineEdit_hookH handle, QHookH hook);
C_EXPORT void QLineEdit_hook_hook_returnPressed(QLineEdit_hookH handle, QHookH hook);
C_EXPORT void QLineEdit_hook_hook_inputRejected(QLineEdit_hookH handle, QHookH hook);
C_EXPORT void QLineEdit_hook_hook_editingFinished(QLineEdit_hookH handle, QHookH hook);
C_EXPORT void QLineEdit_hook_hook_selectionChanged(QLineEdit_hookH handle, QHookH hook);

#endif
