//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QTEXTEDIT_HOOK_C_H
#define QTEXTEDIT_HOOK_C_H

#include "qtextedit_hook.h"

C_EXPORT QTextEdit_hookH QTextEdit_hook_Create(QObjectH handle);
C_EXPORT void QTextEdit_hook_Destroy(QTextEdit_hookH handle);
C_EXPORT void QTextEdit_hook_hook_textChanged(QTextEdit_hookH handle, QHookH hook);
C_EXPORT void QTextEdit_hook_hook_undoAvailable(QTextEdit_hookH handle, QHookH hook);
C_EXPORT void QTextEdit_hook_hook_redoAvailable(QTextEdit_hookH handle, QHookH hook);
C_EXPORT void QTextEdit_hook_hook_currentCharFormatChanged(QTextEdit_hookH handle, QHookH hook);
C_EXPORT void QTextEdit_hook_hook_copyAvailable(QTextEdit_hookH handle, QHookH hook);
C_EXPORT void QTextEdit_hook_hook_selectionChanged(QTextEdit_hookH handle, QHookH hook);
C_EXPORT void QTextEdit_hook_hook_cursorPositionChanged(QTextEdit_hookH handle, QHookH hook);

#endif
