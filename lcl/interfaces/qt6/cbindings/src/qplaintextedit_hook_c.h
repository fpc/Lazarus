//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QPLAINTEXTEDIT_HOOK_C_H
#define QPLAINTEXTEDIT_HOOK_C_H

#include "qplaintextedit_hook.h"

C_EXPORT QPlainTextEdit_hookH QPlainTextEdit_hook_Create(QObjectH handle);
C_EXPORT void QPlainTextEdit_hook_Destroy(QPlainTextEdit_hookH handle);
C_EXPORT void QPlainTextEdit_hook_hook_textChanged(QPlainTextEdit_hookH handle, QHookH hook);
C_EXPORT void QPlainTextEdit_hook_hook_undoAvailable(QPlainTextEdit_hookH handle, QHookH hook);
C_EXPORT void QPlainTextEdit_hook_hook_redoAvailable(QPlainTextEdit_hookH handle, QHookH hook);
C_EXPORT void QPlainTextEdit_hook_hook_copyAvailable(QPlainTextEdit_hookH handle, QHookH hook);
C_EXPORT void QPlainTextEdit_hook_hook_selectionChanged(QPlainTextEdit_hookH handle, QHookH hook);
C_EXPORT void QPlainTextEdit_hook_hook_cursorPositionChanged(QPlainTextEdit_hookH handle, QHookH hook);
C_EXPORT void QPlainTextEdit_hook_hook_updateRequest(QPlainTextEdit_hookH handle, QHookH hook);
C_EXPORT void QPlainTextEdit_hook_hook_blockCountChanged(QPlainTextEdit_hookH handle, QHookH hook);
C_EXPORT void QPlainTextEdit_hook_hook_modificationChanged(QPlainTextEdit_hookH handle, QHookH hook);
C_EXPORT QPlainTextDocumentLayout_hookH QPlainTextDocumentLayout_hook_Create(QObjectH handle);
C_EXPORT void QPlainTextDocumentLayout_hook_Destroy(QPlainTextDocumentLayout_hookH handle);

#endif
