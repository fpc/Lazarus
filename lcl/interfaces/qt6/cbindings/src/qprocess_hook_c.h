//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QPROCESS_HOOK_C_H
#define QPROCESS_HOOK_C_H

#include "qprocess_hook.h"

C_EXPORT QProcessEnvironment_hookH QProcessEnvironment_hook_Create(QObjectH handle);
C_EXPORT void QProcessEnvironment_hook_Destroy(QProcessEnvironment_hookH handle);
C_EXPORT QProcess_hookH QProcess_hook_Create(QObjectH handle);
C_EXPORT void QProcess_hook_Destroy(QProcess_hookH handle);
C_EXPORT void QProcess_hook_hook_started(QProcess_hookH handle, QHookH hook);
C_EXPORT void QProcess_hook_hook_finished(QProcess_hookH handle, QHookH hook);
C_EXPORT void QProcess_hook_hook_finished2(QProcess_hookH handle, QHookH hook);
C_EXPORT void QProcess_hook_hook_error(QProcess_hookH handle, QHookH hook);
C_EXPORT void QProcess_hook_hook_stateChanged(QProcess_hookH handle, QHookH hook);
C_EXPORT void QProcess_hook_hook_readyReadStandardOutput(QProcess_hookH handle, QHookH hook);
C_EXPORT void QProcess_hook_hook_readyReadStandardError(QProcess_hookH handle, QHookH hook);

#endif
