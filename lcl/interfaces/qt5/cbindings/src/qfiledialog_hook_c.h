//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QFILEDIALOG_HOOK_C_H
#define QFILEDIALOG_HOOK_C_H

#include "qfiledialog_hook.h"

C_EXPORT QFileDialog_hookH QFileDialog_hook_Create(QObjectH handle);
C_EXPORT void QFileDialog_hook_Destroy(QFileDialog_hookH handle);
C_EXPORT void QFileDialog_hook_hook_fileSelected(QFileDialog_hookH handle, QHookH hook);
C_EXPORT void QFileDialog_hook_hook_filesSelected(QFileDialog_hookH handle, QHookH hook);
C_EXPORT void QFileDialog_hook_hook_currentChanged(QFileDialog_hookH handle, QHookH hook);
C_EXPORT void QFileDialog_hook_hook_directoryEntered(QFileDialog_hookH handle, QHookH hook);
C_EXPORT void QFileDialog_hook_hook_filterSelected(QFileDialog_hookH handle, QHookH hook);

#endif
