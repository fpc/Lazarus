//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QCLIPBOARD_HOOK_C_H
#define QCLIPBOARD_HOOK_C_H

#include "qclipboard_hook.h"

C_EXPORT QClipboard_hookH QClipboard_hook_Create(QObjectH handle);
C_EXPORT void QClipboard_hook_Destroy(QClipboard_hookH handle);
C_EXPORT void QClipboard_hook_hook_changed(QClipboard_hookH handle, QHookH hook);
C_EXPORT void QClipboard_hook_hook_selectionChanged(QClipboard_hookH handle, QHookH hook);
C_EXPORT void QClipboard_hook_hook_findBufferChanged(QClipboard_hookH handle, QHookH hook);
C_EXPORT void QClipboard_hook_hook_dataChanged(QClipboard_hookH handle, QHookH hook);

#endif
