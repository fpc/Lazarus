//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QACTION_HOOK_C_H
#define QACTION_HOOK_C_H

#include "qaction_hook.h"

C_EXPORT QAction_hookH QAction_hook_Create(QObjectH handle);
C_EXPORT void QAction_hook_Destroy(QAction_hookH handle);
C_EXPORT void QAction_hook_hook_changed(QAction_hookH handle, QHookH hook);
C_EXPORT void QAction_hook_hook_triggered(QAction_hookH handle, QHookH hook);
C_EXPORT void QAction_hook_hook_triggered2(QAction_hookH handle, QHookH hook);
C_EXPORT void QAction_hook_hook_hovered(QAction_hookH handle, QHookH hook);
C_EXPORT void QAction_hook_hook_toggled(QAction_hookH handle, QHookH hook);

#endif
