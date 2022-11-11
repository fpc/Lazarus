//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QSYSTEMTRAYICON_HOOK_C_H
#define QSYSTEMTRAYICON_HOOK_C_H

#include "qsystemtrayicon_hook.h"

C_EXPORT QSystemTrayIcon_hookH QSystemTrayIcon_hook_Create(QObjectH handle);
C_EXPORT void QSystemTrayIcon_hook_Destroy(QSystemTrayIcon_hookH handle);
C_EXPORT void QSystemTrayIcon_hook_hook_activated(QSystemTrayIcon_hookH handle, QHookH hook);
C_EXPORT void QSystemTrayIcon_hook_hook_messageClicked(QSystemTrayIcon_hookH handle, QHookH hook);

#endif
