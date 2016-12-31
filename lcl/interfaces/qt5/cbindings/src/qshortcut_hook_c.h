//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QSHORTCUT_HOOK_C_H
#define QSHORTCUT_HOOK_C_H

#include "qshortcut_hook.h"

C_EXPORT QShortcut_hookH QShortcut_hook_Create(QObjectH handle);
C_EXPORT void QShortcut_hook_Destroy(QShortcut_hookH handle);
C_EXPORT void QShortcut_hook_hook_activated(QShortcut_hookH handle, QHookH hook);
C_EXPORT void QShortcut_hook_hook_activatedAmbiguously(QShortcut_hookH handle, QHookH hook);

#endif
