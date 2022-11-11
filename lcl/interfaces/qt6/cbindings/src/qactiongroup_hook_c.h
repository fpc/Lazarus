//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QACTIONGROUP_HOOK_C_H
#define QACTIONGROUP_HOOK_C_H

#include "qactiongroup_hook.h"

C_EXPORT QActionGroup_hookH QActionGroup_hook_Create(QObjectH handle);
C_EXPORT void QActionGroup_hook_Destroy(QActionGroup_hookH handle);
C_EXPORT void QActionGroup_hook_hook_triggered(QActionGroup_hookH handle, QHookH hook);
C_EXPORT void QActionGroup_hook_hook_hovered(QActionGroup_hookH handle, QHookH hook);

#endif
