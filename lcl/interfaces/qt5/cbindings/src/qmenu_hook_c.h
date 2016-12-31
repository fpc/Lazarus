//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QMENU_HOOK_C_H
#define QMENU_HOOK_C_H

#include "qmenu_hook.h"

C_EXPORT QMenu_hookH QMenu_hook_Create(QObjectH handle);
C_EXPORT void QMenu_hook_Destroy(QMenu_hookH handle);
C_EXPORT void QMenu_hook_hook_aboutToShow(QMenu_hookH handle, QHookH hook);
C_EXPORT void QMenu_hook_hook_aboutToHide(QMenu_hookH handle, QHookH hook);
C_EXPORT void QMenu_hook_hook_triggered(QMenu_hookH handle, QHookH hook);
C_EXPORT void QMenu_hook_hook_hovered(QMenu_hookH handle, QHookH hook);

#endif
