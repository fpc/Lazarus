//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QDOCKWIDGET_HOOK_C_H
#define QDOCKWIDGET_HOOK_C_H

#include "qdockwidget_hook.h"

C_EXPORT QDockWidget_hookH QDockWidget_hook_Create(QObjectH handle);
C_EXPORT void QDockWidget_hook_Destroy(QDockWidget_hookH handle);
C_EXPORT void QDockWidget_hook_hook_featuresChanged(QDockWidget_hookH handle, QHookH hook);
C_EXPORT void QDockWidget_hook_hook_topLevelChanged(QDockWidget_hookH handle, QHookH hook);
C_EXPORT void QDockWidget_hook_hook_allowedAreasChanged(QDockWidget_hookH handle, QHookH hook);
C_EXPORT void QDockWidget_hook_hook_visibilityChanged(QDockWidget_hookH handle, QHookH hook);
C_EXPORT void QDockWidget_hook_hook_dockLocationChanged(QDockWidget_hookH handle, QHookH hook);

#endif
