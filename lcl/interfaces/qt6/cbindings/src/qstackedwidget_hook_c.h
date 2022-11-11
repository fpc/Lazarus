//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QSTACKEDWIDGET_HOOK_C_H
#define QSTACKEDWIDGET_HOOK_C_H

#include "qstackedwidget_hook.h"

C_EXPORT QStackedWidget_hookH QStackedWidget_hook_Create(QObjectH handle);
C_EXPORT void QStackedWidget_hook_Destroy(QStackedWidget_hookH handle);
C_EXPORT void QStackedWidget_hook_hook_currentChanged(QStackedWidget_hookH handle, QHookH hook);
C_EXPORT void QStackedWidget_hook_hook_widgetRemoved(QStackedWidget_hookH handle, QHookH hook);

#endif
