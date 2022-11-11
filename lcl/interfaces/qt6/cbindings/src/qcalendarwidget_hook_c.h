//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QCALENDARWIDGET_HOOK_C_H
#define QCALENDARWIDGET_HOOK_C_H

#include "qcalendarwidget_hook.h"

C_EXPORT QCalendarWidget_hookH QCalendarWidget_hook_Create(QObjectH handle);
C_EXPORT void QCalendarWidget_hook_Destroy(QCalendarWidget_hookH handle);
C_EXPORT void QCalendarWidget_hook_hook_selectionChanged(QCalendarWidget_hookH handle, QHookH hook);
C_EXPORT void QCalendarWidget_hook_hook_clicked(QCalendarWidget_hookH handle, QHookH hook);
C_EXPORT void QCalendarWidget_hook_hook_activated(QCalendarWidget_hookH handle, QHookH hook);
C_EXPORT void QCalendarWidget_hook_hook_currentPageChanged(QCalendarWidget_hookH handle, QHookH hook);

#endif
