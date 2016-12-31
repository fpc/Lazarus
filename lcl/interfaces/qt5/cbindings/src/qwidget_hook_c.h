//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QWIDGET_HOOK_C_H
#define QWIDGET_HOOK_C_H

#include "qwidget_hook.h"

C_EXPORT QWidget_hookH QWidget_hook_Create(QObjectH handle);
C_EXPORT void QWidget_hook_Destroy(QWidget_hookH handle);
C_EXPORT void QWidget_hook_hook_customContextMenuRequested(QWidget_hookH handle, QHookH hook);

#endif
