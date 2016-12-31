//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QABSTRACTBUTTON_HOOK_C_H
#define QABSTRACTBUTTON_HOOK_C_H

#include "qabstractbutton_hook.h"

C_EXPORT QAbstractButton_hookH QAbstractButton_hook_Create(QObjectH handle);
C_EXPORT void QAbstractButton_hook_Destroy(QAbstractButton_hookH handle);
C_EXPORT void QAbstractButton_hook_hook_pressed(QAbstractButton_hookH handle, QHookH hook);
C_EXPORT void QAbstractButton_hook_hook_released(QAbstractButton_hookH handle, QHookH hook);
C_EXPORT void QAbstractButton_hook_hook_clicked(QAbstractButton_hookH handle, QHookH hook);
C_EXPORT void QAbstractButton_hook_hook_clicked2(QAbstractButton_hookH handle, QHookH hook);
C_EXPORT void QAbstractButton_hook_hook_toggled(QAbstractButton_hookH handle, QHookH hook);

#endif
