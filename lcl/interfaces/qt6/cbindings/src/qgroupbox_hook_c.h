//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QGROUPBOX_HOOK_C_H
#define QGROUPBOX_HOOK_C_H

#include "qgroupbox_hook.h"

C_EXPORT QGroupBox_hookH QGroupBox_hook_Create(QObjectH handle);
C_EXPORT void QGroupBox_hook_Destroy(QGroupBox_hookH handle);
C_EXPORT void QGroupBox_hook_hook_clicked(QGroupBox_hookH handle, QHookH hook);
C_EXPORT void QGroupBox_hook_hook_clicked2(QGroupBox_hookH handle, QHookH hook);
C_EXPORT void QGroupBox_hook_hook_toggled(QGroupBox_hookH handle, QHookH hook);

#endif
