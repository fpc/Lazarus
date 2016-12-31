//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QCHECKBOX_HOOK_C_H
#define QCHECKBOX_HOOK_C_H

#include "qcheckbox_hook.h"

C_EXPORT QCheckBox_hookH QCheckBox_hook_Create(QObjectH handle);
C_EXPORT void QCheckBox_hook_Destroy(QCheckBox_hookH handle);
C_EXPORT void QCheckBox_hook_hook_stateChanged(QCheckBox_hookH handle, QHookH hook);

#endif
