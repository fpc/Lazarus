//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QPROGRESSBAR_HOOK_C_H
#define QPROGRESSBAR_HOOK_C_H

#include "qprogressbar_hook.h"

C_EXPORT QProgressBar_hookH QProgressBar_hook_Create(QObjectH handle);
C_EXPORT void QProgressBar_hook_Destroy(QProgressBar_hookH handle);
C_EXPORT void QProgressBar_hook_hook_valueChanged(QProgressBar_hookH handle, QHookH hook);

#endif
