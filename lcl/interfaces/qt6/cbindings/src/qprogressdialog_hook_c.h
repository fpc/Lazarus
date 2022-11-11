//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QPROGRESSDIALOG_HOOK_C_H
#define QPROGRESSDIALOG_HOOK_C_H

#include "qprogressdialog_hook.h"

C_EXPORT QProgressDialog_hookH QProgressDialog_hook_Create(QObjectH handle);
C_EXPORT void QProgressDialog_hook_Destroy(QProgressDialog_hookH handle);
C_EXPORT void QProgressDialog_hook_hook_canceled(QProgressDialog_hookH handle, QHookH hook);

#endif
