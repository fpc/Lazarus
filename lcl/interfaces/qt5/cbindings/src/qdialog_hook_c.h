//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QDIALOG_HOOK_C_H
#define QDIALOG_HOOK_C_H

#include "qdialog_hook.h"

C_EXPORT QDialog_hookH QDialog_hook_Create(QObjectH handle);
C_EXPORT void QDialog_hook_Destroy(QDialog_hookH handle);
C_EXPORT void QDialog_hook_hook_finished(QDialog_hookH handle, QHookH hook);
C_EXPORT void QDialog_hook_hook_accepted(QDialog_hookH handle, QHookH hook);
C_EXPORT void QDialog_hook_hook_rejected(QDialog_hookH handle, QHookH hook);

#endif
