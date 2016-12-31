//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QCOREAPPLICATION_HOOK_C_H
#define QCOREAPPLICATION_HOOK_C_H

#include "qcoreapplication_hook.h"

C_EXPORT QCoreApplication_hookH QCoreApplication_hook_Create(QObjectH handle);
C_EXPORT void QCoreApplication_hook_Destroy(QCoreApplication_hookH handle);
C_EXPORT void QCoreApplication_hook_hook_aboutToQuit(QCoreApplication_hookH handle, QHookH hook);
C_EXPORT void QCoreApplication_hook_hook_organizationNameChanged(QCoreApplication_hookH handle, QHookH hook);
C_EXPORT void QCoreApplication_hook_hook_organizationDomainChanged(QCoreApplication_hookH handle, QHookH hook);
C_EXPORT void QCoreApplication_hook_hook_applicationNameChanged(QCoreApplication_hookH handle, QHookH hook);
C_EXPORT void QCoreApplication_hook_hook_applicationVersionChanged(QCoreApplication_hookH handle, QHookH hook);

#endif
