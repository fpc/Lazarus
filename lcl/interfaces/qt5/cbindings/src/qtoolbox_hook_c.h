//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QTOOLBOX_HOOK_C_H
#define QTOOLBOX_HOOK_C_H

#include "qtoolbox_hook.h"

C_EXPORT QToolBox_hookH QToolBox_hook_Create(QObjectH handle);
C_EXPORT void QToolBox_hook_Destroy(QToolBox_hookH handle);
C_EXPORT void QToolBox_hook_hook_currentChanged(QToolBox_hookH handle, QHookH hook);

#endif
