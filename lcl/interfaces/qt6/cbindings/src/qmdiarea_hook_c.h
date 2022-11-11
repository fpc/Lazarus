//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QMDIAREA_HOOK_C_H
#define QMDIAREA_HOOK_C_H

#include "qmdiarea_hook.h"

C_EXPORT QMdiArea_hookH QMdiArea_hook_Create(QObjectH handle);
C_EXPORT void QMdiArea_hook_Destroy(QMdiArea_hookH handle);
C_EXPORT void QMdiArea_hook_hook_subWindowActivated(QMdiArea_hookH handle, QHookH hook);

#endif
