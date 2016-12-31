//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QABSTRACTSCROLLAREA_HOOK_C_H
#define QABSTRACTSCROLLAREA_HOOK_C_H

#include "qabstractscrollarea_hook.h"

C_EXPORT QAbstractScrollArea_hookH QAbstractScrollArea_hook_Create(QObjectH handle);
C_EXPORT void QAbstractScrollArea_hook_Destroy(QAbstractScrollArea_hookH handle);

#endif
