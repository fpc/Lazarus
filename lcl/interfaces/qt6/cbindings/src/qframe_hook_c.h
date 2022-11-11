//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QFRAME_HOOK_C_H
#define QFRAME_HOOK_C_H

#include "qframe_hook.h"

C_EXPORT QFrame_hookH QFrame_hook_Create(QObjectH handle);
C_EXPORT void QFrame_hook_Destroy(QFrame_hookH handle);

#endif
