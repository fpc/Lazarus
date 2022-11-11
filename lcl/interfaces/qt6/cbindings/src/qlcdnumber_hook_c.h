//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QLCDNUMBER_HOOK_C_H
#define QLCDNUMBER_HOOK_C_H

#include "qlcdnumber_hook.h"

C_EXPORT QLCDNumber_hookH QLCDNumber_hook_Create(QObjectH handle);
C_EXPORT void QLCDNumber_hook_Destroy(QLCDNumber_hookH handle);
C_EXPORT void QLCDNumber_hook_hook_overflow(QLCDNumber_hookH handle, QHookH hook);

#endif
