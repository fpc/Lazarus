//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QLAYOUT_HOOK_C_H
#define QLAYOUT_HOOK_C_H

#include "qlayout_hook.h"

C_EXPORT QLayout_hookH QLayout_hook_Create(QObjectH handle);
C_EXPORT void QLayout_hook_Destroy(QLayout_hookH handle);

#endif
