//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QSCROLLBAR_HOOK_C_H
#define QSCROLLBAR_HOOK_C_H

#include "qscrollbar_hook.h"

C_EXPORT QScrollBar_hookH QScrollBar_hook_Create(QObjectH handle);
C_EXPORT void QScrollBar_hook_Destroy(QScrollBar_hookH handle);

#endif
