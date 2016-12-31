//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QAPPLICATION_HOOK_C_H
#define QAPPLICATION_HOOK_C_H

#include "qapplication_hook.h"

C_EXPORT QApplication_hookH QApplication_hook_Create(QObjectH handle);
C_EXPORT void QApplication_hook_Destroy(QApplication_hookH handle);
C_EXPORT void QApplication_hook_hook_focusChanged(QApplication_hookH handle, QHookH hook);

#endif
