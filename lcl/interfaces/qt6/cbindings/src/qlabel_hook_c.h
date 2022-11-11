//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QLABEL_HOOK_C_H
#define QLABEL_HOOK_C_H

#include "qlabel_hook.h"

C_EXPORT QLabel_hookH QLabel_hook_Create(QObjectH handle);
C_EXPORT void QLabel_hook_Destroy(QLabel_hookH handle);
C_EXPORT void QLabel_hook_hook_linkActivated(QLabel_hookH handle, QHookH hook);
C_EXPORT void QLabel_hook_hook_linkHovered(QLabel_hookH handle, QHookH hook);

#endif
