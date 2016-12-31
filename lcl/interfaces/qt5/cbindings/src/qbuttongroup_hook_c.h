//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QBUTTONGROUP_HOOK_C_H
#define QBUTTONGROUP_HOOK_C_H

#include "qbuttongroup_hook.h"

C_EXPORT QButtonGroup_hookH QButtonGroup_hook_Create(QObjectH handle);
C_EXPORT void QButtonGroup_hook_Destroy(QButtonGroup_hookH handle);
C_EXPORT void QButtonGroup_hook_hook_buttonClicked(QButtonGroup_hookH handle, QHookH hook);
C_EXPORT void QButtonGroup_hook_hook_buttonClicked2(QButtonGroup_hookH handle, QHookH hook);
C_EXPORT void QButtonGroup_hook_hook_buttonPressed(QButtonGroup_hookH handle, QHookH hook);
C_EXPORT void QButtonGroup_hook_hook_buttonPressed2(QButtonGroup_hookH handle, QHookH hook);
C_EXPORT void QButtonGroup_hook_hook_buttonReleased(QButtonGroup_hookH handle, QHookH hook);
C_EXPORT void QButtonGroup_hook_hook_buttonReleased2(QButtonGroup_hookH handle, QHookH hook);

#endif
