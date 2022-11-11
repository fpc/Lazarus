//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QMENUBAR_HOOK_C_H
#define QMENUBAR_HOOK_C_H

#include "qmenubar_hook.h"

C_EXPORT QMenuBar_hookH QMenuBar_hook_Create(QObjectH handle);
C_EXPORT void QMenuBar_hook_Destroy(QMenuBar_hookH handle);
C_EXPORT void QMenuBar_hook_hook_triggered(QMenuBar_hookH handle, QHookH hook);
C_EXPORT void QMenuBar_hook_hook_hovered(QMenuBar_hookH handle, QHookH hook);

#endif
