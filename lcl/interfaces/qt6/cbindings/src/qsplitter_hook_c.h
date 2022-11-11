//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QSPLITTER_HOOK_C_H
#define QSPLITTER_HOOK_C_H

#include "qsplitter_hook.h"

C_EXPORT QSplitter_hookH QSplitter_hook_Create(QObjectH handle);
C_EXPORT void QSplitter_hook_Destroy(QSplitter_hookH handle);
C_EXPORT void QSplitter_hook_hook_splitterMoved(QSplitter_hookH handle, QHookH hook);
C_EXPORT QSplitterHandle_hookH QSplitterHandle_hook_Create(QObjectH handle);
C_EXPORT void QSplitterHandle_hook_Destroy(QSplitterHandle_hookH handle);

#endif
