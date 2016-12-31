//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QDRAG_HOOK_C_H
#define QDRAG_HOOK_C_H

#include "qdrag_hook.h"

C_EXPORT QDrag_hookH QDrag_hook_Create(QObjectH handle);
C_EXPORT void QDrag_hook_Destroy(QDrag_hookH handle);
C_EXPORT void QDrag_hook_hook_actionChanged(QDrag_hookH handle, QHookH hook);
C_EXPORT void QDrag_hook_hook_targetChanged(QDrag_hookH handle, QHookH hook);

#endif
