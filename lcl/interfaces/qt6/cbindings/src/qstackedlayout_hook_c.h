//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QSTACKEDLAYOUT_HOOK_C_H
#define QSTACKEDLAYOUT_HOOK_C_H

#include "qstackedlayout_hook.h"

C_EXPORT QStackedLayout_hookH QStackedLayout_hook_Create(QObjectH handle);
C_EXPORT void QStackedLayout_hook_Destroy(QStackedLayout_hookH handle);
C_EXPORT void QStackedLayout_hook_hook_widgetRemoved(QStackedLayout_hookH handle, QHookH hook);
C_EXPORT void QStackedLayout_hook_hook_currentChanged(QStackedLayout_hookH handle, QHookH hook);

#endif
