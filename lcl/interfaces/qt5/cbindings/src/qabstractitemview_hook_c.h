//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QABSTRACTITEMVIEW_HOOK_C_H
#define QABSTRACTITEMVIEW_HOOK_C_H

#include "qabstractitemview_hook.h"

C_EXPORT QAbstractItemView_hookH QAbstractItemView_hook_Create(QObjectH handle);
C_EXPORT void QAbstractItemView_hook_Destroy(QAbstractItemView_hookH handle);
C_EXPORT void QAbstractItemView_hook_hook_pressed(QAbstractItemView_hookH handle, QHookH hook);
C_EXPORT void QAbstractItemView_hook_hook_clicked(QAbstractItemView_hookH handle, QHookH hook);
C_EXPORT void QAbstractItemView_hook_hook_doubleClicked(QAbstractItemView_hookH handle, QHookH hook);
C_EXPORT void QAbstractItemView_hook_hook_activated(QAbstractItemView_hookH handle, QHookH hook);
C_EXPORT void QAbstractItemView_hook_hook_entered(QAbstractItemView_hookH handle, QHookH hook);
C_EXPORT void QAbstractItemView_hook_hook_viewportEntered(QAbstractItemView_hookH handle, QHookH hook);

#endif
