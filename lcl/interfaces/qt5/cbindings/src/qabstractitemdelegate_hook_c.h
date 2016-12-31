//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QABSTRACTITEMDELEGATE_HOOK_C_H
#define QABSTRACTITEMDELEGATE_HOOK_C_H

#include "qabstractitemdelegate_hook.h"

C_EXPORT QAbstractItemDelegate_hookH QAbstractItemDelegate_hook_Create(QObjectH handle);
C_EXPORT void QAbstractItemDelegate_hook_Destroy(QAbstractItemDelegate_hookH handle);
C_EXPORT void QAbstractItemDelegate_hook_hook_commitData(QAbstractItemDelegate_hookH handle, QHookH hook);
C_EXPORT void QAbstractItemDelegate_hook_hook_closeEditor(QAbstractItemDelegate_hookH handle, QHookH hook);
C_EXPORT void QAbstractItemDelegate_hook_hook_closeEditor2(QAbstractItemDelegate_hookH handle, QHookH hook);
C_EXPORT void QAbstractItemDelegate_hook_hook_sizeHintChanged(QAbstractItemDelegate_hookH handle, QHookH hook);

#endif
