//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QLISTWIDGET_HOOK_C_H
#define QLISTWIDGET_HOOK_C_H

#include "qlistwidget_hook.h"

C_EXPORT QListWidgetItem_hookH QListWidgetItem_hook_Create(QObjectH handle);
C_EXPORT void QListWidgetItem_hook_Destroy(QListWidgetItem_hookH handle);
C_EXPORT QListWidget_hookH QListWidget_hook_Create(QObjectH handle);
C_EXPORT void QListWidget_hook_Destroy(QListWidget_hookH handle);
C_EXPORT void QListWidget_hook_hook_itemPressed(QListWidget_hookH handle, QHookH hook);
C_EXPORT void QListWidget_hook_hook_itemClicked(QListWidget_hookH handle, QHookH hook);
C_EXPORT void QListWidget_hook_hook_itemDoubleClicked(QListWidget_hookH handle, QHookH hook);
C_EXPORT void QListWidget_hook_hook_itemActivated(QListWidget_hookH handle, QHookH hook);
C_EXPORT void QListWidget_hook_hook_itemEntered(QListWidget_hookH handle, QHookH hook);
C_EXPORT void QListWidget_hook_hook_itemChanged(QListWidget_hookH handle, QHookH hook);
C_EXPORT void QListWidget_hook_hook_currentItemChanged(QListWidget_hookH handle, QHookH hook);
C_EXPORT void QListWidget_hook_hook_currentTextChanged(QListWidget_hookH handle, QHookH hook);
C_EXPORT void QListWidget_hook_hook_currentRowChanged(QListWidget_hookH handle, QHookH hook);
C_EXPORT void QListWidget_hook_hook_itemSelectionChanged(QListWidget_hookH handle, QHookH hook);

#endif
