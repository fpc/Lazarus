//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QTREEWIDGET_HOOK_C_H
#define QTREEWIDGET_HOOK_C_H

#include "qtreewidget_hook.h"

C_EXPORT QTreeWidgetItem_hookH QTreeWidgetItem_hook_Create(QObjectH handle);
C_EXPORT void QTreeWidgetItem_hook_Destroy(QTreeWidgetItem_hookH handle);
C_EXPORT QTreeWidget_hookH QTreeWidget_hook_Create(QObjectH handle);
C_EXPORT void QTreeWidget_hook_Destroy(QTreeWidget_hookH handle);
C_EXPORT void QTreeWidget_hook_hook_itemPressed(QTreeWidget_hookH handle, QHookH hook);
C_EXPORT void QTreeWidget_hook_hook_itemClicked(QTreeWidget_hookH handle, QHookH hook);
C_EXPORT void QTreeWidget_hook_hook_itemDoubleClicked(QTreeWidget_hookH handle, QHookH hook);
C_EXPORT void QTreeWidget_hook_hook_itemActivated(QTreeWidget_hookH handle, QHookH hook);
C_EXPORT void QTreeWidget_hook_hook_itemEntered(QTreeWidget_hookH handle, QHookH hook);
C_EXPORT void QTreeWidget_hook_hook_itemChanged(QTreeWidget_hookH handle, QHookH hook);
C_EXPORT void QTreeWidget_hook_hook_itemExpanded(QTreeWidget_hookH handle, QHookH hook);
C_EXPORT void QTreeWidget_hook_hook_itemCollapsed(QTreeWidget_hookH handle, QHookH hook);
C_EXPORT void QTreeWidget_hook_hook_currentItemChanged(QTreeWidget_hookH handle, QHookH hook);
C_EXPORT void QTreeWidget_hook_hook_itemSelectionChanged(QTreeWidget_hookH handle, QHookH hook);

#endif
