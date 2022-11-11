//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QTABLEWIDGET_HOOK_C_H
#define QTABLEWIDGET_HOOK_C_H

#include "qtablewidget_hook.h"

C_EXPORT QTableWidgetSelectionRange_hookH QTableWidgetSelectionRange_hook_Create(QObjectH handle);
C_EXPORT void QTableWidgetSelectionRange_hook_Destroy(QTableWidgetSelectionRange_hookH handle);
C_EXPORT QTableWidgetItem_hookH QTableWidgetItem_hook_Create(QObjectH handle);
C_EXPORT void QTableWidgetItem_hook_Destroy(QTableWidgetItem_hookH handle);
C_EXPORT QTableWidget_hookH QTableWidget_hook_Create(QObjectH handle);
C_EXPORT void QTableWidget_hook_Destroy(QTableWidget_hookH handle);
C_EXPORT void QTableWidget_hook_hook_itemPressed(QTableWidget_hookH handle, QHookH hook);
C_EXPORT void QTableWidget_hook_hook_itemClicked(QTableWidget_hookH handle, QHookH hook);
C_EXPORT void QTableWidget_hook_hook_itemDoubleClicked(QTableWidget_hookH handle, QHookH hook);
C_EXPORT void QTableWidget_hook_hook_itemActivated(QTableWidget_hookH handle, QHookH hook);
C_EXPORT void QTableWidget_hook_hook_itemEntered(QTableWidget_hookH handle, QHookH hook);
C_EXPORT void QTableWidget_hook_hook_itemChanged(QTableWidget_hookH handle, QHookH hook);
C_EXPORT void QTableWidget_hook_hook_currentItemChanged(QTableWidget_hookH handle, QHookH hook);
C_EXPORT void QTableWidget_hook_hook_itemSelectionChanged(QTableWidget_hookH handle, QHookH hook);
C_EXPORT void QTableWidget_hook_hook_cellPressed(QTableWidget_hookH handle, QHookH hook);
C_EXPORT void QTableWidget_hook_hook_cellClicked(QTableWidget_hookH handle, QHookH hook);
C_EXPORT void QTableWidget_hook_hook_cellDoubleClicked(QTableWidget_hookH handle, QHookH hook);
C_EXPORT void QTableWidget_hook_hook_cellActivated(QTableWidget_hookH handle, QHookH hook);
C_EXPORT void QTableWidget_hook_hook_cellEntered(QTableWidget_hookH handle, QHookH hook);
C_EXPORT void QTableWidget_hook_hook_cellChanged(QTableWidget_hookH handle, QHookH hook);
C_EXPORT void QTableWidget_hook_hook_currentCellChanged(QTableWidget_hookH handle, QHookH hook);

#endif
