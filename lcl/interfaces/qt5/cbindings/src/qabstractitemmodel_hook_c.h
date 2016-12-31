//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QABSTRACTITEMMODEL_HOOK_C_H
#define QABSTRACTITEMMODEL_HOOK_C_H

#include "qabstractitemmodel_hook.h"

C_EXPORT QModelIndex_hookH QModelIndex_hook_Create(QObjectH handle);
C_EXPORT void QModelIndex_hook_Destroy(QModelIndex_hookH handle);
C_EXPORT QPersistentModelIndex_hookH QPersistentModelIndex_hook_Create(QObjectH handle);
C_EXPORT void QPersistentModelIndex_hook_Destroy(QPersistentModelIndex_hookH handle);
C_EXPORT QAbstractItemModel_hookH QAbstractItemModel_hook_Create(QObjectH handle);
C_EXPORT void QAbstractItemModel_hook_Destroy(QAbstractItemModel_hookH handle);
C_EXPORT void QAbstractItemModel_hook_hook_dataChanged(QAbstractItemModel_hookH handle, QHookH hook);
C_EXPORT void QAbstractItemModel_hook_hook_headerDataChanged(QAbstractItemModel_hookH handle, QHookH hook);
C_EXPORT void QAbstractItemModel_hook_hook_layoutChanged(QAbstractItemModel_hookH handle, QHookH hook);
C_EXPORT void QAbstractItemModel_hook_hook_layoutAboutToBeChanged(QAbstractItemModel_hookH handle, QHookH hook);
C_EXPORT void QAbstractItemModel_hook_hook_rowsAboutToBeInserted(QAbstractItemModel_hookH handle, QHookH hook);
C_EXPORT void QAbstractItemModel_hook_hook_rowsInserted(QAbstractItemModel_hookH handle, QHookH hook);
C_EXPORT void QAbstractItemModel_hook_hook_rowsAboutToBeRemoved(QAbstractItemModel_hookH handle, QHookH hook);
C_EXPORT void QAbstractItemModel_hook_hook_rowsRemoved(QAbstractItemModel_hookH handle, QHookH hook);
C_EXPORT void QAbstractItemModel_hook_hook_columnsAboutToBeInserted(QAbstractItemModel_hookH handle, QHookH hook);
C_EXPORT void QAbstractItemModel_hook_hook_columnsInserted(QAbstractItemModel_hookH handle, QHookH hook);
C_EXPORT void QAbstractItemModel_hook_hook_columnsAboutToBeRemoved(QAbstractItemModel_hookH handle, QHookH hook);
C_EXPORT void QAbstractItemModel_hook_hook_columnsRemoved(QAbstractItemModel_hookH handle, QHookH hook);
C_EXPORT void QAbstractItemModel_hook_hook_modelAboutToBeReset(QAbstractItemModel_hookH handle, QHookH hook);
C_EXPORT void QAbstractItemModel_hook_hook_modelReset(QAbstractItemModel_hookH handle, QHookH hook);
C_EXPORT void QAbstractItemModel_hook_hook_rowsAboutToBeMoved(QAbstractItemModel_hookH handle, QHookH hook);
C_EXPORT void QAbstractItemModel_hook_hook_rowsMoved(QAbstractItemModel_hookH handle, QHookH hook);
C_EXPORT void QAbstractItemModel_hook_hook_columnsAboutToBeMoved(QAbstractItemModel_hookH handle, QHookH hook);
C_EXPORT void QAbstractItemModel_hook_hook_columnsMoved(QAbstractItemModel_hookH handle, QHookH hook);
C_EXPORT QAbstractTableModel_hookH QAbstractTableModel_hook_Create(QObjectH handle);
C_EXPORT void QAbstractTableModel_hook_Destroy(QAbstractTableModel_hookH handle);
C_EXPORT QAbstractListModel_hookH QAbstractListModel_hook_Create(QObjectH handle);
C_EXPORT void QAbstractListModel_hook_Destroy(QAbstractListModel_hookH handle);

#endif
