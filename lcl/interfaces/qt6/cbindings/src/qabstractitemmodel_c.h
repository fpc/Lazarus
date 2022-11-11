//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QABSTRACTITEMMODEL_C_H
#define QABSTRACTITEMMODEL_C_H

#include <QtCore>
#include "pascalbind.h"

C_EXPORT QModelIndexH QModelIndex_Create();
C_EXPORT void QModelIndex_Destroy(QModelIndexH handle);
C_EXPORT int QModelIndex_row(QModelIndexH handle);
C_EXPORT int QModelIndex_column(QModelIndexH handle);
C_EXPORT quintptr QModelIndex_internalId(QModelIndexH handle);
C_EXPORT void* QModelIndex_internalPointer(QModelIndexH handle);
C_EXPORT void QModelIndex_parent(QModelIndexH handle, QModelIndexH retval);
C_EXPORT void QModelIndex_sibling(QModelIndexH handle, QModelIndexH retval, int row, int column);
C_EXPORT void QModelIndex_child(QModelIndexH handle, QModelIndexH retval, int row, int column);
C_EXPORT void QModelIndex_data(QModelIndexH handle, QVariantH retval, Qt::ItemDataRole role);
C_EXPORT unsigned int QModelIndex_flags(QModelIndexH handle);
C_EXPORT const QAbstractItemModelH QModelIndex_model(QModelIndexH handle);
C_EXPORT bool QModelIndex_isValid(QModelIndexH handle);
C_EXPORT QPersistentModelIndexH QPersistentModelIndex_Create();
C_EXPORT void QPersistentModelIndex_Destroy(QPersistentModelIndexH handle);
C_EXPORT QPersistentModelIndexH QPersistentModelIndex_Create2(const QModelIndexH index);
C_EXPORT QPersistentModelIndexH QPersistentModelIndex_Create3(const QPersistentModelIndexH other);
C_EXPORT void QPersistentModelIndex_swap(QPersistentModelIndexH handle, QPersistentModelIndexH other);
C_EXPORT int QPersistentModelIndex_row(QPersistentModelIndexH handle);
C_EXPORT int QPersistentModelIndex_column(QPersistentModelIndexH handle);
C_EXPORT void* QPersistentModelIndex_internalPointer(QPersistentModelIndexH handle);
C_EXPORT quintptr QPersistentModelIndex_internalId(QPersistentModelIndexH handle);
C_EXPORT void QPersistentModelIndex_parent(QPersistentModelIndexH handle, QModelIndexH retval);
C_EXPORT void QPersistentModelIndex_sibling(QPersistentModelIndexH handle, QModelIndexH retval, int row, int column);
C_EXPORT void QPersistentModelIndex_child(QPersistentModelIndexH handle, QModelIndexH retval, int row, int column);
C_EXPORT void QPersistentModelIndex_data(QPersistentModelIndexH handle, QVariantH retval, Qt::ItemDataRole role);
C_EXPORT unsigned int QPersistentModelIndex_flags(QPersistentModelIndexH handle);
C_EXPORT const QAbstractItemModelH QPersistentModelIndex_model(QPersistentModelIndexH handle);
C_EXPORT bool QPersistentModelIndex_isValid(QPersistentModelIndexH handle);
C_EXPORT bool QAbstractItemModel_hasIndex(QAbstractItemModelH handle, int row, int column, const QModelIndexH parent);
C_EXPORT void QAbstractItemModel_index(QAbstractItemModelH handle, QModelIndexH retval, int row, int column, const QModelIndexH parent);
C_EXPORT void QAbstractItemModel_parent(QAbstractItemModelH handle, QModelIndexH retval, const QModelIndexH child);
C_EXPORT void QAbstractItemModel_sibling(QAbstractItemModelH handle, QModelIndexH retval, int row, int column, const QModelIndexH idx);
C_EXPORT int QAbstractItemModel_rowCount(QAbstractItemModelH handle, const QModelIndexH parent);
C_EXPORT int QAbstractItemModel_columnCount(QAbstractItemModelH handle, const QModelIndexH parent);
C_EXPORT bool QAbstractItemModel_hasChildren(QAbstractItemModelH handle, const QModelIndexH parent);
C_EXPORT void QAbstractItemModel_data(QAbstractItemModelH handle, QVariantH retval, const QModelIndexH index, Qt::ItemDataRole role);
C_EXPORT bool QAbstractItemModel_setData(QAbstractItemModelH handle, const QModelIndexH index, const QVariantH value, Qt::ItemDataRole role);
C_EXPORT void QAbstractItemModel_headerData(QAbstractItemModelH handle, QVariantH retval, int section, Qt::Orientation orientation, Qt::ItemDataRole role);
C_EXPORT bool QAbstractItemModel_setHeaderData(QAbstractItemModelH handle, int section, Qt::Orientation orientation, const QVariantH value, Qt::ItemDataRole role);
C_EXPORT void QAbstractItemModel_mimeTypes(QAbstractItemModelH handle, QStringListH retval);
C_EXPORT bool QAbstractItemModel_canDropMimeData(QAbstractItemModelH handle, const QMimeDataH data, Qt::DropAction action, int row, int column, const QModelIndexH parent);
C_EXPORT bool QAbstractItemModel_dropMimeData(QAbstractItemModelH handle, const QMimeDataH data, Qt::DropAction action, int row, int column, const QModelIndexH parent);
C_EXPORT unsigned int QAbstractItemModel_supportedDropActions(QAbstractItemModelH handle);
C_EXPORT unsigned int QAbstractItemModel_supportedDragActions(QAbstractItemModelH handle);
C_EXPORT bool QAbstractItemModel_insertRows(QAbstractItemModelH handle, int row, int count, const QModelIndexH parent);
C_EXPORT bool QAbstractItemModel_insertColumns(QAbstractItemModelH handle, int column, int count, const QModelIndexH parent);
C_EXPORT bool QAbstractItemModel_removeRows(QAbstractItemModelH handle, int row, int count, const QModelIndexH parent);
C_EXPORT bool QAbstractItemModel_removeColumns(QAbstractItemModelH handle, int column, int count, const QModelIndexH parent);
C_EXPORT bool QAbstractItemModel_moveRows(QAbstractItemModelH handle, const QModelIndexH sourceParent, int sourceRow, int count, const QModelIndexH destinationParent, int destinationChild);
C_EXPORT bool QAbstractItemModel_moveColumns(QAbstractItemModelH handle, const QModelIndexH sourceParent, int sourceColumn, int count, const QModelIndexH destinationParent, int destinationChild);
C_EXPORT bool QAbstractItemModel_insertRow(QAbstractItemModelH handle, int row, const QModelIndexH parent);
C_EXPORT bool QAbstractItemModel_insertColumn(QAbstractItemModelH handle, int column, const QModelIndexH parent);
C_EXPORT bool QAbstractItemModel_removeRow(QAbstractItemModelH handle, int row, const QModelIndexH parent);
C_EXPORT bool QAbstractItemModel_removeColumn(QAbstractItemModelH handle, int column, const QModelIndexH parent);
C_EXPORT bool QAbstractItemModel_moveRow(QAbstractItemModelH handle, const QModelIndexH sourceParent, int sourceRow, const QModelIndexH destinationParent, int destinationChild);
C_EXPORT bool QAbstractItemModel_moveColumn(QAbstractItemModelH handle, const QModelIndexH sourceParent, int sourceColumn, const QModelIndexH destinationParent, int destinationChild);
C_EXPORT void QAbstractItemModel_fetchMore(QAbstractItemModelH handle, const QModelIndexH parent);
C_EXPORT bool QAbstractItemModel_canFetchMore(QAbstractItemModelH handle, const QModelIndexH parent);
C_EXPORT unsigned int QAbstractItemModel_flags(QAbstractItemModelH handle, const QModelIndexH index);
C_EXPORT void QAbstractItemModel_sort(QAbstractItemModelH handle, int column, Qt::SortOrder order);
C_EXPORT void QAbstractItemModel_buddy(QAbstractItemModelH handle, QModelIndexH retval, const QModelIndexH index);
C_EXPORT void QAbstractItemModel_match(QAbstractItemModelH handle, PPtrIntArray retval, const QModelIndexH start, int role, const QVariantH value, int hits, unsigned int flags);
C_EXPORT void QAbstractItemModel_span(QAbstractItemModelH handle, PSize retval, const QModelIndexH index);
C_EXPORT bool QAbstractItemModel_submit(QAbstractItemModelH handle);
C_EXPORT void QAbstractItemModel_revert(QAbstractItemModelH handle);
C_EXPORT void QAbstractTableModel_index(QAbstractTableModelH handle, QModelIndexH retval, int row, int column, const QModelIndexH parent);
C_EXPORT bool QAbstractTableModel_dropMimeData(QAbstractTableModelH handle, const QMimeDataH data, Qt::DropAction action, int row, int column, const QModelIndexH parent);
C_EXPORT unsigned int QAbstractTableModel_flags(QAbstractTableModelH handle, const QModelIndexH index);
C_EXPORT void QAbstractListModel_index(QAbstractListModelH handle, QModelIndexH retval, int row, int column, const QModelIndexH parent);
C_EXPORT bool QAbstractListModel_dropMimeData(QAbstractListModelH handle, const QMimeDataH data, Qt::DropAction action, int row, int column, const QModelIndexH parent);
C_EXPORT unsigned int QAbstractListModel_flags(QAbstractListModelH handle, const QModelIndexH index);

#endif
