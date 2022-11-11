//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qabstractitemmodel_c.h"

QModelIndexH QModelIndex_Create()
{
	return (QModelIndexH) new QModelIndex();
}

void QModelIndex_Destroy(QModelIndexH handle)
{
	delete (QModelIndex *)handle;
}

int QModelIndex_row(QModelIndexH handle)
{
	return (int) ((QModelIndex *)handle)->row();
}

int QModelIndex_column(QModelIndexH handle)
{
	return (int) ((QModelIndex *)handle)->column();
}

quintptr QModelIndex_internalId(QModelIndexH handle)
{
	return (quintptr) ((QModelIndex *)handle)->internalId();
}

void* QModelIndex_internalPointer(QModelIndexH handle)
{
	return (void*) ((QModelIndex *)handle)->internalPointer();
}

void QModelIndex_parent(QModelIndexH handle, QModelIndexH retval)
{
	*(QModelIndex *)retval = ((QModelIndex *)handle)->parent();
}

void QModelIndex_sibling(QModelIndexH handle, QModelIndexH retval, int row, int column)
{
	*(QModelIndex *)retval = ((QModelIndex *)handle)->sibling(row, column);
}

void QModelIndex_child(QModelIndexH handle, QModelIndexH retval, int row, int column)
{
  /* TODO: check howto retrieve parent for 3rd param */
	*(QModelIndex *)retval = ((QModelIndex *)handle)->model()->index(row, column);
}

void QModelIndex_data(QModelIndexH handle, QVariantH retval, Qt::ItemDataRole role)
{
	*(QVariant *)retval = ((QModelIndex *)handle)->data(role);
}

unsigned int QModelIndex_flags(QModelIndexH handle)
{
	return (unsigned int) ((QModelIndex *)handle)->flags();
}

const QAbstractItemModelH QModelIndex_model(QModelIndexH handle)
{
	return (const QAbstractItemModelH) ((QModelIndex *)handle)->model();
}

bool QModelIndex_isValid(QModelIndexH handle)
{
	return (bool) ((QModelIndex *)handle)->isValid();
}

QPersistentModelIndexH QPersistentModelIndex_Create()
{
	return (QPersistentModelIndexH) new QPersistentModelIndex();
}

void QPersistentModelIndex_Destroy(QPersistentModelIndexH handle)
{
	delete (QPersistentModelIndex *)handle;
}

QPersistentModelIndexH QPersistentModelIndex_Create2(const QModelIndexH index)
{
	return (QPersistentModelIndexH) new QPersistentModelIndex(*(const QModelIndex*)index);
}

QPersistentModelIndexH QPersistentModelIndex_Create3(const QPersistentModelIndexH other)
{
	return (QPersistentModelIndexH) new QPersistentModelIndex(*(const QPersistentModelIndex*)other);
}

void QPersistentModelIndex_swap(QPersistentModelIndexH handle, QPersistentModelIndexH other)
{
	((QPersistentModelIndex *)handle)->swap(*(QPersistentModelIndex*)other);
}

int QPersistentModelIndex_row(QPersistentModelIndexH handle)
{
	return (int) ((QPersistentModelIndex *)handle)->row();
}

int QPersistentModelIndex_column(QPersistentModelIndexH handle)
{
	return (int) ((QPersistentModelIndex *)handle)->column();
}

void* QPersistentModelIndex_internalPointer(QPersistentModelIndexH handle)
{
	return (void*) ((QPersistentModelIndex *)handle)->internalPointer();
}

quintptr QPersistentModelIndex_internalId(QPersistentModelIndexH handle)
{
	return (quintptr) ((QPersistentModelIndex *)handle)->internalId();
}

void QPersistentModelIndex_parent(QPersistentModelIndexH handle, QModelIndexH retval)
{
	*(QModelIndex *)retval = ((QPersistentModelIndex *)handle)->parent();
}

void QPersistentModelIndex_sibling(QPersistentModelIndexH handle, QModelIndexH retval, int row, int column)
{
	*(QModelIndex *)retval = ((QPersistentModelIndex *)handle)->sibling(row, column);
}

void QPersistentModelIndex_child(QPersistentModelIndexH handle, QModelIndexH retval, int row, int column)
{
  /* TODO: check howto retrieve parent for 3rd param */
	*(QModelIndex *)retval = ((QPersistentModelIndex *)handle)->model()->index(row, column);
}

void QPersistentModelIndex_data(QPersistentModelIndexH handle, QVariantH retval, Qt::ItemDataRole role)
{
	*(QVariant *)retval = ((QPersistentModelIndex *)handle)->data(role);
}

unsigned int QPersistentModelIndex_flags(QPersistentModelIndexH handle)
{
	return (unsigned int) ((QPersistentModelIndex *)handle)->flags();
}

const QAbstractItemModelH QPersistentModelIndex_model(QPersistentModelIndexH handle)
{
	return (const QAbstractItemModelH) ((QPersistentModelIndex *)handle)->model();
}

bool QPersistentModelIndex_isValid(QPersistentModelIndexH handle)
{
	return (bool) ((QPersistentModelIndex *)handle)->isValid();
}

bool QAbstractItemModel_hasIndex(QAbstractItemModelH handle, int row, int column, const QModelIndexH parent)
{
	return (bool) ((QAbstractItemModel *)handle)->hasIndex(row, column, *(const QModelIndex*)parent);
}

void QAbstractItemModel_index(QAbstractItemModelH handle, QModelIndexH retval, int row, int column, const QModelIndexH parent)
{
	*(QModelIndex *)retval = ((QAbstractItemModel *)handle)->index(row, column, *(const QModelIndex*)parent);
}

void QAbstractItemModel_parent(QAbstractItemModelH handle, QModelIndexH retval, const QModelIndexH child)
{
	*(QModelIndex *)retval = ((QAbstractItemModel *)handle)->parent(*(const QModelIndex*)child);
}

void QAbstractItemModel_sibling(QAbstractItemModelH handle, QModelIndexH retval, int row, int column, const QModelIndexH idx)
{
	*(QModelIndex *)retval = ((QAbstractItemModel *)handle)->sibling(row, column, *(const QModelIndex*)idx);
}

int QAbstractItemModel_rowCount(QAbstractItemModelH handle, const QModelIndexH parent)
{
	return (int) ((QAbstractItemModel *)handle)->rowCount(*(const QModelIndex*)parent);
}

int QAbstractItemModel_columnCount(QAbstractItemModelH handle, const QModelIndexH parent)
{
	return (int) ((QAbstractItemModel *)handle)->columnCount(*(const QModelIndex*)parent);
}

bool QAbstractItemModel_hasChildren(QAbstractItemModelH handle, const QModelIndexH parent)
{
	return (bool) ((QAbstractItemModel *)handle)->hasChildren(*(const QModelIndex*)parent);
}

void QAbstractItemModel_data(QAbstractItemModelH handle, QVariantH retval, const QModelIndexH index, Qt::ItemDataRole role)
{
	*(QVariant *)retval = ((QAbstractItemModel *)handle)->data(*(const QModelIndex*)index, role);
}

bool QAbstractItemModel_setData(QAbstractItemModelH handle, const QModelIndexH index, const QVariantH value, Qt::ItemDataRole role)
{
	return (bool) ((QAbstractItemModel *)handle)->setData(*(const QModelIndex*)index, *(const QVariant*)value, role);
}

void QAbstractItemModel_headerData(QAbstractItemModelH handle, QVariantH retval, int section, Qt::Orientation orientation, Qt::ItemDataRole role)
{
	*(QVariant *)retval = ((QAbstractItemModel *)handle)->headerData(section, orientation, role);
}

bool QAbstractItemModel_setHeaderData(QAbstractItemModelH handle, int section, Qt::Orientation orientation, const QVariantH value, Qt::ItemDataRole role)
{
	return (bool) ((QAbstractItemModel *)handle)->setHeaderData(section, orientation, *(const QVariant*)value, role);
}

void QAbstractItemModel_mimeTypes(QAbstractItemModelH handle, QStringListH retval)
{
	*(QStringList *)retval = ((QAbstractItemModel *)handle)->mimeTypes();
}

bool QAbstractItemModel_canDropMimeData(QAbstractItemModelH handle, const QMimeDataH data, Qt::DropAction action, int row, int column, const QModelIndexH parent)
{
	return (bool) ((QAbstractItemModel *)handle)->canDropMimeData((const QMimeData*)data, action, row, column, *(const QModelIndex*)parent);
}

bool QAbstractItemModel_dropMimeData(QAbstractItemModelH handle, const QMimeDataH data, Qt::DropAction action, int row, int column, const QModelIndexH parent)
{
	return (bool) ((QAbstractItemModel *)handle)->dropMimeData((const QMimeData*)data, action, row, column, *(const QModelIndex*)parent);
}

unsigned int QAbstractItemModel_supportedDropActions(QAbstractItemModelH handle)
{
	return (unsigned int) ((QAbstractItemModel *)handle)->supportedDropActions();
}

unsigned int QAbstractItemModel_supportedDragActions(QAbstractItemModelH handle)
{
	return (unsigned int) ((QAbstractItemModel *)handle)->supportedDragActions();
}

bool QAbstractItemModel_insertRows(QAbstractItemModelH handle, int row, int count, const QModelIndexH parent)
{
	return (bool) ((QAbstractItemModel *)handle)->insertRows(row, count, *(const QModelIndex*)parent);
}

bool QAbstractItemModel_insertColumns(QAbstractItemModelH handle, int column, int count, const QModelIndexH parent)
{
	return (bool) ((QAbstractItemModel *)handle)->insertColumns(column, count, *(const QModelIndex*)parent);
}

bool QAbstractItemModel_removeRows(QAbstractItemModelH handle, int row, int count, const QModelIndexH parent)
{
	return (bool) ((QAbstractItemModel *)handle)->removeRows(row, count, *(const QModelIndex*)parent);
}

bool QAbstractItemModel_removeColumns(QAbstractItemModelH handle, int column, int count, const QModelIndexH parent)
{
	return (bool) ((QAbstractItemModel *)handle)->removeColumns(column, count, *(const QModelIndex*)parent);
}

bool QAbstractItemModel_moveRows(QAbstractItemModelH handle, const QModelIndexH sourceParent, int sourceRow, int count, const QModelIndexH destinationParent, int destinationChild)
{
	return (bool) ((QAbstractItemModel *)handle)->moveRows(*(const QModelIndex*)sourceParent, sourceRow, count, *(const QModelIndex*)destinationParent, destinationChild);
}

bool QAbstractItemModel_moveColumns(QAbstractItemModelH handle, const QModelIndexH sourceParent, int sourceColumn, int count, const QModelIndexH destinationParent, int destinationChild)
{
	return (bool) ((QAbstractItemModel *)handle)->moveColumns(*(const QModelIndex*)sourceParent, sourceColumn, count, *(const QModelIndex*)destinationParent, destinationChild);
}

bool QAbstractItemModel_insertRow(QAbstractItemModelH handle, int row, const QModelIndexH parent)
{
	return (bool) ((QAbstractItemModel *)handle)->insertRow(row, *(const QModelIndex*)parent);
}

bool QAbstractItemModel_insertColumn(QAbstractItemModelH handle, int column, const QModelIndexH parent)
{
	return (bool) ((QAbstractItemModel *)handle)->insertColumn(column, *(const QModelIndex*)parent);
}

bool QAbstractItemModel_removeRow(QAbstractItemModelH handle, int row, const QModelIndexH parent)
{
	return (bool) ((QAbstractItemModel *)handle)->removeRow(row, *(const QModelIndex*)parent);
}

bool QAbstractItemModel_removeColumn(QAbstractItemModelH handle, int column, const QModelIndexH parent)
{
	return (bool) ((QAbstractItemModel *)handle)->removeColumn(column, *(const QModelIndex*)parent);
}

bool QAbstractItemModel_moveRow(QAbstractItemModelH handle, const QModelIndexH sourceParent, int sourceRow, const QModelIndexH destinationParent, int destinationChild)
{
	return (bool) ((QAbstractItemModel *)handle)->moveRow(*(const QModelIndex*)sourceParent, sourceRow, *(const QModelIndex*)destinationParent, destinationChild);
}

bool QAbstractItemModel_moveColumn(QAbstractItemModelH handle, const QModelIndexH sourceParent, int sourceColumn, const QModelIndexH destinationParent, int destinationChild)
{
	return (bool) ((QAbstractItemModel *)handle)->moveColumn(*(const QModelIndex*)sourceParent, sourceColumn, *(const QModelIndex*)destinationParent, destinationChild);
}

void QAbstractItemModel_fetchMore(QAbstractItemModelH handle, const QModelIndexH parent)
{
	((QAbstractItemModel *)handle)->fetchMore(*(const QModelIndex*)parent);
}

bool QAbstractItemModel_canFetchMore(QAbstractItemModelH handle, const QModelIndexH parent)
{
	return (bool) ((QAbstractItemModel *)handle)->canFetchMore(*(const QModelIndex*)parent);
}

unsigned int QAbstractItemModel_flags(QAbstractItemModelH handle, const QModelIndexH index)
{
	return (unsigned int) ((QAbstractItemModel *)handle)->flags(*(const QModelIndex*)index);
}

void QAbstractItemModel_sort(QAbstractItemModelH handle, int column, Qt::SortOrder order)
{
	((QAbstractItemModel *)handle)->sort(column, order);
}

void QAbstractItemModel_buddy(QAbstractItemModelH handle, QModelIndexH retval, const QModelIndexH index)
{
	*(QModelIndex *)retval = ((QAbstractItemModel *)handle)->buddy(*(const QModelIndex*)index);
}

void QAbstractItemModel_match(QAbstractItemModelH handle, PPtrIntArray retval, const QModelIndexH start, int role, const QVariantH value, int hits, unsigned int flags)
{
	QList<QModelIndex> t_retval;
	t_retval = ((QAbstractItemModel *)handle)->match(*(const QModelIndex*)start, role, *(const QVariant*)value, hits, (Qt::MatchFlags)flags);
	copyQListTemplateToPtrIntArrayWithNew(t_retval, retval);
}

void QAbstractItemModel_span(QAbstractItemModelH handle, PSize retval, const QModelIndexH index)
{
	*(QSize *)retval = ((QAbstractItemModel *)handle)->span(*(const QModelIndex*)index);
}

bool QAbstractItemModel_submit(QAbstractItemModelH handle)
{
	return (bool) ((QAbstractItemModel *)handle)->submit();
}

void QAbstractItemModel_revert(QAbstractItemModelH handle)
{
	((QAbstractItemModel *)handle)->revert();
}

void QAbstractTableModel_index(QAbstractTableModelH handle, QModelIndexH retval, int row, int column, const QModelIndexH parent)
{
	*(QModelIndex *)retval = ((QAbstractTableModel *)handle)->index(row, column, *(const QModelIndex*)parent);
}

bool QAbstractTableModel_dropMimeData(QAbstractTableModelH handle, const QMimeDataH data, Qt::DropAction action, int row, int column, const QModelIndexH parent)
{
	return (bool) ((QAbstractTableModel *)handle)->dropMimeData((const QMimeData*)data, action, row, column, *(const QModelIndex*)parent);
}

unsigned int QAbstractTableModel_flags(QAbstractTableModelH handle, const QModelIndexH index)
{
	return (unsigned int) ((QAbstractTableModel *)handle)->flags(*(const QModelIndex*)index);
}

void QAbstractListModel_index(QAbstractListModelH handle, QModelIndexH retval, int row, int column, const QModelIndexH parent)
{
	*(QModelIndex *)retval = ((QAbstractListModel *)handle)->index(row, column, *(const QModelIndex*)parent);
}

bool QAbstractListModel_dropMimeData(QAbstractListModelH handle, const QMimeDataH data, Qt::DropAction action, int row, int column, const QModelIndexH parent)
{
	return (bool) ((QAbstractListModel *)handle)->dropMimeData((const QMimeData*)data, action, row, column, *(const QModelIndex*)parent);
}

unsigned int QAbstractListModel_flags(QAbstractListModelH handle, const QModelIndexH index)
{
	return (unsigned int) ((QAbstractListModel *)handle)->flags(*(const QModelIndex*)index);
}

