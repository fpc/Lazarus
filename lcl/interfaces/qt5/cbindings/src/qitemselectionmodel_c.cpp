//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qitemselectionmodel_c.h"

QItemSelectionRangeH QItemSelectionRange_Create()
{
	return (QItemSelectionRangeH) new QItemSelectionRange();
}

void QItemSelectionRange_Destroy(QItemSelectionRangeH handle)
{
	delete (QItemSelectionRange *)handle;
}

QItemSelectionRangeH QItemSelectionRange_Create2(const QItemSelectionRangeH other)
{
	return (QItemSelectionRangeH) new QItemSelectionRange(*(const QItemSelectionRange*)other);
}

QItemSelectionRangeH QItemSelectionRange_Create3(const QModelIndexH topLeft, const QModelIndexH bottomRight)
{
	return (QItemSelectionRangeH) new QItemSelectionRange(*(const QModelIndex*)topLeft, *(const QModelIndex*)bottomRight);
}

QItemSelectionRangeH QItemSelectionRange_Create4(const QModelIndexH index)
{
	return (QItemSelectionRangeH) new QItemSelectionRange(*(const QModelIndex*)index);
}

int QItemSelectionRange_top(QItemSelectionRangeH handle)
{
	return (int) ((QItemSelectionRange *)handle)->top();
}

int QItemSelectionRange_left(QItemSelectionRangeH handle)
{
	return (int) ((QItemSelectionRange *)handle)->left();
}

int QItemSelectionRange_bottom(QItemSelectionRangeH handle)
{
	return (int) ((QItemSelectionRange *)handle)->bottom();
}

int QItemSelectionRange_right(QItemSelectionRangeH handle)
{
	return (int) ((QItemSelectionRange *)handle)->right();
}

int QItemSelectionRange_width(QItemSelectionRangeH handle)
{
	return (int) ((QItemSelectionRange *)handle)->width();
}

int QItemSelectionRange_height(QItemSelectionRangeH handle)
{
	return (int) ((QItemSelectionRange *)handle)->height();
}

const QPersistentModelIndexH QItemSelectionRange_topLeft(QItemSelectionRangeH handle)
{
	return (const QPersistentModelIndexH) &((QItemSelectionRange *)handle)->topLeft();
}

const QPersistentModelIndexH QItemSelectionRange_bottomRight(QItemSelectionRangeH handle)
{
	return (const QPersistentModelIndexH) &((QItemSelectionRange *)handle)->bottomRight();
}

void QItemSelectionRange_parent(QItemSelectionRangeH handle, QModelIndexH retval)
{
	*(QModelIndex *)retval = ((QItemSelectionRange *)handle)->parent();
}

const QAbstractItemModelH QItemSelectionRange_model(QItemSelectionRangeH handle)
{
	return (const QAbstractItemModelH) ((QItemSelectionRange *)handle)->model();
}

bool QItemSelectionRange_contains(QItemSelectionRangeH handle, const QModelIndexH index)
{
	return (bool) ((QItemSelectionRange *)handle)->contains(*(const QModelIndex*)index);
}

bool QItemSelectionRange_contains2(QItemSelectionRangeH handle, int row, int column, const QModelIndexH parentIndex)
{
	return (bool) ((QItemSelectionRange *)handle)->contains(row, column, *(const QModelIndex*)parentIndex);
}

bool QItemSelectionRange_intersects(QItemSelectionRangeH handle, const QItemSelectionRangeH other)
{
	return (bool) ((QItemSelectionRange *)handle)->intersects(*(const QItemSelectionRange*)other);
}

void QItemSelectionRange_intersected(QItemSelectionRangeH handle, QItemSelectionRangeH retval, const QItemSelectionRangeH other)
{
	*(QItemSelectionRange *)retval = ((QItemSelectionRange *)handle)->intersected(*(const QItemSelectionRange*)other);
}

bool QItemSelectionRange_isValid(QItemSelectionRangeH handle)
{
	return (bool) ((QItemSelectionRange *)handle)->isValid();
}

bool QItemSelectionRange_isEmpty(QItemSelectionRangeH handle)
{
	return (bool) ((QItemSelectionRange *)handle)->isEmpty();
}

void QItemSelectionRange_indexes(QItemSelectionRangeH handle, PPtrIntArray retval)
{
	QList<QModelIndex> t_retval;
	t_retval = ((QItemSelectionRange *)handle)->indexes();
	copyQListTemplateToPtrIntArrayWithNew(t_retval, retval);
}

QItemSelectionModelH QItemSelectionModel_Create(QAbstractItemModelH model)
{
	return (QItemSelectionModelH) new QItemSelectionModel((QAbstractItemModel*)model);
}

void QItemSelectionModel_Destroy(QItemSelectionModelH handle)
{
	delete (QItemSelectionModel *)handle;
}

QItemSelectionModelH QItemSelectionModel_Create2(QAbstractItemModelH model, QObjectH parent)
{
	return (QItemSelectionModelH) new QItemSelectionModel((QAbstractItemModel*)model, (QObject*)parent);
}

void QItemSelectionModel_currentIndex(QItemSelectionModelH handle, QModelIndexH retval)
{
	*(QModelIndex *)retval = ((QItemSelectionModel *)handle)->currentIndex();
}

bool QItemSelectionModel_isSelected(QItemSelectionModelH handle, const QModelIndexH index)
{
	return (bool) ((QItemSelectionModel *)handle)->isSelected(*(const QModelIndex*)index);
}

bool QItemSelectionModel_isRowSelected(QItemSelectionModelH handle, int row, const QModelIndexH parent)
{
	return (bool) ((QItemSelectionModel *)handle)->isRowSelected(row, *(const QModelIndex*)parent);
}

bool QItemSelectionModel_isColumnSelected(QItemSelectionModelH handle, int column, const QModelIndexH parent)
{
	return (bool) ((QItemSelectionModel *)handle)->isColumnSelected(column, *(const QModelIndex*)parent);
}

bool QItemSelectionModel_rowIntersectsSelection(QItemSelectionModelH handle, int row, const QModelIndexH parent)
{
	return (bool) ((QItemSelectionModel *)handle)->rowIntersectsSelection(row, *(const QModelIndex*)parent);
}

bool QItemSelectionModel_columnIntersectsSelection(QItemSelectionModelH handle, int column, const QModelIndexH parent)
{
	return (bool) ((QItemSelectionModel *)handle)->columnIntersectsSelection(column, *(const QModelIndex*)parent);
}

bool QItemSelectionModel_hasSelection(QItemSelectionModelH handle)
{
	return (bool) ((QItemSelectionModel *)handle)->hasSelection();
}

void QItemSelectionModel_selectedIndexes(QItemSelectionModelH handle, PPtrIntArray retval)
{
	QList<QModelIndex> t_retval;
	t_retval = ((QItemSelectionModel *)handle)->selectedIndexes();
	copyQListTemplateToPtrIntArrayWithNew(t_retval, retval);
}

void QItemSelectionModel_selectedRows(QItemSelectionModelH handle, PPtrIntArray retval, int column)
{
	QList<QModelIndex> t_retval;
	t_retval = ((QItemSelectionModel *)handle)->selectedRows(column);
	copyQListTemplateToPtrIntArrayWithNew(t_retval, retval);
}

void QItemSelectionModel_selectedColumns(QItemSelectionModelH handle, PPtrIntArray retval, int row)
{
	QList<QModelIndex> t_retval;
	t_retval = ((QItemSelectionModel *)handle)->selectedColumns(row);
	copyQListTemplateToPtrIntArrayWithNew(t_retval, retval);
}

const QAbstractItemModelH QItemSelectionModel_model(QItemSelectionModelH handle)
{
	return (const QAbstractItemModelH) ((QItemSelectionModel *)handle)->model();
}

void QItemSelectionModel_setCurrentIndex(QItemSelectionModelH handle, const QModelIndexH index, unsigned int command)
{
	((QItemSelectionModel *)handle)->setCurrentIndex(*(const QModelIndex*)index, (QItemSelectionModel::SelectionFlags)command);
}

void QItemSelectionModel_select(QItemSelectionModelH handle, const QModelIndexH index, unsigned int command)
{
	((QItemSelectionModel *)handle)->select(*(const QModelIndex*)index, (QItemSelectionModel::SelectionFlags)command);
}

void QItemSelectionModel_clear(QItemSelectionModelH handle)
{
	((QItemSelectionModel *)handle)->clear();
}

void QItemSelectionModel_reset(QItemSelectionModelH handle)
{
	((QItemSelectionModel *)handle)->reset();
}

void QItemSelectionModel_clearSelection(QItemSelectionModelH handle)
{
	((QItemSelectionModel *)handle)->clearSelection();
}

void QItemSelectionModel_clearCurrentIndex(QItemSelectionModelH handle)
{
	((QItemSelectionModel *)handle)->clearCurrentIndex();
}

