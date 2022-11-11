//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QITEMSELECTIONMODEL_C_H
#define QITEMSELECTIONMODEL_C_H

#include <QtCore>
#include "pascalbind.h"

C_EXPORT QItemSelectionRangeH QItemSelectionRange_Create();
C_EXPORT void QItemSelectionRange_Destroy(QItemSelectionRangeH handle);
C_EXPORT QItemSelectionRangeH QItemSelectionRange_Create2(const QItemSelectionRangeH other);
C_EXPORT QItemSelectionRangeH QItemSelectionRange_Create3(const QModelIndexH topLeft, const QModelIndexH bottomRight);
C_EXPORT QItemSelectionRangeH QItemSelectionRange_Create4(const QModelIndexH index);
C_EXPORT int QItemSelectionRange_top(QItemSelectionRangeH handle);
C_EXPORT int QItemSelectionRange_left(QItemSelectionRangeH handle);
C_EXPORT int QItemSelectionRange_bottom(QItemSelectionRangeH handle);
C_EXPORT int QItemSelectionRange_right(QItemSelectionRangeH handle);
C_EXPORT int QItemSelectionRange_width(QItemSelectionRangeH handle);
C_EXPORT int QItemSelectionRange_height(QItemSelectionRangeH handle);
C_EXPORT const QPersistentModelIndexH QItemSelectionRange_topLeft(QItemSelectionRangeH handle);
C_EXPORT const QPersistentModelIndexH QItemSelectionRange_bottomRight(QItemSelectionRangeH handle);
C_EXPORT void QItemSelectionRange_parent(QItemSelectionRangeH handle, QModelIndexH retval);
C_EXPORT const QAbstractItemModelH QItemSelectionRange_model(QItemSelectionRangeH handle);
C_EXPORT bool QItemSelectionRange_contains(QItemSelectionRangeH handle, const QModelIndexH index);
C_EXPORT bool QItemSelectionRange_contains2(QItemSelectionRangeH handle, int row, int column, const QModelIndexH parentIndex);
C_EXPORT bool QItemSelectionRange_intersects(QItemSelectionRangeH handle, const QItemSelectionRangeH other);
C_EXPORT void QItemSelectionRange_intersected(QItemSelectionRangeH handle, QItemSelectionRangeH retval, const QItemSelectionRangeH other);
C_EXPORT bool QItemSelectionRange_isValid(QItemSelectionRangeH handle);
C_EXPORT bool QItemSelectionRange_isEmpty(QItemSelectionRangeH handle);
C_EXPORT void QItemSelectionRange_indexes(QItemSelectionRangeH handle, PPtrIntArray retval);
C_EXPORT QItemSelectionModelH QItemSelectionModel_Create(QAbstractItemModelH model);
C_EXPORT void QItemSelectionModel_Destroy(QItemSelectionModelH handle);
C_EXPORT QItemSelectionModelH QItemSelectionModel_Create2(QAbstractItemModelH model, QObjectH parent);
C_EXPORT void QItemSelectionModel_currentIndex(QItemSelectionModelH handle, QModelIndexH retval);
C_EXPORT bool QItemSelectionModel_isSelected(QItemSelectionModelH handle, const QModelIndexH index);
C_EXPORT bool QItemSelectionModel_isRowSelected(QItemSelectionModelH handle, int row, const QModelIndexH parent);
C_EXPORT bool QItemSelectionModel_isColumnSelected(QItemSelectionModelH handle, int column, const QModelIndexH parent);
C_EXPORT bool QItemSelectionModel_rowIntersectsSelection(QItemSelectionModelH handle, int row, const QModelIndexH parent);
C_EXPORT bool QItemSelectionModel_columnIntersectsSelection(QItemSelectionModelH handle, int column, const QModelIndexH parent);
C_EXPORT bool QItemSelectionModel_hasSelection(QItemSelectionModelH handle);
C_EXPORT void QItemSelectionModel_selectedIndexes(QItemSelectionModelH handle, PPtrIntArray retval);
C_EXPORT void QItemSelectionModel_selectedRows(QItemSelectionModelH handle, PPtrIntArray retval, int column);
C_EXPORT void QItemSelectionModel_selectedColumns(QItemSelectionModelH handle, PPtrIntArray retval, int row);
C_EXPORT const QAbstractItemModelH QItemSelectionModel_model(QItemSelectionModelH handle);
C_EXPORT void QItemSelectionModel_setCurrentIndex(QItemSelectionModelH handle, const QModelIndexH index, unsigned int command);
C_EXPORT void QItemSelectionModel_select(QItemSelectionModelH handle, const QModelIndexH index, unsigned int command);
C_EXPORT void QItemSelectionModel_clear(QItemSelectionModelH handle);
C_EXPORT void QItemSelectionModel_reset(QItemSelectionModelH handle);
C_EXPORT void QItemSelectionModel_clearSelection(QItemSelectionModelH handle);
C_EXPORT void QItemSelectionModel_clearCurrentIndex(QItemSelectionModelH handle);

#endif
