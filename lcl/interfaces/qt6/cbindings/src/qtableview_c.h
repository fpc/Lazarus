//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QTABLEVIEW_C_H
#define QTABLEVIEW_C_H

#include <QtWidgets>
#include "pascalbind.h"

C_EXPORT QTableViewH QTableView_Create(QWidgetH parent);
C_EXPORT void QTableView_Destroy(QTableViewH handle);
C_EXPORT void QTableView_setModel(QTableViewH handle, QAbstractItemModelH model);
C_EXPORT void QTableView_setRootIndex(QTableViewH handle, const QModelIndexH index);
C_EXPORT void QTableView_setSelectionModel(QTableViewH handle, QItemSelectionModelH selectionModel);
C_EXPORT void QTableView_doItemsLayout(QTableViewH handle);
C_EXPORT QHeaderViewH QTableView_horizontalHeader(QTableViewH handle);
C_EXPORT QHeaderViewH QTableView_verticalHeader(QTableViewH handle);
C_EXPORT void QTableView_setHorizontalHeader(QTableViewH handle, QHeaderViewH header);
C_EXPORT void QTableView_setVerticalHeader(QTableViewH handle, QHeaderViewH header);
C_EXPORT int QTableView_rowViewportPosition(QTableViewH handle, int row);
C_EXPORT int QTableView_rowAt(QTableViewH handle, int y);
C_EXPORT void QTableView_setRowHeight(QTableViewH handle, int row, int height);
C_EXPORT int QTableView_rowHeight(QTableViewH handle, int row);
C_EXPORT int QTableView_columnViewportPosition(QTableViewH handle, int column);
C_EXPORT int QTableView_columnAt(QTableViewH handle, int x);
C_EXPORT void QTableView_setColumnWidth(QTableViewH handle, int column, int width);
C_EXPORT int QTableView_columnWidth(QTableViewH handle, int column);
C_EXPORT bool QTableView_isRowHidden(QTableViewH handle, int row);
C_EXPORT void QTableView_setRowHidden(QTableViewH handle, int row, bool hide);
C_EXPORT bool QTableView_isColumnHidden(QTableViewH handle, int column);
C_EXPORT void QTableView_setColumnHidden(QTableViewH handle, int column, bool hide);
C_EXPORT void QTableView_setSortingEnabled(QTableViewH handle, bool enable);
C_EXPORT bool QTableView_isSortingEnabled(QTableViewH handle);
C_EXPORT bool QTableView_showGrid(QTableViewH handle);
C_EXPORT Qt::PenStyle QTableView_gridStyle(QTableViewH handle);
C_EXPORT void QTableView_setGridStyle(QTableViewH handle, Qt::PenStyle style);
C_EXPORT void QTableView_setWordWrap(QTableViewH handle, bool on);
C_EXPORT bool QTableView_wordWrap(QTableViewH handle);
C_EXPORT void QTableView_setCornerButtonEnabled(QTableViewH handle, bool enable);
C_EXPORT bool QTableView_isCornerButtonEnabled(QTableViewH handle);
C_EXPORT void QTableView_visualRect(QTableViewH handle, PRect retval, const QModelIndexH index);
C_EXPORT void QTableView_scrollTo(QTableViewH handle, const QModelIndexH index, QAbstractItemView::ScrollHint hint);
C_EXPORT void QTableView_indexAt(QTableViewH handle, QModelIndexH retval, const QPointH p);
C_EXPORT void QTableView_setSpan(QTableViewH handle, int row, int column, int rowSpan, int columnSpan);
C_EXPORT int QTableView_rowSpan(QTableViewH handle, int row, int column);
C_EXPORT int QTableView_columnSpan(QTableViewH handle, int row, int column);
C_EXPORT void QTableView_clearSpans(QTableViewH handle);
C_EXPORT void QTableView_sortByColumn(QTableViewH handle, int column, Qt::SortOrder order);
C_EXPORT void QTableView_selectRow(QTableViewH handle, int row);
C_EXPORT void QTableView_selectColumn(QTableViewH handle, int column);
C_EXPORT void QTableView_hideRow(QTableViewH handle, int row);
C_EXPORT void QTableView_hideColumn(QTableViewH handle, int column);
C_EXPORT void QTableView_showRow(QTableViewH handle, int row);
C_EXPORT void QTableView_showColumn(QTableViewH handle, int column);
C_EXPORT void QTableView_resizeRowToContents(QTableViewH handle, int row);
C_EXPORT void QTableView_resizeRowsToContents(QTableViewH handle);
C_EXPORT void QTableView_resizeColumnToContents(QTableViewH handle, int column);
C_EXPORT void QTableView_resizeColumnsToContents(QTableViewH handle);
C_EXPORT void QTableView_setShowGrid(QTableViewH handle, bool show);

#endif
