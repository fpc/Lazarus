//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qtableview_c.h"

QTableViewH QTableView_Create(QWidgetH parent)
{
	return (QTableViewH) new QTableView((QWidget*)parent);
}

void QTableView_Destroy(QTableViewH handle)
{
	delete (QTableView *)handle;
}

void QTableView_setModel(QTableViewH handle, QAbstractItemModelH model)
{
	((QTableView *)handle)->setModel((QAbstractItemModel*)model);
}

void QTableView_setRootIndex(QTableViewH handle, const QModelIndexH index)
{
	((QTableView *)handle)->setRootIndex(*(const QModelIndex*)index);
}

void QTableView_setSelectionModel(QTableViewH handle, QItemSelectionModelH selectionModel)
{
	((QTableView *)handle)->setSelectionModel((QItemSelectionModel*)selectionModel);
}

void QTableView_doItemsLayout(QTableViewH handle)
{
	((QTableView *)handle)->doItemsLayout();
}

QHeaderViewH QTableView_horizontalHeader(QTableViewH handle)
{
	return (QHeaderViewH) ((QTableView *)handle)->horizontalHeader();
}

QHeaderViewH QTableView_verticalHeader(QTableViewH handle)
{
	return (QHeaderViewH) ((QTableView *)handle)->verticalHeader();
}

void QTableView_setHorizontalHeader(QTableViewH handle, QHeaderViewH header)
{
	((QTableView *)handle)->setHorizontalHeader((QHeaderView*)header);
}

void QTableView_setVerticalHeader(QTableViewH handle, QHeaderViewH header)
{
	((QTableView *)handle)->setVerticalHeader((QHeaderView*)header);
}

int QTableView_rowViewportPosition(QTableViewH handle, int row)
{
	return (int) ((QTableView *)handle)->rowViewportPosition(row);
}

int QTableView_rowAt(QTableViewH handle, int y)
{
	return (int) ((QTableView *)handle)->rowAt(y);
}

void QTableView_setRowHeight(QTableViewH handle, int row, int height)
{
	((QTableView *)handle)->setRowHeight(row, height);
}

int QTableView_rowHeight(QTableViewH handle, int row)
{
	return (int) ((QTableView *)handle)->rowHeight(row);
}

int QTableView_columnViewportPosition(QTableViewH handle, int column)
{
	return (int) ((QTableView *)handle)->columnViewportPosition(column);
}

int QTableView_columnAt(QTableViewH handle, int x)
{
	return (int) ((QTableView *)handle)->columnAt(x);
}

void QTableView_setColumnWidth(QTableViewH handle, int column, int width)
{
	((QTableView *)handle)->setColumnWidth(column, width);
}

int QTableView_columnWidth(QTableViewH handle, int column)
{
	return (int) ((QTableView *)handle)->columnWidth(column);
}

bool QTableView_isRowHidden(QTableViewH handle, int row)
{
	return (bool) ((QTableView *)handle)->isRowHidden(row);
}

void QTableView_setRowHidden(QTableViewH handle, int row, bool hide)
{
	((QTableView *)handle)->setRowHidden(row, hide);
}

bool QTableView_isColumnHidden(QTableViewH handle, int column)
{
	return (bool) ((QTableView *)handle)->isColumnHidden(column);
}

void QTableView_setColumnHidden(QTableViewH handle, int column, bool hide)
{
	((QTableView *)handle)->setColumnHidden(column, hide);
}

void QTableView_setSortingEnabled(QTableViewH handle, bool enable)
{
	((QTableView *)handle)->setSortingEnabled(enable);
}

bool QTableView_isSortingEnabled(QTableViewH handle)
{
	return (bool) ((QTableView *)handle)->isSortingEnabled();
}

bool QTableView_showGrid(QTableViewH handle)
{
	return (bool) ((QTableView *)handle)->showGrid();
}

Qt::PenStyle QTableView_gridStyle(QTableViewH handle)
{
	return (Qt::PenStyle) ((QTableView *)handle)->gridStyle();
}

void QTableView_setGridStyle(QTableViewH handle, Qt::PenStyle style)
{
	((QTableView *)handle)->setGridStyle(style);
}

void QTableView_setWordWrap(QTableViewH handle, bool on)
{
	((QTableView *)handle)->setWordWrap(on);
}

bool QTableView_wordWrap(QTableViewH handle)
{
	return (bool) ((QTableView *)handle)->wordWrap();
}

void QTableView_setCornerButtonEnabled(QTableViewH handle, bool enable)
{
	((QTableView *)handle)->setCornerButtonEnabled(enable);
}

bool QTableView_isCornerButtonEnabled(QTableViewH handle)
{
	return (bool) ((QTableView *)handle)->isCornerButtonEnabled();
}

void QTableView_visualRect(QTableViewH handle, PRect retval, const QModelIndexH index)
{
	QRect t_retval;
	t_retval = ((QTableView *)handle)->visualRect(*(const QModelIndex*)index);
	copyQRectToPRect(t_retval, retval);
}

void QTableView_scrollTo(QTableViewH handle, const QModelIndexH index, QAbstractItemView::ScrollHint hint)
{
	((QTableView *)handle)->scrollTo(*(const QModelIndex*)index, hint);
}

void QTableView_indexAt(QTableViewH handle, QModelIndexH retval, const QPointH p)
{
	*(QModelIndex *)retval = ((QTableView *)handle)->indexAt(*(const QPoint*)p);
}

void QTableView_setSpan(QTableViewH handle, int row, int column, int rowSpan, int columnSpan)
{
	((QTableView *)handle)->setSpan(row, column, rowSpan, columnSpan);
}

int QTableView_rowSpan(QTableViewH handle, int row, int column)
{
	return (int) ((QTableView *)handle)->rowSpan(row, column);
}

int QTableView_columnSpan(QTableViewH handle, int row, int column)
{
	return (int) ((QTableView *)handle)->columnSpan(row, column);
}

void QTableView_clearSpans(QTableViewH handle)
{
	((QTableView *)handle)->clearSpans();
}

void QTableView_sortByColumn(QTableViewH handle, int column, Qt::SortOrder order)
{
	((QTableView *)handle)->sortByColumn(column, order);
}

void QTableView_selectRow(QTableViewH handle, int row)
{
	((QTableView *)handle)->selectRow(row);
}

void QTableView_selectColumn(QTableViewH handle, int column)
{
	((QTableView *)handle)->selectColumn(column);
}

void QTableView_hideRow(QTableViewH handle, int row)
{
	((QTableView *)handle)->hideRow(row);
}

void QTableView_hideColumn(QTableViewH handle, int column)
{
	((QTableView *)handle)->hideColumn(column);
}

void QTableView_showRow(QTableViewH handle, int row)
{
	((QTableView *)handle)->showRow(row);
}

void QTableView_showColumn(QTableViewH handle, int column)
{
	((QTableView *)handle)->showColumn(column);
}

void QTableView_resizeRowToContents(QTableViewH handle, int row)
{
	((QTableView *)handle)->resizeRowToContents(row);
}

void QTableView_resizeRowsToContents(QTableViewH handle)
{
	((QTableView *)handle)->resizeRowsToContents();
}

void QTableView_resizeColumnToContents(QTableViewH handle, int column)
{
	((QTableView *)handle)->resizeColumnToContents(column);
}

void QTableView_resizeColumnsToContents(QTableViewH handle)
{
	((QTableView *)handle)->resizeColumnsToContents();
}

void QTableView_setShowGrid(QTableViewH handle, bool show)
{
	((QTableView *)handle)->setShowGrid(show);
}

