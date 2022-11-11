//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qtreeview_c.h"

QTreeViewH QTreeView_Create(QWidgetH parent)
{
	return (QTreeViewH) new QTreeView((QWidget*)parent);
}

void QTreeView_Destroy(QTreeViewH handle)
{
	delete (QTreeView *)handle;
}

void QTreeView_setModel(QTreeViewH handle, QAbstractItemModelH model)
{
	((QTreeView *)handle)->setModel((QAbstractItemModel*)model);
}

void QTreeView_setRootIndex(QTreeViewH handle, const QModelIndexH index)
{
	((QTreeView *)handle)->setRootIndex(*(const QModelIndex*)index);
}

void QTreeView_setSelectionModel(QTreeViewH handle, QItemSelectionModelH selectionModel)
{
	((QTreeView *)handle)->setSelectionModel((QItemSelectionModel*)selectionModel);
}

QHeaderViewH QTreeView_header(QTreeViewH handle)
{
	return (QHeaderViewH) ((QTreeView *)handle)->header();
}

void QTreeView_setHeader(QTreeViewH handle, QHeaderViewH header)
{
	((QTreeView *)handle)->setHeader((QHeaderView*)header);
}

int QTreeView_autoExpandDelay(QTreeViewH handle)
{
	return (int) ((QTreeView *)handle)->autoExpandDelay();
}

void QTreeView_setAutoExpandDelay(QTreeViewH handle, int delay)
{
	((QTreeView *)handle)->setAutoExpandDelay(delay);
}

int QTreeView_indentation(QTreeViewH handle)
{
	return (int) ((QTreeView *)handle)->indentation();
}

void QTreeView_setIndentation(QTreeViewH handle, int i)
{
	((QTreeView *)handle)->setIndentation(i);
}

bool QTreeView_rootIsDecorated(QTreeViewH handle)
{
	return (bool) ((QTreeView *)handle)->rootIsDecorated();
}

void QTreeView_setRootIsDecorated(QTreeViewH handle, bool show)
{
	((QTreeView *)handle)->setRootIsDecorated(show);
}

bool QTreeView_uniformRowHeights(QTreeViewH handle)
{
	return (bool) ((QTreeView *)handle)->uniformRowHeights();
}

void QTreeView_setUniformRowHeights(QTreeViewH handle, bool uniform)
{
	((QTreeView *)handle)->setUniformRowHeights(uniform);
}

bool QTreeView_itemsExpandable(QTreeViewH handle)
{
	return (bool) ((QTreeView *)handle)->itemsExpandable();
}

void QTreeView_setItemsExpandable(QTreeViewH handle, bool enable)
{
	((QTreeView *)handle)->setItemsExpandable(enable);
}

bool QTreeView_expandsOnDoubleClick(QTreeViewH handle)
{
	return (bool) ((QTreeView *)handle)->expandsOnDoubleClick();
}

void QTreeView_setExpandsOnDoubleClick(QTreeViewH handle, bool enable)
{
	((QTreeView *)handle)->setExpandsOnDoubleClick(enable);
}

int QTreeView_columnViewportPosition(QTreeViewH handle, int column)
{
	return (int) ((QTreeView *)handle)->columnViewportPosition(column);
}

int QTreeView_columnWidth(QTreeViewH handle, int column)
{
	return (int) ((QTreeView *)handle)->columnWidth(column);
}

void QTreeView_setColumnWidth(QTreeViewH handle, int column, int width)
{
	((QTreeView *)handle)->setColumnWidth(column, width);
}

int QTreeView_columnAt(QTreeViewH handle, int x)
{
	return (int) ((QTreeView *)handle)->columnAt(x);
}

bool QTreeView_isColumnHidden(QTreeViewH handle, int column)
{
	return (bool) ((QTreeView *)handle)->isColumnHidden(column);
}

void QTreeView_setColumnHidden(QTreeViewH handle, int column, bool hide)
{
	((QTreeView *)handle)->setColumnHidden(column, hide);
}

bool QTreeView_isHeaderHidden(QTreeViewH handle)
{
	return (bool) ((QTreeView *)handle)->isHeaderHidden();
}

void QTreeView_setHeaderHidden(QTreeViewH handle, bool hide)
{
	((QTreeView *)handle)->setHeaderHidden(hide);
}

bool QTreeView_isRowHidden(QTreeViewH handle, int row, const QModelIndexH parent)
{
	return (bool) ((QTreeView *)handle)->isRowHidden(row, *(const QModelIndex*)parent);
}

void QTreeView_setRowHidden(QTreeViewH handle, int row, const QModelIndexH parent, bool hide)
{
	((QTreeView *)handle)->setRowHidden(row, *(const QModelIndex*)parent, hide);
}

bool QTreeView_isFirstColumnSpanned(QTreeViewH handle, int row, const QModelIndexH parent)
{
	return (bool) ((QTreeView *)handle)->isFirstColumnSpanned(row, *(const QModelIndex*)parent);
}

void QTreeView_setFirstColumnSpanned(QTreeViewH handle, int row, const QModelIndexH parent, bool span)
{
	((QTreeView *)handle)->setFirstColumnSpanned(row, *(const QModelIndex*)parent, span);
}

bool QTreeView_isExpanded(QTreeViewH handle, const QModelIndexH index)
{
	return (bool) ((QTreeView *)handle)->isExpanded(*(const QModelIndex*)index);
}

void QTreeView_setExpanded(QTreeViewH handle, const QModelIndexH index, bool expand)
{
	((QTreeView *)handle)->setExpanded(*(const QModelIndex*)index, expand);
}

void QTreeView_setSortingEnabled(QTreeViewH handle, bool enable)
{
	((QTreeView *)handle)->setSortingEnabled(enable);
}

bool QTreeView_isSortingEnabled(QTreeViewH handle)
{
	return (bool) ((QTreeView *)handle)->isSortingEnabled();
}

void QTreeView_setAnimated(QTreeViewH handle, bool enable)
{
	((QTreeView *)handle)->setAnimated(enable);
}

bool QTreeView_isAnimated(QTreeViewH handle)
{
	return (bool) ((QTreeView *)handle)->isAnimated();
}

void QTreeView_setAllColumnsShowFocus(QTreeViewH handle, bool enable)
{
	((QTreeView *)handle)->setAllColumnsShowFocus(enable);
}

bool QTreeView_allColumnsShowFocus(QTreeViewH handle)
{
	return (bool) ((QTreeView *)handle)->allColumnsShowFocus();
}

void QTreeView_setWordWrap(QTreeViewH handle, bool on)
{
	((QTreeView *)handle)->setWordWrap(on);
}

bool QTreeView_wordWrap(QTreeViewH handle)
{
	return (bool) ((QTreeView *)handle)->wordWrap();
}

void QTreeView_keyboardSearch(QTreeViewH handle, PWideString search)
{
	QString t_search;
	copyPWideStringToQString(search, t_search);
	((QTreeView *)handle)->keyboardSearch(t_search);
}

void QTreeView_visualRect(QTreeViewH handle, PRect retval, const QModelIndexH index)
{
	QRect t_retval;
	t_retval = ((QTreeView *)handle)->visualRect(*(const QModelIndex*)index);
	copyQRectToPRect(t_retval, retval);
}

void QTreeView_scrollTo(QTreeViewH handle, const QModelIndexH index, QAbstractItemView::ScrollHint hint)
{
	((QTreeView *)handle)->scrollTo(*(const QModelIndex*)index, hint);
}

void QTreeView_indexAt(QTreeViewH handle, QModelIndexH retval, const QPointH p)
{
	*(QModelIndex *)retval = ((QTreeView *)handle)->indexAt(*(const QPoint*)p);
}

void QTreeView_indexAbove(QTreeViewH handle, QModelIndexH retval, const QModelIndexH index)
{
	*(QModelIndex *)retval = ((QTreeView *)handle)->indexAbove(*(const QModelIndex*)index);
}

void QTreeView_indexBelow(QTreeViewH handle, QModelIndexH retval, const QModelIndexH index)
{
	*(QModelIndex *)retval = ((QTreeView *)handle)->indexBelow(*(const QModelIndex*)index);
}

void QTreeView_doItemsLayout(QTreeViewH handle)
{
	((QTreeView *)handle)->doItemsLayout();
}

void QTreeView_reset(QTreeViewH handle)
{
	((QTreeView *)handle)->reset();
}

void QTreeView_selectAll(QTreeViewH handle)
{
	((QTreeView *)handle)->selectAll();
}

void QTreeView_hideColumn(QTreeViewH handle, int column)
{
	((QTreeView *)handle)->hideColumn(column);
}

void QTreeView_showColumn(QTreeViewH handle, int column)
{
	((QTreeView *)handle)->showColumn(column);
}

void QTreeView_expand(QTreeViewH handle, const QModelIndexH index)
{
	((QTreeView *)handle)->expand(*(const QModelIndex*)index);
}

void QTreeView_collapse(QTreeViewH handle, const QModelIndexH index)
{
	((QTreeView *)handle)->collapse(*(const QModelIndex*)index);
}

void QTreeView_resizeColumnToContents(QTreeViewH handle, int column)
{
	((QTreeView *)handle)->resizeColumnToContents(column);
}

void QTreeView_expandAll(QTreeViewH handle)
{
	((QTreeView *)handle)->expandAll();
}

void QTreeView_collapseAll(QTreeViewH handle)
{
	((QTreeView *)handle)->collapseAll();
}

void QTreeView_expandToDepth(QTreeViewH handle, int depth)
{
	((QTreeView *)handle)->expandToDepth(depth);
}

void QTreeView_sortByColumn(QTreeViewH handle, int column, Qt::SortOrder order)
{
	((QTreeView *)handle)->sortByColumn(column, order);
}

