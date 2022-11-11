//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QTREEVIEW_C_H
#define QTREEVIEW_C_H

#include <QtWidgets>
#include "pascalbind.h"

C_EXPORT QTreeViewH QTreeView_Create(QWidgetH parent);
C_EXPORT void QTreeView_Destroy(QTreeViewH handle);
C_EXPORT void QTreeView_setModel(QTreeViewH handle, QAbstractItemModelH model);
C_EXPORT void QTreeView_setRootIndex(QTreeViewH handle, const QModelIndexH index);
C_EXPORT void QTreeView_setSelectionModel(QTreeViewH handle, QItemSelectionModelH selectionModel);
C_EXPORT QHeaderViewH QTreeView_header(QTreeViewH handle);
C_EXPORT void QTreeView_setHeader(QTreeViewH handle, QHeaderViewH header);
C_EXPORT int QTreeView_autoExpandDelay(QTreeViewH handle);
C_EXPORT void QTreeView_setAutoExpandDelay(QTreeViewH handle, int delay);
C_EXPORT int QTreeView_indentation(QTreeViewH handle);
C_EXPORT void QTreeView_setIndentation(QTreeViewH handle, int i);
C_EXPORT bool QTreeView_rootIsDecorated(QTreeViewH handle);
C_EXPORT void QTreeView_setRootIsDecorated(QTreeViewH handle, bool show);
C_EXPORT bool QTreeView_uniformRowHeights(QTreeViewH handle);
C_EXPORT void QTreeView_setUniformRowHeights(QTreeViewH handle, bool uniform);
C_EXPORT bool QTreeView_itemsExpandable(QTreeViewH handle);
C_EXPORT void QTreeView_setItemsExpandable(QTreeViewH handle, bool enable);
C_EXPORT bool QTreeView_expandsOnDoubleClick(QTreeViewH handle);
C_EXPORT void QTreeView_setExpandsOnDoubleClick(QTreeViewH handle, bool enable);
C_EXPORT int QTreeView_columnViewportPosition(QTreeViewH handle, int column);
C_EXPORT int QTreeView_columnWidth(QTreeViewH handle, int column);
C_EXPORT void QTreeView_setColumnWidth(QTreeViewH handle, int column, int width);
C_EXPORT int QTreeView_columnAt(QTreeViewH handle, int x);
C_EXPORT bool QTreeView_isColumnHidden(QTreeViewH handle, int column);
C_EXPORT void QTreeView_setColumnHidden(QTreeViewH handle, int column, bool hide);
C_EXPORT bool QTreeView_isHeaderHidden(QTreeViewH handle);
C_EXPORT void QTreeView_setHeaderHidden(QTreeViewH handle, bool hide);
C_EXPORT bool QTreeView_isRowHidden(QTreeViewH handle, int row, const QModelIndexH parent);
C_EXPORT void QTreeView_setRowHidden(QTreeViewH handle, int row, const QModelIndexH parent, bool hide);
C_EXPORT bool QTreeView_isFirstColumnSpanned(QTreeViewH handle, int row, const QModelIndexH parent);
C_EXPORT void QTreeView_setFirstColumnSpanned(QTreeViewH handle, int row, const QModelIndexH parent, bool span);
C_EXPORT bool QTreeView_isExpanded(QTreeViewH handle, const QModelIndexH index);
C_EXPORT void QTreeView_setExpanded(QTreeViewH handle, const QModelIndexH index, bool expand);
C_EXPORT void QTreeView_setSortingEnabled(QTreeViewH handle, bool enable);
C_EXPORT bool QTreeView_isSortingEnabled(QTreeViewH handle);
C_EXPORT void QTreeView_setAnimated(QTreeViewH handle, bool enable);
C_EXPORT bool QTreeView_isAnimated(QTreeViewH handle);
C_EXPORT void QTreeView_setAllColumnsShowFocus(QTreeViewH handle, bool enable);
C_EXPORT bool QTreeView_allColumnsShowFocus(QTreeViewH handle);
C_EXPORT void QTreeView_setWordWrap(QTreeViewH handle, bool on);
C_EXPORT bool QTreeView_wordWrap(QTreeViewH handle);
C_EXPORT void QTreeView_keyboardSearch(QTreeViewH handle, PWideString search);
C_EXPORT void QTreeView_visualRect(QTreeViewH handle, PRect retval, const QModelIndexH index);
C_EXPORT void QTreeView_scrollTo(QTreeViewH handle, const QModelIndexH index, QAbstractItemView::ScrollHint hint);
C_EXPORT void QTreeView_indexAt(QTreeViewH handle, QModelIndexH retval, const QPointH p);
C_EXPORT void QTreeView_indexAbove(QTreeViewH handle, QModelIndexH retval, const QModelIndexH index);
C_EXPORT void QTreeView_indexBelow(QTreeViewH handle, QModelIndexH retval, const QModelIndexH index);
C_EXPORT void QTreeView_doItemsLayout(QTreeViewH handle);
C_EXPORT void QTreeView_reset(QTreeViewH handle);
C_EXPORT void QTreeView_selectAll(QTreeViewH handle);
C_EXPORT void QTreeView_hideColumn(QTreeViewH handle, int column);
C_EXPORT void QTreeView_showColumn(QTreeViewH handle, int column);
C_EXPORT void QTreeView_expand(QTreeViewH handle, const QModelIndexH index);
C_EXPORT void QTreeView_collapse(QTreeViewH handle, const QModelIndexH index);
C_EXPORT void QTreeView_resizeColumnToContents(QTreeViewH handle, int column);
C_EXPORT void QTreeView_expandAll(QTreeViewH handle);
C_EXPORT void QTreeView_collapseAll(QTreeViewH handle);
C_EXPORT void QTreeView_expandToDepth(QTreeViewH handle, int depth);
C_EXPORT void QTreeView_sortByColumn(QTreeViewH handle, int column, Qt::SortOrder order);

#endif
