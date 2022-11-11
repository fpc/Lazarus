//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QLISTVIEW_C_H
#define QLISTVIEW_C_H

#include <QtWidgets>
#include "pascalbind.h"

C_EXPORT QListViewH QListView_Create(QWidgetH parent);
C_EXPORT void QListView_Destroy(QListViewH handle);
C_EXPORT void QListView_setMovement(QListViewH handle, QListView::Movement movement);
C_EXPORT QListView::Movement QListView_movement(QListViewH handle);
C_EXPORT void QListView_setFlow(QListViewH handle, QListView::Flow flow);
C_EXPORT QListView::Flow QListView_flow(QListViewH handle);
C_EXPORT void QListView_setWrapping(QListViewH handle, bool enable);
C_EXPORT bool QListView_isWrapping(QListViewH handle);
C_EXPORT void QListView_setResizeMode(QListViewH handle, QListView::ResizeMode mode);
C_EXPORT QListView::ResizeMode QListView_resizeMode(QListViewH handle);
C_EXPORT void QListView_setLayoutMode(QListViewH handle, QListView::LayoutMode mode);
C_EXPORT QListView::LayoutMode QListView_layoutMode(QListViewH handle);
C_EXPORT void QListView_setSpacing(QListViewH handle, int space);
C_EXPORT int QListView_spacing(QListViewH handle);
C_EXPORT void QListView_setBatchSize(QListViewH handle, int batchSize);
C_EXPORT int QListView_batchSize(QListViewH handle);
C_EXPORT void QListView_setGridSize(QListViewH handle, const QSizeH size);
C_EXPORT void QListView_gridSize(QListViewH handle, PSize retval);
C_EXPORT void QListView_setViewMode(QListViewH handle, QListView::ViewMode mode);
C_EXPORT QListView::ViewMode QListView_viewMode(QListViewH handle);
C_EXPORT void QListView_clearPropertyFlags(QListViewH handle);
C_EXPORT bool QListView_isRowHidden(QListViewH handle, int row);
C_EXPORT void QListView_setRowHidden(QListViewH handle, int row, bool hide);
C_EXPORT void QListView_setModelColumn(QListViewH handle, int column);
C_EXPORT int QListView_modelColumn(QListViewH handle);
C_EXPORT void QListView_setUniformItemSizes(QListViewH handle, bool enable);
C_EXPORT bool QListView_uniformItemSizes(QListViewH handle);
C_EXPORT void QListView_setWordWrap(QListViewH handle, bool on);
C_EXPORT bool QListView_wordWrap(QListViewH handle);
C_EXPORT void QListView_setSelectionRectVisible(QListViewH handle, bool show);
C_EXPORT bool QListView_isSelectionRectVisible(QListViewH handle);
C_EXPORT void QListView_visualRect(QListViewH handle, PRect retval, const QModelIndexH index);
C_EXPORT void QListView_scrollTo(QListViewH handle, const QModelIndexH index, QAbstractItemView::ScrollHint hint);
C_EXPORT void QListView_indexAt(QListViewH handle, QModelIndexH retval, const QPointH p);
C_EXPORT void QListView_doItemsLayout(QListViewH handle);
C_EXPORT void QListView_reset(QListViewH handle);
C_EXPORT void QListView_setRootIndex(QListViewH handle, const QModelIndexH index);
C_EXPORT Qt::Alignment QListView_itemAlignment(QListViewH handle);
C_EXPORT void QListView_setItemAlignment(QListViewH handle, Qt::Alignment alignment);

#endif
