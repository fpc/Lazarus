//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qlistview_c.h"

QListViewH QListView_Create(QWidgetH parent)
{
	return (QListViewH) new QListView((QWidget*)parent);
}

void QListView_Destroy(QListViewH handle)
{
	delete (QListView *)handle;
}

void QListView_setMovement(QListViewH handle, QListView::Movement movement)
{
	((QListView *)handle)->setMovement(movement);
}

QListView::Movement QListView_movement(QListViewH handle)
{
	return (QListView::Movement) ((QListView *)handle)->movement();
}

void QListView_setFlow(QListViewH handle, QListView::Flow flow)
{
	((QListView *)handle)->setFlow(flow);
}

QListView::Flow QListView_flow(QListViewH handle)
{
	return (QListView::Flow) ((QListView *)handle)->flow();
}

void QListView_setWrapping(QListViewH handle, bool enable)
{
	((QListView *)handle)->setWrapping(enable);
}

bool QListView_isWrapping(QListViewH handle)
{
	return (bool) ((QListView *)handle)->isWrapping();
}

void QListView_setResizeMode(QListViewH handle, QListView::ResizeMode mode)
{
	((QListView *)handle)->setResizeMode(mode);
}

QListView::ResizeMode QListView_resizeMode(QListViewH handle)
{
	return (QListView::ResizeMode) ((QListView *)handle)->resizeMode();
}

void QListView_setLayoutMode(QListViewH handle, QListView::LayoutMode mode)
{
	((QListView *)handle)->setLayoutMode(mode);
}

QListView::LayoutMode QListView_layoutMode(QListViewH handle)
{
	return (QListView::LayoutMode) ((QListView *)handle)->layoutMode();
}

void QListView_setSpacing(QListViewH handle, int space)
{
	((QListView *)handle)->setSpacing(space);
}

int QListView_spacing(QListViewH handle)
{
	return (int) ((QListView *)handle)->spacing();
}

void QListView_setBatchSize(QListViewH handle, int batchSize)
{
	((QListView *)handle)->setBatchSize(batchSize);
}

int QListView_batchSize(QListViewH handle)
{
	return (int) ((QListView *)handle)->batchSize();
}

void QListView_setGridSize(QListViewH handle, const QSizeH size)
{
	((QListView *)handle)->setGridSize(*(const QSize*)size);
}

void QListView_gridSize(QListViewH handle, PSize retval)
{
	*(QSize *)retval = ((QListView *)handle)->gridSize();
}

void QListView_setViewMode(QListViewH handle, QListView::ViewMode mode)
{
	((QListView *)handle)->setViewMode(mode);
}

QListView::ViewMode QListView_viewMode(QListViewH handle)
{
	return (QListView::ViewMode) ((QListView *)handle)->viewMode();
}

void QListView_clearPropertyFlags(QListViewH handle)
{
	((QListView *)handle)->clearPropertyFlags();
}

bool QListView_isRowHidden(QListViewH handle, int row)
{
	return (bool) ((QListView *)handle)->isRowHidden(row);
}

void QListView_setRowHidden(QListViewH handle, int row, bool hide)
{
	((QListView *)handle)->setRowHidden(row, hide);
}

void QListView_setModelColumn(QListViewH handle, int column)
{
	((QListView *)handle)->setModelColumn(column);
}

int QListView_modelColumn(QListViewH handle)
{
	return (int) ((QListView *)handle)->modelColumn();
}

void QListView_setUniformItemSizes(QListViewH handle, bool enable)
{
	((QListView *)handle)->setUniformItemSizes(enable);
}

bool QListView_uniformItemSizes(QListViewH handle)
{
	return (bool) ((QListView *)handle)->uniformItemSizes();
}

void QListView_setWordWrap(QListViewH handle, bool on)
{
	((QListView *)handle)->setWordWrap(on);
}

bool QListView_wordWrap(QListViewH handle)
{
	return (bool) ((QListView *)handle)->wordWrap();
}

void QListView_setSelectionRectVisible(QListViewH handle, bool show)
{
	((QListView *)handle)->setSelectionRectVisible(show);
}

bool QListView_isSelectionRectVisible(QListViewH handle)
{
	return (bool) ((QListView *)handle)->isSelectionRectVisible();
}

void QListView_visualRect(QListViewH handle, PRect retval, const QModelIndexH index)
{
	QRect t_retval;
	t_retval = ((QListView *)handle)->visualRect(*(const QModelIndex*)index);
	copyQRectToPRect(t_retval, retval);
}

void QListView_scrollTo(QListViewH handle, const QModelIndexH index, QAbstractItemView::ScrollHint hint)
{
	((QListView *)handle)->scrollTo(*(const QModelIndex*)index, hint);
}

void QListView_indexAt(QListViewH handle, QModelIndexH retval, const QPointH p)
{
	*(QModelIndex *)retval = ((QListView *)handle)->indexAt(*(const QPoint*)p);
}

void QListView_doItemsLayout(QListViewH handle)
{
	((QListView *)handle)->doItemsLayout();
}

void QListView_reset(QListViewH handle)
{
	((QListView *)handle)->reset();
}

void QListView_setRootIndex(QListViewH handle, const QModelIndexH index)
{
	((QListView *)handle)->setRootIndex(*(const QModelIndex*)index);
}

Qt::Alignment QListView_itemAlignment(QListViewH handle)
{
  return (Qt::Alignment) ((QListView *)handle)->itemAlignment();
}

void QListView_setItemAlignment(QListViewH handle, Qt::Alignment alignment)
{
  ((QListView *)handle)->setItemAlignment(alignment);
}

