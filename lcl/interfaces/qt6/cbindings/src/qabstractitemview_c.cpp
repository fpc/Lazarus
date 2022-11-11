//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qabstractitemview_c.h"

void QAbstractItemView_setModel(QAbstractItemViewH handle, QAbstractItemModelH model)
{
	((QAbstractItemView *)handle)->setModel((QAbstractItemModel*)model);
}

QAbstractItemModelH QAbstractItemView_model(QAbstractItemViewH handle)
{
	return (QAbstractItemModelH) ((QAbstractItemView *)handle)->model();
}

void QAbstractItemView_setSelectionModel(QAbstractItemViewH handle, QItemSelectionModelH selectionModel)
{
	((QAbstractItemView *)handle)->setSelectionModel((QItemSelectionModel*)selectionModel);
}

QItemSelectionModelH QAbstractItemView_selectionModel(QAbstractItemViewH handle)
{
	return (QItemSelectionModelH) ((QAbstractItemView *)handle)->selectionModel();
}

void QAbstractItemView_setItemDelegate(QAbstractItemViewH handle, QAbstractItemDelegateH delegate)
{
	((QAbstractItemView *)handle)->setItemDelegate((QAbstractItemDelegate*)delegate);
}

QAbstractItemDelegateH QAbstractItemView_itemDelegate(QAbstractItemViewH handle)
{
	return (QAbstractItemDelegateH) ((QAbstractItemView *)handle)->itemDelegate();
}

void QAbstractItemView_setSelectionMode(QAbstractItemViewH handle, QAbstractItemView::SelectionMode mode)
{
	((QAbstractItemView *)handle)->setSelectionMode(mode);
}

QAbstractItemView::SelectionMode QAbstractItemView_selectionMode(QAbstractItemViewH handle)
{
	return (QAbstractItemView::SelectionMode) ((QAbstractItemView *)handle)->selectionMode();
}

void QAbstractItemView_setSelectionBehavior(QAbstractItemViewH handle, QAbstractItemView::SelectionBehavior behavior)
{
	((QAbstractItemView *)handle)->setSelectionBehavior(behavior);
}

QAbstractItemView::SelectionBehavior QAbstractItemView_selectionBehavior(QAbstractItemViewH handle)
{
	return (QAbstractItemView::SelectionBehavior) ((QAbstractItemView *)handle)->selectionBehavior();
}

void QAbstractItemView_currentIndex(QAbstractItemViewH handle, QModelIndexH retval)
{
	*(QModelIndex *)retval = ((QAbstractItemView *)handle)->currentIndex();
}

void QAbstractItemView_rootIndex(QAbstractItemViewH handle, QModelIndexH retval)
{
	*(QModelIndex *)retval = ((QAbstractItemView *)handle)->rootIndex();
}

void QAbstractItemView_setEditTriggers(QAbstractItemViewH handle, unsigned int triggers)
{
	((QAbstractItemView *)handle)->setEditTriggers((QAbstractItemView::EditTriggers)triggers);
}

unsigned int QAbstractItemView_editTriggers(QAbstractItemViewH handle)
{
	return (unsigned int) ((QAbstractItemView *)handle)->editTriggers();
}

void QAbstractItemView_setVerticalScrollMode(QAbstractItemViewH handle, QAbstractItemView::ScrollMode mode)
{
	((QAbstractItemView *)handle)->setVerticalScrollMode(mode);
}

QAbstractItemView::ScrollMode QAbstractItemView_verticalScrollMode(QAbstractItemViewH handle)
{
	return (QAbstractItemView::ScrollMode) ((QAbstractItemView *)handle)->verticalScrollMode();
}

void QAbstractItemView_setHorizontalScrollMode(QAbstractItemViewH handle, QAbstractItemView::ScrollMode mode)
{
	((QAbstractItemView *)handle)->setHorizontalScrollMode(mode);
}

QAbstractItemView::ScrollMode QAbstractItemView_horizontalScrollMode(QAbstractItemViewH handle)
{
	return (QAbstractItemView::ScrollMode) ((QAbstractItemView *)handle)->horizontalScrollMode();
}

void QAbstractItemView_setAutoScroll(QAbstractItemViewH handle, bool enable)
{
	((QAbstractItemView *)handle)->setAutoScroll(enable);
}

bool QAbstractItemView_hasAutoScroll(QAbstractItemViewH handle)
{
	return (bool) ((QAbstractItemView *)handle)->hasAutoScroll();
}

void QAbstractItemView_setAutoScrollMargin(QAbstractItemViewH handle, int margin)
{
	((QAbstractItemView *)handle)->setAutoScrollMargin(margin);
}

int QAbstractItemView_autoScrollMargin(QAbstractItemViewH handle)
{
	return (int) ((QAbstractItemView *)handle)->autoScrollMargin();
}

void QAbstractItemView_setTabKeyNavigation(QAbstractItemViewH handle, bool enable)
{
	((QAbstractItemView *)handle)->setTabKeyNavigation(enable);
}

bool QAbstractItemView_tabKeyNavigation(QAbstractItemViewH handle)
{
	return (bool) ((QAbstractItemView *)handle)->tabKeyNavigation();
}

void QAbstractItemView_setDropIndicatorShown(QAbstractItemViewH handle, bool enable)
{
	((QAbstractItemView *)handle)->setDropIndicatorShown(enable);
}

bool QAbstractItemView_showDropIndicator(QAbstractItemViewH handle)
{
	return (bool) ((QAbstractItemView *)handle)->showDropIndicator();
}

void QAbstractItemView_setDragEnabled(QAbstractItemViewH handle, bool enable)
{
	((QAbstractItemView *)handle)->setDragEnabled(enable);
}

bool QAbstractItemView_dragEnabled(QAbstractItemViewH handle)
{
	return (bool) ((QAbstractItemView *)handle)->dragEnabled();
}

void QAbstractItemView_setDragDropOverwriteMode(QAbstractItemViewH handle, bool overwrite)
{
	((QAbstractItemView *)handle)->setDragDropOverwriteMode(overwrite);
}

bool QAbstractItemView_dragDropOverwriteMode(QAbstractItemViewH handle)
{
	return (bool) ((QAbstractItemView *)handle)->dragDropOverwriteMode();
}

void QAbstractItemView_setDragDropMode(QAbstractItemViewH handle, QAbstractItemView::DragDropMode behavior)
{
	((QAbstractItemView *)handle)->setDragDropMode(behavior);
}

QAbstractItemView::DragDropMode QAbstractItemView_dragDropMode(QAbstractItemViewH handle)
{
	return (QAbstractItemView::DragDropMode) ((QAbstractItemView *)handle)->dragDropMode();
}

void QAbstractItemView_setDefaultDropAction(QAbstractItemViewH handle, Qt::DropAction dropAction)
{
	((QAbstractItemView *)handle)->setDefaultDropAction(dropAction);
}

Qt::DropAction QAbstractItemView_defaultDropAction(QAbstractItemViewH handle)
{
	return (Qt::DropAction) ((QAbstractItemView *)handle)->defaultDropAction();
}

void QAbstractItemView_setAlternatingRowColors(QAbstractItemViewH handle, bool enable)
{
	((QAbstractItemView *)handle)->setAlternatingRowColors(enable);
}

bool QAbstractItemView_alternatingRowColors(QAbstractItemViewH handle)
{
	return (bool) ((QAbstractItemView *)handle)->alternatingRowColors();
}

void QAbstractItemView_setIconSize(QAbstractItemViewH handle, const QSizeH size)
{
	((QAbstractItemView *)handle)->setIconSize(*(const QSize*)size);
}

void QAbstractItemView_iconSize(QAbstractItemViewH handle, PSize retval)
{
	*(QSize *)retval = ((QAbstractItemView *)handle)->iconSize();
}

void QAbstractItemView_setTextElideMode(QAbstractItemViewH handle, Qt::TextElideMode mode)
{
	((QAbstractItemView *)handle)->setTextElideMode(mode);
}

Qt::TextElideMode QAbstractItemView_textElideMode(QAbstractItemViewH handle)
{
	return (Qt::TextElideMode) ((QAbstractItemView *)handle)->textElideMode();
}

void QAbstractItemView_keyboardSearch(QAbstractItemViewH handle, PWideString search)
{
	QString t_search;
	copyPWideStringToQString(search, t_search);
	((QAbstractItemView *)handle)->keyboardSearch(t_search);
}

void QAbstractItemView_visualRect(QAbstractItemViewH handle, PRect retval, const QModelIndexH index)
{
	QRect t_retval;
	t_retval = ((QAbstractItemView *)handle)->visualRect(*(const QModelIndex*)index);
	copyQRectToPRect(t_retval, retval);
}

void QAbstractItemView_scrollTo(QAbstractItemViewH handle, const QModelIndexH index, QAbstractItemView::ScrollHint hint)
{
	((QAbstractItemView *)handle)->scrollTo(*(const QModelIndex*)index, hint);
}

void QAbstractItemView_indexAt(QAbstractItemViewH handle, QModelIndexH retval, const QPointH point)
{
	*(QModelIndex *)retval = ((QAbstractItemView *)handle)->indexAt(*(const QPoint*)point);
}

void QAbstractItemView_sizeHintForIndex(QAbstractItemViewH handle, PSize retval, const QModelIndexH index)
{
	*(QSize *)retval = ((QAbstractItemView *)handle)->sizeHintForIndex(*(const QModelIndex*)index);
}

int QAbstractItemView_sizeHintForRow(QAbstractItemViewH handle, int row)
{
	return (int) ((QAbstractItemView *)handle)->sizeHintForRow(row);
}

int QAbstractItemView_sizeHintForColumn(QAbstractItemViewH handle, int column)
{
	return (int) ((QAbstractItemView *)handle)->sizeHintForColumn(column);
}

void QAbstractItemView_openPersistentEditor(QAbstractItemViewH handle, const QModelIndexH index)
{
	((QAbstractItemView *)handle)->openPersistentEditor(*(const QModelIndex*)index);
}

void QAbstractItemView_closePersistentEditor(QAbstractItemViewH handle, const QModelIndexH index)
{
	((QAbstractItemView *)handle)->closePersistentEditor(*(const QModelIndex*)index);
}

void QAbstractItemView_setIndexWidget(QAbstractItemViewH handle, const QModelIndexH index, QWidgetH widget)
{
	((QAbstractItemView *)handle)->setIndexWidget(*(const QModelIndex*)index, (QWidget*)widget);
}

QWidgetH QAbstractItemView_indexWidget(QAbstractItemViewH handle, const QModelIndexH index)
{
	return (QWidgetH) ((QAbstractItemView *)handle)->indexWidget(*(const QModelIndex*)index);
}

void QAbstractItemView_setItemDelegateForRow(QAbstractItemViewH handle, int row, QAbstractItemDelegateH delegate)
{
	((QAbstractItemView *)handle)->setItemDelegateForRow(row, (QAbstractItemDelegate*)delegate);
}

QAbstractItemDelegateH QAbstractItemView_itemDelegateForRow(QAbstractItemViewH handle, int row)
{
	return (QAbstractItemDelegateH) ((QAbstractItemView *)handle)->itemDelegateForRow(row);
}

void QAbstractItemView_setItemDelegateForColumn(QAbstractItemViewH handle, int column, QAbstractItemDelegateH delegate)
{
	((QAbstractItemView *)handle)->setItemDelegateForColumn(column, (QAbstractItemDelegate*)delegate);
}

QAbstractItemDelegateH QAbstractItemView_itemDelegateForColumn(QAbstractItemViewH handle, int column)
{
	return (QAbstractItemDelegateH) ((QAbstractItemView *)handle)->itemDelegateForColumn(column);
}

QAbstractItemDelegateH QAbstractItemView_itemDelegate2(QAbstractItemViewH handle, const QModelIndexH index)
{
	return (QAbstractItemDelegateH) ((QAbstractItemView *)handle)->itemDelegate(*(const QModelIndex*)index);
}

void QAbstractItemView_inputMethodQuery(QAbstractItemViewH handle, QVariantH retval, Qt::InputMethodQuery query)
{
	*(QVariant *)retval = ((QAbstractItemView *)handle)->inputMethodQuery(query);
}

void QAbstractItemView_reset(QAbstractItemViewH handle)
{
	((QAbstractItemView *)handle)->reset();
}

void QAbstractItemView_setRootIndex(QAbstractItemViewH handle, const QModelIndexH index)
{
	((QAbstractItemView *)handle)->setRootIndex(*(const QModelIndex*)index);
}

void QAbstractItemView_doItemsLayout(QAbstractItemViewH handle)
{
	((QAbstractItemView *)handle)->doItemsLayout();
}

void QAbstractItemView_selectAll(QAbstractItemViewH handle)
{
	((QAbstractItemView *)handle)->selectAll();
}

void QAbstractItemView_edit(QAbstractItemViewH handle, const QModelIndexH index)
{
	((QAbstractItemView *)handle)->edit(*(const QModelIndex*)index);
}

void QAbstractItemView_clearSelection(QAbstractItemViewH handle)
{
	((QAbstractItemView *)handle)->clearSelection();
}

void QAbstractItemView_setCurrentIndex(QAbstractItemViewH handle, const QModelIndexH index)
{
	((QAbstractItemView *)handle)->setCurrentIndex(*(const QModelIndex*)index);
}

void QAbstractItemView_scrollToTop(QAbstractItemViewH handle)
{
	((QAbstractItemView *)handle)->scrollToTop();
}

void QAbstractItemView_scrollToBottom(QAbstractItemViewH handle)
{
	((QAbstractItemView *)handle)->scrollToBottom();
}

void QAbstractItemView_update(QAbstractItemViewH handle, const QModelIndexH index)
{
	((QAbstractItemView *)handle)->update(*(const QModelIndex*)index);
}

bool QAbstractItemView_isPersistentEditorOpen(QAbstractItemViewH handle, const QModelIndexH index)
{
  return (bool) ((QAbstractItemView *)handle)->isPersistentEditorOpen(*(const QModelIndex*)index);
}


