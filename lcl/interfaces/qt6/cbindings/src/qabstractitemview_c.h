//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QABSTRACTITEMVIEW_C_H
#define QABSTRACTITEMVIEW_C_H

#include <QtWidgets>
#include "pascalbind.h"

C_EXPORT void QAbstractItemView_setModel(QAbstractItemViewH handle, QAbstractItemModelH model);
C_EXPORT QAbstractItemModelH QAbstractItemView_model(QAbstractItemViewH handle);
C_EXPORT void QAbstractItemView_setSelectionModel(QAbstractItemViewH handle, QItemSelectionModelH selectionModel);
C_EXPORT QItemSelectionModelH QAbstractItemView_selectionModel(QAbstractItemViewH handle);
C_EXPORT void QAbstractItemView_setItemDelegate(QAbstractItemViewH handle, QAbstractItemDelegateH delegate);
C_EXPORT QAbstractItemDelegateH QAbstractItemView_itemDelegate(QAbstractItemViewH handle);
C_EXPORT void QAbstractItemView_setSelectionMode(QAbstractItemViewH handle, QAbstractItemView::SelectionMode mode);
C_EXPORT QAbstractItemView::SelectionMode QAbstractItemView_selectionMode(QAbstractItemViewH handle);
C_EXPORT void QAbstractItemView_setSelectionBehavior(QAbstractItemViewH handle, QAbstractItemView::SelectionBehavior behavior);
C_EXPORT QAbstractItemView::SelectionBehavior QAbstractItemView_selectionBehavior(QAbstractItemViewH handle);
C_EXPORT void QAbstractItemView_currentIndex(QAbstractItemViewH handle, QModelIndexH retval);
C_EXPORT void QAbstractItemView_rootIndex(QAbstractItemViewH handle, QModelIndexH retval);
C_EXPORT void QAbstractItemView_setEditTriggers(QAbstractItemViewH handle, unsigned int triggers);
C_EXPORT unsigned int QAbstractItemView_editTriggers(QAbstractItemViewH handle);
C_EXPORT void QAbstractItemView_setVerticalScrollMode(QAbstractItemViewH handle, QAbstractItemView::ScrollMode mode);
C_EXPORT QAbstractItemView::ScrollMode QAbstractItemView_verticalScrollMode(QAbstractItemViewH handle);
C_EXPORT void QAbstractItemView_setHorizontalScrollMode(QAbstractItemViewH handle, QAbstractItemView::ScrollMode mode);
C_EXPORT QAbstractItemView::ScrollMode QAbstractItemView_horizontalScrollMode(QAbstractItemViewH handle);
C_EXPORT void QAbstractItemView_setAutoScroll(QAbstractItemViewH handle, bool enable);
C_EXPORT bool QAbstractItemView_hasAutoScroll(QAbstractItemViewH handle);
C_EXPORT void QAbstractItemView_setAutoScrollMargin(QAbstractItemViewH handle, int margin);
C_EXPORT int QAbstractItemView_autoScrollMargin(QAbstractItemViewH handle);
C_EXPORT void QAbstractItemView_setTabKeyNavigation(QAbstractItemViewH handle, bool enable);
C_EXPORT bool QAbstractItemView_tabKeyNavigation(QAbstractItemViewH handle);
C_EXPORT void QAbstractItemView_setDropIndicatorShown(QAbstractItemViewH handle, bool enable);
C_EXPORT bool QAbstractItemView_showDropIndicator(QAbstractItemViewH handle);
C_EXPORT void QAbstractItemView_setDragEnabled(QAbstractItemViewH handle, bool enable);
C_EXPORT bool QAbstractItemView_dragEnabled(QAbstractItemViewH handle);
C_EXPORT void QAbstractItemView_setDragDropOverwriteMode(QAbstractItemViewH handle, bool overwrite);
C_EXPORT bool QAbstractItemView_dragDropOverwriteMode(QAbstractItemViewH handle);
C_EXPORT void QAbstractItemView_setDragDropMode(QAbstractItemViewH handle, QAbstractItemView::DragDropMode behavior);
C_EXPORT QAbstractItemView::DragDropMode QAbstractItemView_dragDropMode(QAbstractItemViewH handle);
C_EXPORT void QAbstractItemView_setDefaultDropAction(QAbstractItemViewH handle, Qt::DropAction dropAction);
C_EXPORT Qt::DropAction QAbstractItemView_defaultDropAction(QAbstractItemViewH handle);
C_EXPORT void QAbstractItemView_setAlternatingRowColors(QAbstractItemViewH handle, bool enable);
C_EXPORT bool QAbstractItemView_alternatingRowColors(QAbstractItemViewH handle);
C_EXPORT void QAbstractItemView_setIconSize(QAbstractItemViewH handle, const QSizeH size);
C_EXPORT void QAbstractItemView_iconSize(QAbstractItemViewH handle, PSize retval);
C_EXPORT void QAbstractItemView_setTextElideMode(QAbstractItemViewH handle, Qt::TextElideMode mode);
C_EXPORT Qt::TextElideMode QAbstractItemView_textElideMode(QAbstractItemViewH handle);
C_EXPORT void QAbstractItemView_keyboardSearch(QAbstractItemViewH handle, PWideString search);
C_EXPORT void QAbstractItemView_visualRect(QAbstractItemViewH handle, PRect retval, const QModelIndexH index);
C_EXPORT void QAbstractItemView_scrollTo(QAbstractItemViewH handle, const QModelIndexH index, QAbstractItemView::ScrollHint hint);
C_EXPORT void QAbstractItemView_indexAt(QAbstractItemViewH handle, QModelIndexH retval, const QPointH point);
C_EXPORT void QAbstractItemView_sizeHintForIndex(QAbstractItemViewH handle, PSize retval, const QModelIndexH index);
C_EXPORT int QAbstractItemView_sizeHintForRow(QAbstractItemViewH handle, int row);
C_EXPORT int QAbstractItemView_sizeHintForColumn(QAbstractItemViewH handle, int column);
C_EXPORT void QAbstractItemView_openPersistentEditor(QAbstractItemViewH handle, const QModelIndexH index);
C_EXPORT void QAbstractItemView_closePersistentEditor(QAbstractItemViewH handle, const QModelIndexH index);
C_EXPORT void QAbstractItemView_setIndexWidget(QAbstractItemViewH handle, const QModelIndexH index, QWidgetH widget);
C_EXPORT QWidgetH QAbstractItemView_indexWidget(QAbstractItemViewH handle, const QModelIndexH index);
C_EXPORT void QAbstractItemView_setItemDelegateForRow(QAbstractItemViewH handle, int row, QAbstractItemDelegateH delegate);
C_EXPORT QAbstractItemDelegateH QAbstractItemView_itemDelegateForRow(QAbstractItemViewH handle, int row);
C_EXPORT void QAbstractItemView_setItemDelegateForColumn(QAbstractItemViewH handle, int column, QAbstractItemDelegateH delegate);
C_EXPORT QAbstractItemDelegateH QAbstractItemView_itemDelegateForColumn(QAbstractItemViewH handle, int column);
C_EXPORT QAbstractItemDelegateH QAbstractItemView_itemDelegate2(QAbstractItemViewH handle, const QModelIndexH index);
C_EXPORT void QAbstractItemView_inputMethodQuery(QAbstractItemViewH handle, QVariantH retval, Qt::InputMethodQuery query);
C_EXPORT void QAbstractItemView_reset(QAbstractItemViewH handle);
C_EXPORT void QAbstractItemView_setRootIndex(QAbstractItemViewH handle, const QModelIndexH index);
C_EXPORT void QAbstractItemView_doItemsLayout(QAbstractItemViewH handle);
C_EXPORT void QAbstractItemView_selectAll(QAbstractItemViewH handle);
C_EXPORT void QAbstractItemView_edit(QAbstractItemViewH handle, const QModelIndexH index);
C_EXPORT void QAbstractItemView_clearSelection(QAbstractItemViewH handle);
C_EXPORT void QAbstractItemView_setCurrentIndex(QAbstractItemViewH handle, const QModelIndexH index);
C_EXPORT void QAbstractItemView_scrollToTop(QAbstractItemViewH handle);
C_EXPORT void QAbstractItemView_scrollToBottom(QAbstractItemViewH handle);
C_EXPORT void QAbstractItemView_update(QAbstractItemViewH handle, const QModelIndexH index);
C_EXPORT bool QAbstractItemView_isPersistentEditorOpen(QAbstractItemViewH handle, const QModelIndexH index);

#endif
