//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QTREEWIDGET_C_H
#define QTREEWIDGET_C_H

#include <QtWidgets>
#include "pascalbind.h"

C_EXPORT QTreeWidgetItemH QTreeWidgetItem_Create(int type);
C_EXPORT void QTreeWidgetItem_Destroy(QTreeWidgetItemH handle);
C_EXPORT QTreeWidgetItemH QTreeWidgetItem_Create2(const QStringListH strings, int type);
C_EXPORT QTreeWidgetItemH QTreeWidgetItem_Create3(QTreeWidgetH view, int type);
C_EXPORT QTreeWidgetItemH QTreeWidgetItem_Create4(QTreeWidgetH view, const QStringListH strings, int type);
C_EXPORT QTreeWidgetItemH QTreeWidgetItem_Create5(QTreeWidgetH view, QTreeWidgetItemH after, int type);
C_EXPORT QTreeWidgetItemH QTreeWidgetItem_Create6(QTreeWidgetItemH parent, int type);
C_EXPORT QTreeWidgetItemH QTreeWidgetItem_Create7(QTreeWidgetItemH parent, const QStringListH strings, int type);
C_EXPORT QTreeWidgetItemH QTreeWidgetItem_Create8(QTreeWidgetItemH parent, QTreeWidgetItemH after, int type);
C_EXPORT QTreeWidgetItemH QTreeWidgetItem_Create9(const QTreeWidgetItemH other);
C_EXPORT QTreeWidgetItemH QTreeWidgetItem_clone(QTreeWidgetItemH handle);
C_EXPORT QTreeWidgetH QTreeWidgetItem_treeWidget(QTreeWidgetItemH handle);
C_EXPORT void QTreeWidgetItem_setSelected(QTreeWidgetItemH handle, bool select);
C_EXPORT bool QTreeWidgetItem_isSelected(QTreeWidgetItemH handle);
C_EXPORT void QTreeWidgetItem_setHidden(QTreeWidgetItemH handle, bool hide);
C_EXPORT bool QTreeWidgetItem_isHidden(QTreeWidgetItemH handle);
C_EXPORT void QTreeWidgetItem_setExpanded(QTreeWidgetItemH handle, bool expand);
C_EXPORT bool QTreeWidgetItem_isExpanded(QTreeWidgetItemH handle);
C_EXPORT void QTreeWidgetItem_setFirstColumnSpanned(QTreeWidgetItemH handle, bool span);
C_EXPORT bool QTreeWidgetItem_isFirstColumnSpanned(QTreeWidgetItemH handle);
C_EXPORT void QTreeWidgetItem_setDisabled(QTreeWidgetItemH handle, bool disabled);
C_EXPORT bool QTreeWidgetItem_isDisabled(QTreeWidgetItemH handle);
C_EXPORT void QTreeWidgetItem_setChildIndicatorPolicy(QTreeWidgetItemH handle, QTreeWidgetItem::ChildIndicatorPolicy policy);
C_EXPORT QTreeWidgetItem::ChildIndicatorPolicy QTreeWidgetItem_childIndicatorPolicy(QTreeWidgetItemH handle);
C_EXPORT unsigned int QTreeWidgetItem_flags(QTreeWidgetItemH handle);
C_EXPORT void QTreeWidgetItem_setFlags(QTreeWidgetItemH handle, unsigned int flags);
C_EXPORT void QTreeWidgetItem_text(QTreeWidgetItemH handle, PWideString retval, int column);
C_EXPORT void QTreeWidgetItem_setText(QTreeWidgetItemH handle, int column, PWideString text);
C_EXPORT void QTreeWidgetItem_icon(QTreeWidgetItemH handle, QIconH retval, int column);
C_EXPORT void QTreeWidgetItem_setIcon(QTreeWidgetItemH handle, int column, const QIconH icon);
C_EXPORT void QTreeWidgetItem_statusTip(QTreeWidgetItemH handle, PWideString retval, int column);
C_EXPORT void QTreeWidgetItem_setStatusTip(QTreeWidgetItemH handle, int column, PWideString statusTip);
C_EXPORT void QTreeWidgetItem_toolTip(QTreeWidgetItemH handle, PWideString retval, int column);
C_EXPORT void QTreeWidgetItem_setToolTip(QTreeWidgetItemH handle, int column, PWideString toolTip);
C_EXPORT void QTreeWidgetItem_whatsThis(QTreeWidgetItemH handle, PWideString retval, int column);
C_EXPORT void QTreeWidgetItem_setWhatsThis(QTreeWidgetItemH handle, int column, PWideString whatsThis);
C_EXPORT void QTreeWidgetItem_font(QTreeWidgetItemH handle, QFontH retval, int column);
C_EXPORT void QTreeWidgetItem_setFont(QTreeWidgetItemH handle, int column, const QFontH font);
C_EXPORT int QTreeWidgetItem_textAlignment(QTreeWidgetItemH handle, int column);
C_EXPORT void QTreeWidgetItem_setTextAlignment(QTreeWidgetItemH handle, int column, int alignment);
C_EXPORT void QTreeWidgetItem_backgroundColor(QTreeWidgetItemH handle, PQColor retval, int column);
C_EXPORT void QTreeWidgetItem_setBackgroundColor(QTreeWidgetItemH handle, int column, const QColorH color);
C_EXPORT void QTreeWidgetItem_background(QTreeWidgetItemH handle, QBrushH retval, int column);
C_EXPORT void QTreeWidgetItem_setBackground(QTreeWidgetItemH handle, int column, const QBrushH brush);
C_EXPORT void QTreeWidgetItem_foreground(QTreeWidgetItemH handle, QBrushH retval, int column);
C_EXPORT void QTreeWidgetItem_setForeground(QTreeWidgetItemH handle, int column, const QBrushH brush);
C_EXPORT Qt::CheckState QTreeWidgetItem_checkState(QTreeWidgetItemH handle, int column);
C_EXPORT void QTreeWidgetItem_setCheckState(QTreeWidgetItemH handle, int column, Qt::CheckState state);
C_EXPORT void QTreeWidgetItem_sizeHint(QTreeWidgetItemH handle, PSize retval, int column);
C_EXPORT void QTreeWidgetItem_setSizeHint(QTreeWidgetItemH handle, int column, const QSizeH size);
C_EXPORT void QTreeWidgetItem_data(QTreeWidgetItemH handle, QVariantH retval, int column, int role);
C_EXPORT void QTreeWidgetItem_setData(QTreeWidgetItemH handle, int column, int role, const QVariantH value);
C_EXPORT void QTreeWidgetItem_read(QTreeWidgetItemH handle, QDataStreamH in);
C_EXPORT void QTreeWidgetItem_write(QTreeWidgetItemH handle, QDataStreamH out);
C_EXPORT QTreeWidgetItemH QTreeWidgetItem_parent(QTreeWidgetItemH handle);
C_EXPORT QTreeWidgetItemH QTreeWidgetItem_child(QTreeWidgetItemH handle, int index);
C_EXPORT int QTreeWidgetItem_childCount(QTreeWidgetItemH handle);
C_EXPORT int QTreeWidgetItem_columnCount(QTreeWidgetItemH handle);
C_EXPORT int QTreeWidgetItem_indexOfChild(QTreeWidgetItemH handle, QTreeWidgetItemH child);
C_EXPORT void QTreeWidgetItem_addChild(QTreeWidgetItemH handle, QTreeWidgetItemH child);
C_EXPORT void QTreeWidgetItem_insertChild(QTreeWidgetItemH handle, int index, QTreeWidgetItemH child);
C_EXPORT void QTreeWidgetItem_removeChild(QTreeWidgetItemH handle, QTreeWidgetItemH child);
C_EXPORT QTreeWidgetItemH QTreeWidgetItem_takeChild(QTreeWidgetItemH handle, int index);
C_EXPORT void QTreeWidgetItem_addChildren(QTreeWidgetItemH handle, PPtrIntArray children);
C_EXPORT void QTreeWidgetItem_insertChildren(QTreeWidgetItemH handle, int index, PPtrIntArray children);
C_EXPORT void QTreeWidgetItem_takeChildren(QTreeWidgetItemH handle, PPtrIntArray retval);
C_EXPORT int QTreeWidgetItem_type(QTreeWidgetItemH handle);
C_EXPORT void QTreeWidgetItem_sortChildren(QTreeWidgetItemH handle, int column, Qt::SortOrder order);
C_EXPORT QTreeWidgetH QTreeWidget_Create(QWidgetH parent);
C_EXPORT void QTreeWidget_Destroy(QTreeWidgetH handle);
C_EXPORT int QTreeWidget_columnCount(QTreeWidgetH handle);
C_EXPORT void QTreeWidget_setColumnCount(QTreeWidgetH handle, int columns);
C_EXPORT QTreeWidgetItemH QTreeWidget_invisibleRootItem(QTreeWidgetH handle);
C_EXPORT QTreeWidgetItemH QTreeWidget_topLevelItem(QTreeWidgetH handle, int index);
C_EXPORT int QTreeWidget_topLevelItemCount(QTreeWidgetH handle);
C_EXPORT void QTreeWidget_insertTopLevelItem(QTreeWidgetH handle, int index, QTreeWidgetItemH item);
C_EXPORT void QTreeWidget_addTopLevelItem(QTreeWidgetH handle, QTreeWidgetItemH item);
C_EXPORT QTreeWidgetItemH QTreeWidget_takeTopLevelItem(QTreeWidgetH handle, int index);
C_EXPORT int QTreeWidget_indexOfTopLevelItem(QTreeWidgetH handle, QTreeWidgetItemH item);
C_EXPORT void QTreeWidget_insertTopLevelItems(QTreeWidgetH handle, int index, PPtrIntArray items);
C_EXPORT void QTreeWidget_addTopLevelItems(QTreeWidgetH handle, PPtrIntArray items);
C_EXPORT QTreeWidgetItemH QTreeWidget_headerItem(QTreeWidgetH handle);
C_EXPORT void QTreeWidget_setHeaderItem(QTreeWidgetH handle, QTreeWidgetItemH item);
C_EXPORT void QTreeWidget_setHeaderLabels(QTreeWidgetH handle, const QStringListH labels);
C_EXPORT void QTreeWidget_setHeaderLabel(QTreeWidgetH handle, PWideString label);
C_EXPORT QTreeWidgetItemH QTreeWidget_currentItem(QTreeWidgetH handle);
C_EXPORT int QTreeWidget_currentColumn(QTreeWidgetH handle);
C_EXPORT void QTreeWidget_setCurrentItem(QTreeWidgetH handle, QTreeWidgetItemH item);
C_EXPORT void QTreeWidget_setCurrentItem2(QTreeWidgetH handle, QTreeWidgetItemH item, int column);
C_EXPORT void QTreeWidget_setCurrentItem3(QTreeWidgetH handle, QTreeWidgetItemH item, int column, unsigned int command);
C_EXPORT QTreeWidgetItemH QTreeWidget_itemAt(QTreeWidgetH handle, const QPointH p);
C_EXPORT QTreeWidgetItemH QTreeWidget_itemAt2(QTreeWidgetH handle, int x, int y);
C_EXPORT void QTreeWidget_visualItemRect(QTreeWidgetH handle, PRect retval, const QTreeWidgetItemH item);
C_EXPORT int QTreeWidget_sortColumn(QTreeWidgetH handle);
C_EXPORT void QTreeWidget_sortItems(QTreeWidgetH handle, int column, Qt::SortOrder order);
C_EXPORT void QTreeWidget_editItem(QTreeWidgetH handle, QTreeWidgetItemH item, int column);
C_EXPORT void QTreeWidget_openPersistentEditor(QTreeWidgetH handle, QTreeWidgetItemH item, int column);
C_EXPORT void QTreeWidget_closePersistentEditor(QTreeWidgetH handle, QTreeWidgetItemH item, int column);
C_EXPORT QWidgetH QTreeWidget_itemWidget(QTreeWidgetH handle, QTreeWidgetItemH item, int column);
C_EXPORT void QTreeWidget_setItemWidget(QTreeWidgetH handle, QTreeWidgetItemH item, int column, QWidgetH widget);
C_EXPORT void QTreeWidget_removeItemWidget(QTreeWidgetH handle, QTreeWidgetItemH item, int column);
C_EXPORT void QTreeWidget_selectedItems(QTreeWidgetH handle, PPtrIntArray retval);
C_EXPORT void QTreeWidget_findItems(QTreeWidgetH handle, PPtrIntArray retval, PWideString text, unsigned int flags, int column);
C_EXPORT QTreeWidgetItemH QTreeWidget_itemAbove(QTreeWidgetH handle, const QTreeWidgetItemH item);
C_EXPORT QTreeWidgetItemH QTreeWidget_itemBelow(QTreeWidgetH handle, const QTreeWidgetItemH item);
C_EXPORT void QTreeWidget_setSelectionModel(QTreeWidgetH handle, QItemSelectionModelH selectionModel);
C_EXPORT void QTreeWidget_scrollToItem(QTreeWidgetH handle, const QTreeWidgetItemH item, QAbstractItemView::ScrollHint hint);
C_EXPORT void QTreeWidget_expandItem(QTreeWidgetH handle, const QTreeWidgetItemH item);
C_EXPORT void QTreeWidget_collapseItem(QTreeWidgetH handle, const QTreeWidgetItemH item);
C_EXPORT void QTreeWidget_clear(QTreeWidgetH handle);
C_EXPORT bool QTreeWidget_isPersistentEditorOpen(QTreeWidgetH handle, QTreeWidgetItemH item, int column = 0);
#endif
