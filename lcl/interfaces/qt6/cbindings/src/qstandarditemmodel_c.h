//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QSTANDARDITEMMODEL_C_H
#define QSTANDARDITEMMODEL_C_H

#include <QtGui>
#include "pascalbind.h"

C_EXPORT QStandardItemH QStandardItem_Create();
C_EXPORT void QStandardItem_Destroy(QStandardItemH handle);
C_EXPORT QStandardItemH QStandardItem_Create2(PWideString text);
C_EXPORT QStandardItemH QStandardItem_Create3(const QIconH icon, PWideString text);
C_EXPORT QStandardItemH QStandardItem_Create4(int rows, int columns);
C_EXPORT void QStandardItem_data(QStandardItemH handle, QVariantH retval, Qt::ItemDataRole role);
C_EXPORT void QStandardItem_setData(QStandardItemH handle, const QVariantH value, Qt::ItemDataRole role);
C_EXPORT void QStandardItem_text(QStandardItemH handle, PWideString retval);
C_EXPORT void QStandardItem_setText(QStandardItemH handle, PWideString text);
C_EXPORT void QStandardItem_icon(QStandardItemH handle, QIconH retval);
C_EXPORT void QStandardItem_setIcon(QStandardItemH handle, const QIconH icon);
C_EXPORT void QStandardItem_toolTip(QStandardItemH handle, PWideString retval);
C_EXPORT void QStandardItem_setToolTip(QStandardItemH handle, PWideString toolTip);
C_EXPORT void QStandardItem_statusTip(QStandardItemH handle, PWideString retval);
C_EXPORT void QStandardItem_setStatusTip(QStandardItemH handle, PWideString statusTip);
C_EXPORT void QStandardItem_whatsThis(QStandardItemH handle, PWideString retval);
C_EXPORT void QStandardItem_setWhatsThis(QStandardItemH handle, PWideString whatsThis);
C_EXPORT void QStandardItem_sizeHint(QStandardItemH handle, PSize retval);
C_EXPORT void QStandardItem_setSizeHint(QStandardItemH handle, const QSizeH sizeHint);
C_EXPORT void QStandardItem_font(QStandardItemH handle, QFontH retval);
C_EXPORT void QStandardItem_setFont(QStandardItemH handle, const QFontH font);
C_EXPORT unsigned int QStandardItem_textAlignment(QStandardItemH handle);
C_EXPORT void QStandardItem_setTextAlignment(QStandardItemH handle, unsigned int textAlignment);
C_EXPORT void QStandardItem_background(QStandardItemH handle, QBrushH retval);
C_EXPORT void QStandardItem_setBackground(QStandardItemH handle, const QBrushH brush);
C_EXPORT void QStandardItem_foreground(QStandardItemH handle, QBrushH retval);
C_EXPORT void QStandardItem_setForeground(QStandardItemH handle, const QBrushH brush);
C_EXPORT Qt::CheckState QStandardItem_checkState(QStandardItemH handle);
C_EXPORT void QStandardItem_setCheckState(QStandardItemH handle, Qt::CheckState checkState);
C_EXPORT void QStandardItem_accessibleText(QStandardItemH handle, PWideString retval);
C_EXPORT void QStandardItem_setAccessibleText(QStandardItemH handle, PWideString accessibleText);
C_EXPORT void QStandardItem_accessibleDescription(QStandardItemH handle, PWideString retval);
C_EXPORT void QStandardItem_setAccessibleDescription(QStandardItemH handle, PWideString accessibleDescription);
C_EXPORT unsigned int QStandardItem_flags(QStandardItemH handle);
C_EXPORT void QStandardItem_setFlags(QStandardItemH handle, unsigned int flags);
C_EXPORT bool QStandardItem_isEnabled(QStandardItemH handle);
C_EXPORT void QStandardItem_setEnabled(QStandardItemH handle, bool enabled);
C_EXPORT bool QStandardItem_isEditable(QStandardItemH handle);
C_EXPORT void QStandardItem_setEditable(QStandardItemH handle, bool editable);
C_EXPORT bool QStandardItem_isSelectable(QStandardItemH handle);
C_EXPORT void QStandardItem_setSelectable(QStandardItemH handle, bool selectable);
C_EXPORT bool QStandardItem_isCheckable(QStandardItemH handle);
C_EXPORT void QStandardItem_setCheckable(QStandardItemH handle, bool checkable);
C_EXPORT bool QStandardItem_isDragEnabled(QStandardItemH handle);
C_EXPORT void QStandardItem_setDragEnabled(QStandardItemH handle, bool dragEnabled);
C_EXPORT bool QStandardItem_isDropEnabled(QStandardItemH handle);
C_EXPORT void QStandardItem_setDropEnabled(QStandardItemH handle, bool dropEnabled);
C_EXPORT QStandardItemH QStandardItem_parent(QStandardItemH handle);
C_EXPORT int QStandardItem_row(QStandardItemH handle);
C_EXPORT int QStandardItem_column(QStandardItemH handle);
C_EXPORT void QStandardItem_index(QStandardItemH handle, QModelIndexH retval);
C_EXPORT QStandardItemModelH QStandardItem_model(QStandardItemH handle);
C_EXPORT int QStandardItem_rowCount(QStandardItemH handle);
C_EXPORT void QStandardItem_setRowCount(QStandardItemH handle, int rows);
C_EXPORT int QStandardItem_columnCount(QStandardItemH handle);
C_EXPORT void QStandardItem_setColumnCount(QStandardItemH handle, int columns);
C_EXPORT bool QStandardItem_hasChildren(QStandardItemH handle);
C_EXPORT QStandardItemH QStandardItem_child(QStandardItemH handle, int row, int column);
C_EXPORT void QStandardItem_setChild(QStandardItemH handle, int row, int column, QStandardItemH item);
C_EXPORT void QStandardItem_setChild2(QStandardItemH handle, int row, QStandardItemH item);
C_EXPORT void QStandardItem_insertRow(QStandardItemH handle, int row, PPtrIntArray items);
C_EXPORT void QStandardItem_insertColumn(QStandardItemH handle, int column, PPtrIntArray items);
C_EXPORT void QStandardItem_insertRows(QStandardItemH handle, int row, PPtrIntArray items);
C_EXPORT void QStandardItem_insertRows2(QStandardItemH handle, int row, int count);
C_EXPORT void QStandardItem_insertColumns(QStandardItemH handle, int column, int count);
C_EXPORT void QStandardItem_removeRow(QStandardItemH handle, int row);
C_EXPORT void QStandardItem_removeColumn(QStandardItemH handle, int column);
C_EXPORT void QStandardItem_removeRows(QStandardItemH handle, int row, int count);
C_EXPORT void QStandardItem_removeColumns(QStandardItemH handle, int column, int count);
C_EXPORT void QStandardItem_appendRow(QStandardItemH handle, PPtrIntArray items);
C_EXPORT void QStandardItem_appendRows(QStandardItemH handle, PPtrIntArray items);
C_EXPORT void QStandardItem_appendColumn(QStandardItemH handle, PPtrIntArray items);
C_EXPORT void QStandardItem_insertRow2(QStandardItemH handle, int row, QStandardItemH item);
C_EXPORT void QStandardItem_appendRow2(QStandardItemH handle, QStandardItemH item);
C_EXPORT QStandardItemH QStandardItem_takeChild(QStandardItemH handle, int row, int column);
C_EXPORT void QStandardItem_takeRow(QStandardItemH handle, PPtrIntArray retval, int row);
C_EXPORT void QStandardItem_takeColumn(QStandardItemH handle, PPtrIntArray retval, int column);
C_EXPORT void QStandardItem_sortChildren(QStandardItemH handle, int column, Qt::SortOrder order);
C_EXPORT QStandardItemH QStandardItem_clone(QStandardItemH handle);
C_EXPORT int QStandardItem_type(QStandardItemH handle);
C_EXPORT void QStandardItem_read(QStandardItemH handle, QDataStreamH in);
C_EXPORT void QStandardItem_write(QStandardItemH handle, QDataStreamH out);
C_EXPORT QStandardItemModelH QStandardItemModel_Create(QObjectH parent);
C_EXPORT void QStandardItemModel_Destroy(QStandardItemModelH handle);
C_EXPORT QStandardItemModelH QStandardItemModel_Create2(int rows, int columns, QObjectH parent);
C_EXPORT void QStandardItemModel_index(QStandardItemModelH handle, QModelIndexH retval, int row, int column, const QModelIndexH parent);
C_EXPORT void QStandardItemModel_parent(QStandardItemModelH handle, QModelIndexH retval, const QModelIndexH child);
C_EXPORT int QStandardItemModel_rowCount(QStandardItemModelH handle, const QModelIndexH parent);
C_EXPORT int QStandardItemModel_columnCount(QStandardItemModelH handle, const QModelIndexH parent);
C_EXPORT bool QStandardItemModel_hasChildren(QStandardItemModelH handle, const QModelIndexH parent);
C_EXPORT void QStandardItemModel_sibling(QStandardItemModelH handle, QModelIndexH retval, int row, int column, const QModelIndexH idx);
C_EXPORT void QStandardItemModel_data(QStandardItemModelH handle, QVariantH retval, const QModelIndexH index, Qt::ItemDataRole role);
C_EXPORT bool QStandardItemModel_setData(QStandardItemModelH handle, const QModelIndexH index, const QVariantH value, Qt::ItemDataRole role);
C_EXPORT void QStandardItemModel_headerData(QStandardItemModelH handle, QVariantH retval, int section, Qt::Orientation orientation, Qt::ItemDataRole role);
C_EXPORT bool QStandardItemModel_setHeaderData(QStandardItemModelH handle, int section, Qt::Orientation orientation, const QVariantH value, Qt::ItemDataRole role);
C_EXPORT bool QStandardItemModel_insertRows(QStandardItemModelH handle, int row, int count, const QModelIndexH parent);
C_EXPORT bool QStandardItemModel_insertColumns(QStandardItemModelH handle, int column, int count, const QModelIndexH parent);
C_EXPORT bool QStandardItemModel_removeRows(QStandardItemModelH handle, int row, int count, const QModelIndexH parent);
C_EXPORT bool QStandardItemModel_removeColumns(QStandardItemModelH handle, int column, int count, const QModelIndexH parent);
C_EXPORT unsigned int QStandardItemModel_flags(QStandardItemModelH handle, const QModelIndexH index);
C_EXPORT unsigned int QStandardItemModel_supportedDropActions(QStandardItemModelH handle);
C_EXPORT void QStandardItemModel_clear(QStandardItemModelH handle);
C_EXPORT void QStandardItemModel_sort(QStandardItemModelH handle, int column, Qt::SortOrder order);
C_EXPORT QStandardItemH QStandardItemModel_itemFromIndex(QStandardItemModelH handle, const QModelIndexH index);
C_EXPORT void QStandardItemModel_indexFromItem(QStandardItemModelH handle, QModelIndexH retval, const QStandardItemH item);
C_EXPORT QStandardItemH QStandardItemModel_item(QStandardItemModelH handle, int row, int column);
C_EXPORT void QStandardItemModel_setItem(QStandardItemModelH handle, int row, int column, QStandardItemH item);
C_EXPORT void QStandardItemModel_setItem2(QStandardItemModelH handle, int row, QStandardItemH item);
C_EXPORT QStandardItemH QStandardItemModel_invisibleRootItem(QStandardItemModelH handle);
C_EXPORT QStandardItemH QStandardItemModel_horizontalHeaderItem(QStandardItemModelH handle, int column);
C_EXPORT void QStandardItemModel_setHorizontalHeaderItem(QStandardItemModelH handle, int column, QStandardItemH item);
C_EXPORT QStandardItemH QStandardItemModel_verticalHeaderItem(QStandardItemModelH handle, int row);
C_EXPORT void QStandardItemModel_setVerticalHeaderItem(QStandardItemModelH handle, int row, QStandardItemH item);
C_EXPORT void QStandardItemModel_setHorizontalHeaderLabels(QStandardItemModelH handle, const QStringListH labels);
C_EXPORT void QStandardItemModel_setVerticalHeaderLabels(QStandardItemModelH handle, const QStringListH labels);
C_EXPORT void QStandardItemModel_setRowCount(QStandardItemModelH handle, int rows);
C_EXPORT void QStandardItemModel_setColumnCount(QStandardItemModelH handle, int columns);
C_EXPORT void QStandardItemModel_appendRow(QStandardItemModelH handle, PPtrIntArray items);
C_EXPORT void QStandardItemModel_appendColumn(QStandardItemModelH handle, PPtrIntArray items);
C_EXPORT void QStandardItemModel_appendRow2(QStandardItemModelH handle, QStandardItemH item);
C_EXPORT void QStandardItemModel_insertRow(QStandardItemModelH handle, int row, PPtrIntArray items);
C_EXPORT void QStandardItemModel_insertColumn(QStandardItemModelH handle, int column, PPtrIntArray items);
C_EXPORT void QStandardItemModel_insertRow2(QStandardItemModelH handle, int row, QStandardItemH item);
C_EXPORT bool QStandardItemModel_insertRow3(QStandardItemModelH handle, int row, const QModelIndexH parent);
C_EXPORT bool QStandardItemModel_insertColumn2(QStandardItemModelH handle, int column, const QModelIndexH parent);
C_EXPORT QStandardItemH QStandardItemModel_takeItem(QStandardItemModelH handle, int row, int column);
C_EXPORT void QStandardItemModel_takeRow(QStandardItemModelH handle, PPtrIntArray retval, int row);
C_EXPORT void QStandardItemModel_takeColumn(QStandardItemModelH handle, PPtrIntArray retval, int column);
C_EXPORT QStandardItemH QStandardItemModel_takeHorizontalHeaderItem(QStandardItemModelH handle, int column);
C_EXPORT QStandardItemH QStandardItemModel_takeVerticalHeaderItem(QStandardItemModelH handle, int row);
C_EXPORT const QStandardItemH QStandardItemModel_itemPrototype(QStandardItemModelH handle);
C_EXPORT void QStandardItemModel_setItemPrototype(QStandardItemModelH handle, const QStandardItemH item);
C_EXPORT void QStandardItemModel_findItems(QStandardItemModelH handle, PPtrIntArray retval, PWideString text, unsigned int flags, int column);
C_EXPORT int QStandardItemModel_sortRole(QStandardItemModelH handle);
C_EXPORT void QStandardItemModel_setSortRole(QStandardItemModelH handle, int role);
C_EXPORT void QStandardItemModel_mimeTypes(QStandardItemModelH handle, QStringListH retval);
C_EXPORT bool QStandardItemModel_dropMimeData(QStandardItemModelH handle, const QMimeDataH data, Qt::DropAction action, int row, int column, const QModelIndexH parent);

#endif
