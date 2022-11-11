//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qstandarditemmodel_c.h"

QStandardItemH QStandardItem_Create()
{
	return (QStandardItemH) new QStandardItem();
}

void QStandardItem_Destroy(QStandardItemH handle)
{
	delete (QStandardItem *)handle;
}

QStandardItemH QStandardItem_Create2(PWideString text)
{
	QString t_text;
	copyPWideStringToQString(text, t_text);
	return (QStandardItemH) new QStandardItem(t_text);
}

QStandardItemH QStandardItem_Create3(const QIconH icon, PWideString text)
{
	QString t_text;
	copyPWideStringToQString(text, t_text);
	return (QStandardItemH) new QStandardItem(*(const QIcon*)icon, t_text);
}

QStandardItemH QStandardItem_Create4(int rows, int columns)
{
	return (QStandardItemH) new QStandardItem(rows, columns);
}

void QStandardItem_data(QStandardItemH handle, QVariantH retval, Qt::ItemDataRole role)
{
	*(QVariant *)retval = ((QStandardItem *)handle)->data(role);
}

void QStandardItem_setData(QStandardItemH handle, const QVariantH value, Qt::ItemDataRole role)
{
	((QStandardItem *)handle)->setData(*(const QVariant*)value, role);
}

void QStandardItem_text(QStandardItemH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QStandardItem *)handle)->text();
	copyQStringToPWideString(t_retval, retval);
}

void QStandardItem_setText(QStandardItemH handle, PWideString text)
{
	QString t_text;
	copyPWideStringToQString(text, t_text);
	((QStandardItem *)handle)->setText(t_text);
}

void QStandardItem_icon(QStandardItemH handle, QIconH retval)
{
	*(QIcon *)retval = ((QStandardItem *)handle)->icon();
}

void QStandardItem_setIcon(QStandardItemH handle, const QIconH icon)
{
	((QStandardItem *)handle)->setIcon(*(const QIcon*)icon);
}

void QStandardItem_toolTip(QStandardItemH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QStandardItem *)handle)->toolTip();
	copyQStringToPWideString(t_retval, retval);
}

void QStandardItem_setToolTip(QStandardItemH handle, PWideString toolTip)
{
	QString t_toolTip;
	copyPWideStringToQString(toolTip, t_toolTip);
	((QStandardItem *)handle)->setToolTip(t_toolTip);
}

void QStandardItem_statusTip(QStandardItemH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QStandardItem *)handle)->statusTip();
	copyQStringToPWideString(t_retval, retval);
}

void QStandardItem_setStatusTip(QStandardItemH handle, PWideString statusTip)
{
	QString t_statusTip;
	copyPWideStringToQString(statusTip, t_statusTip);
	((QStandardItem *)handle)->setStatusTip(t_statusTip);
}

void QStandardItem_whatsThis(QStandardItemH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QStandardItem *)handle)->whatsThis();
	copyQStringToPWideString(t_retval, retval);
}

void QStandardItem_setWhatsThis(QStandardItemH handle, PWideString whatsThis)
{
	QString t_whatsThis;
	copyPWideStringToQString(whatsThis, t_whatsThis);
	((QStandardItem *)handle)->setWhatsThis(t_whatsThis);
}

void QStandardItem_sizeHint(QStandardItemH handle, PSize retval)
{
	*(QSize *)retval = ((QStandardItem *)handle)->sizeHint();
}

void QStandardItem_setSizeHint(QStandardItemH handle, const QSizeH sizeHint)
{
	((QStandardItem *)handle)->setSizeHint(*(const QSize*)sizeHint);
}

void QStandardItem_font(QStandardItemH handle, QFontH retval)
{
	*(QFont *)retval = ((QStandardItem *)handle)->font();
}

void QStandardItem_setFont(QStandardItemH handle, const QFontH font)
{
	((QStandardItem *)handle)->setFont(*(const QFont*)font);
}

unsigned int QStandardItem_textAlignment(QStandardItemH handle)
{
	return (unsigned int) ((QStandardItem *)handle)->textAlignment();
}

void QStandardItem_setTextAlignment(QStandardItemH handle, unsigned int textAlignment)
{
	((QStandardItem *)handle)->setTextAlignment((Qt::Alignment)textAlignment);
}

void QStandardItem_background(QStandardItemH handle, QBrushH retval)
{
	*(QBrush *)retval = ((QStandardItem *)handle)->background();
}

void QStandardItem_setBackground(QStandardItemH handle, const QBrushH brush)
{
	((QStandardItem *)handle)->setBackground(*(const QBrush*)brush);
}

void QStandardItem_foreground(QStandardItemH handle, QBrushH retval)
{
	*(QBrush *)retval = ((QStandardItem *)handle)->foreground();
}

void QStandardItem_setForeground(QStandardItemH handle, const QBrushH brush)
{
	((QStandardItem *)handle)->setForeground(*(const QBrush*)brush);
}

Qt::CheckState QStandardItem_checkState(QStandardItemH handle)
{
	return (Qt::CheckState) ((QStandardItem *)handle)->checkState();
}

void QStandardItem_setCheckState(QStandardItemH handle, Qt::CheckState checkState)
{
	((QStandardItem *)handle)->setCheckState(checkState);
}

void QStandardItem_accessibleText(QStandardItemH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QStandardItem *)handle)->accessibleText();
	copyQStringToPWideString(t_retval, retval);
}

void QStandardItem_setAccessibleText(QStandardItemH handle, PWideString accessibleText)
{
	QString t_accessibleText;
	copyPWideStringToQString(accessibleText, t_accessibleText);
	((QStandardItem *)handle)->setAccessibleText(t_accessibleText);
}

void QStandardItem_accessibleDescription(QStandardItemH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QStandardItem *)handle)->accessibleDescription();
	copyQStringToPWideString(t_retval, retval);
}

void QStandardItem_setAccessibleDescription(QStandardItemH handle, PWideString accessibleDescription)
{
	QString t_accessibleDescription;
	copyPWideStringToQString(accessibleDescription, t_accessibleDescription);
	((QStandardItem *)handle)->setAccessibleDescription(t_accessibleDescription);
}

unsigned int QStandardItem_flags(QStandardItemH handle)
{
	return (unsigned int) ((QStandardItem *)handle)->flags();
}

void QStandardItem_setFlags(QStandardItemH handle, unsigned int flags)
{
	((QStandardItem *)handle)->setFlags((Qt::ItemFlags)flags);
}

bool QStandardItem_isEnabled(QStandardItemH handle)
{
	return (bool) ((QStandardItem *)handle)->isEnabled();
}

void QStandardItem_setEnabled(QStandardItemH handle, bool enabled)
{
	((QStandardItem *)handle)->setEnabled(enabled);
}

bool QStandardItem_isEditable(QStandardItemH handle)
{
	return (bool) ((QStandardItem *)handle)->isEditable();
}

void QStandardItem_setEditable(QStandardItemH handle, bool editable)
{
	((QStandardItem *)handle)->setEditable(editable);
}

bool QStandardItem_isSelectable(QStandardItemH handle)
{
	return (bool) ((QStandardItem *)handle)->isSelectable();
}

void QStandardItem_setSelectable(QStandardItemH handle, bool selectable)
{
	((QStandardItem *)handle)->setSelectable(selectable);
}

bool QStandardItem_isCheckable(QStandardItemH handle)
{
	return (bool) ((QStandardItem *)handle)->isCheckable();
}

void QStandardItem_setCheckable(QStandardItemH handle, bool checkable)
{
	((QStandardItem *)handle)->setCheckable(checkable);
}

bool QStandardItem_isDragEnabled(QStandardItemH handle)
{
	return (bool) ((QStandardItem *)handle)->isDragEnabled();
}

void QStandardItem_setDragEnabled(QStandardItemH handle, bool dragEnabled)
{
	((QStandardItem *)handle)->setDragEnabled(dragEnabled);
}

bool QStandardItem_isDropEnabled(QStandardItemH handle)
{
	return (bool) ((QStandardItem *)handle)->isDropEnabled();
}

void QStandardItem_setDropEnabled(QStandardItemH handle, bool dropEnabled)
{
	((QStandardItem *)handle)->setDropEnabled(dropEnabled);
}

QStandardItemH QStandardItem_parent(QStandardItemH handle)
{
	return (QStandardItemH) ((QStandardItem *)handle)->parent();
}

int QStandardItem_row(QStandardItemH handle)
{
	return (int) ((QStandardItem *)handle)->row();
}

int QStandardItem_column(QStandardItemH handle)
{
	return (int) ((QStandardItem *)handle)->column();
}

void QStandardItem_index(QStandardItemH handle, QModelIndexH retval)
{
	*(QModelIndex *)retval = ((QStandardItem *)handle)->index();
}

QStandardItemModelH QStandardItem_model(QStandardItemH handle)
{
	return (QStandardItemModelH) ((QStandardItem *)handle)->model();
}

int QStandardItem_rowCount(QStandardItemH handle)
{
	return (int) ((QStandardItem *)handle)->rowCount();
}

void QStandardItem_setRowCount(QStandardItemH handle, int rows)
{
	((QStandardItem *)handle)->setRowCount(rows);
}

int QStandardItem_columnCount(QStandardItemH handle)
{
	return (int) ((QStandardItem *)handle)->columnCount();
}

void QStandardItem_setColumnCount(QStandardItemH handle, int columns)
{
	((QStandardItem *)handle)->setColumnCount(columns);
}

bool QStandardItem_hasChildren(QStandardItemH handle)
{
	return (bool) ((QStandardItem *)handle)->hasChildren();
}

QStandardItemH QStandardItem_child(QStandardItemH handle, int row, int column)
{
	return (QStandardItemH) ((QStandardItem *)handle)->child(row, column);
}

void QStandardItem_setChild(QStandardItemH handle, int row, int column, QStandardItemH item)
{
	((QStandardItem *)handle)->setChild(row, column, (QStandardItem*)item);
}

void QStandardItem_setChild2(QStandardItemH handle, int row, QStandardItemH item)
{
	((QStandardItem *)handle)->setChild(row, (QStandardItem*)item);
}

void QStandardItem_insertRow(QStandardItemH handle, int row, PPtrIntArray items)
{
	QList<QStandardItem*> t_items;
	copyPtrIntArrayToQListTemplate(items, t_items);
	((QStandardItem *)handle)->insertRow(row, t_items);
}

void QStandardItem_insertColumn(QStandardItemH handle, int column, PPtrIntArray items)
{
	QList<QStandardItem*> t_items;
	copyPtrIntArrayToQListTemplate(items, t_items);
	((QStandardItem *)handle)->insertColumn(column, t_items);
}

void QStandardItem_insertRows(QStandardItemH handle, int row, PPtrIntArray items)
{
	QList<QStandardItem*> t_items;
	copyPtrIntArrayToQListTemplate(items, t_items);
	((QStandardItem *)handle)->insertRows(row, t_items);
}

void QStandardItem_insertRows2(QStandardItemH handle, int row, int count)
{
	((QStandardItem *)handle)->insertRows(row, count);
}

void QStandardItem_insertColumns(QStandardItemH handle, int column, int count)
{
	((QStandardItem *)handle)->insertColumns(column, count);
}

void QStandardItem_removeRow(QStandardItemH handle, int row)
{
	((QStandardItem *)handle)->removeRow(row);
}

void QStandardItem_removeColumn(QStandardItemH handle, int column)
{
	((QStandardItem *)handle)->removeColumn(column);
}

void QStandardItem_removeRows(QStandardItemH handle, int row, int count)
{
	((QStandardItem *)handle)->removeRows(row, count);
}

void QStandardItem_removeColumns(QStandardItemH handle, int column, int count)
{
	((QStandardItem *)handle)->removeColumns(column, count);
}

void QStandardItem_appendRow(QStandardItemH handle, PPtrIntArray items)
{
	QList<QStandardItem*> t_items;
	copyPtrIntArrayToQListTemplate(items, t_items);
	((QStandardItem *)handle)->appendRow(t_items);
}

void QStandardItem_appendRows(QStandardItemH handle, PPtrIntArray items)
{
	QList<QStandardItem*> t_items;
	copyPtrIntArrayToQListTemplate(items, t_items);
	((QStandardItem *)handle)->appendRows(t_items);
}

void QStandardItem_appendColumn(QStandardItemH handle, PPtrIntArray items)
{
	QList<QStandardItem*> t_items;
	copyPtrIntArrayToQListTemplate(items, t_items);
	((QStandardItem *)handle)->appendColumn(t_items);
}

void QStandardItem_insertRow2(QStandardItemH handle, int row, QStandardItemH item)
{
	((QStandardItem *)handle)->insertRow(row, (QStandardItem*)item);
}

void QStandardItem_appendRow2(QStandardItemH handle, QStandardItemH item)
{
	((QStandardItem *)handle)->appendRow((QStandardItem*)item);
}

QStandardItemH QStandardItem_takeChild(QStandardItemH handle, int row, int column)
{
	return (QStandardItemH) ((QStandardItem *)handle)->takeChild(row, column);
}

void QStandardItem_takeRow(QStandardItemH handle, PPtrIntArray retval, int row)
{
	QList<QStandardItem*> t_retval;
	t_retval = ((QStandardItem *)handle)->takeRow(row);
	copyQListTemplateToPtrIntArray(t_retval, retval);
}

void QStandardItem_takeColumn(QStandardItemH handle, PPtrIntArray retval, int column)
{
	QList<QStandardItem*> t_retval;
	t_retval = ((QStandardItem *)handle)->takeColumn(column);
	copyQListTemplateToPtrIntArray(t_retval, retval);
}

void QStandardItem_sortChildren(QStandardItemH handle, int column, Qt::SortOrder order)
{
	((QStandardItem *)handle)->sortChildren(column, order);
}

QStandardItemH QStandardItem_clone(QStandardItemH handle)
{
	return (QStandardItemH) ((QStandardItem *)handle)->clone();
}

int QStandardItem_type(QStandardItemH handle)
{
	return (int) ((QStandardItem *)handle)->type();
}

void QStandardItem_read(QStandardItemH handle, QDataStreamH in)
{
	((QStandardItem *)handle)->read(*(QDataStream*)in);
}

void QStandardItem_write(QStandardItemH handle, QDataStreamH out)
{
	((QStandardItem *)handle)->write(*(QDataStream*)out);
}

QStandardItemModelH QStandardItemModel_Create(QObjectH parent)
{
	return (QStandardItemModelH) new QStandardItemModel((QObject*)parent);
}

void QStandardItemModel_Destroy(QStandardItemModelH handle)
{
	delete (QStandardItemModel *)handle;
}

QStandardItemModelH QStandardItemModel_Create2(int rows, int columns, QObjectH parent)
{
	return (QStandardItemModelH) new QStandardItemModel(rows, columns, (QObject*)parent);
}

void QStandardItemModel_index(QStandardItemModelH handle, QModelIndexH retval, int row, int column, const QModelIndexH parent)
{
	*(QModelIndex *)retval = ((QStandardItemModel *)handle)->index(row, column, *(const QModelIndex*)parent);
}

void QStandardItemModel_parent(QStandardItemModelH handle, QModelIndexH retval, const QModelIndexH child)
{
	*(QModelIndex *)retval = ((QStandardItemModel *)handle)->parent(*(const QModelIndex*)child);
}

int QStandardItemModel_rowCount(QStandardItemModelH handle, const QModelIndexH parent)
{
	return (int) ((QStandardItemModel *)handle)->rowCount(*(const QModelIndex*)parent);
}

int QStandardItemModel_columnCount(QStandardItemModelH handle, const QModelIndexH parent)
{
	return (int) ((QStandardItemModel *)handle)->columnCount(*(const QModelIndex*)parent);
}

bool QStandardItemModel_hasChildren(QStandardItemModelH handle, const QModelIndexH parent)
{
	return (bool) ((QStandardItemModel *)handle)->hasChildren(*(const QModelIndex*)parent);
}

void QStandardItemModel_sibling(QStandardItemModelH handle, QModelIndexH retval, int row, int column, const QModelIndexH idx)
{
	*(QModelIndex *)retval = ((QStandardItemModel *)handle)->sibling(row, column, *(const QModelIndex*)idx);
}

void QStandardItemModel_data(QStandardItemModelH handle, QVariantH retval, const QModelIndexH index, Qt::ItemDataRole role)
{
	*(QVariant *)retval = ((QStandardItemModel *)handle)->data(*(const QModelIndex*)index, role);
}

bool QStandardItemModel_setData(QStandardItemModelH handle, const QModelIndexH index, const QVariantH value, Qt::ItemDataRole role)
{
	return (bool) ((QStandardItemModel *)handle)->setData(*(const QModelIndex*)index, *(const QVariant*)value, role);
}

void QStandardItemModel_headerData(QStandardItemModelH handle, QVariantH retval, int section, Qt::Orientation orientation, Qt::ItemDataRole role)
{
	*(QVariant *)retval = ((QStandardItemModel *)handle)->headerData(section, orientation, role);
}

bool QStandardItemModel_setHeaderData(QStandardItemModelH handle, int section, Qt::Orientation orientation, const QVariantH value, Qt::ItemDataRole role)
{
	return (bool) ((QStandardItemModel *)handle)->setHeaderData(section, orientation, *(const QVariant*)value, role);
}

bool QStandardItemModel_insertRows(QStandardItemModelH handle, int row, int count, const QModelIndexH parent)
{
	return (bool) ((QStandardItemModel *)handle)->insertRows(row, count, *(const QModelIndex*)parent);
}

bool QStandardItemModel_insertColumns(QStandardItemModelH handle, int column, int count, const QModelIndexH parent)
{
	return (bool) ((QStandardItemModel *)handle)->insertColumns(column, count, *(const QModelIndex*)parent);
}

bool QStandardItemModel_removeRows(QStandardItemModelH handle, int row, int count, const QModelIndexH parent)
{
	return (bool) ((QStandardItemModel *)handle)->removeRows(row, count, *(const QModelIndex*)parent);
}

bool QStandardItemModel_removeColumns(QStandardItemModelH handle, int column, int count, const QModelIndexH parent)
{
	return (bool) ((QStandardItemModel *)handle)->removeColumns(column, count, *(const QModelIndex*)parent);
}

unsigned int QStandardItemModel_flags(QStandardItemModelH handle, const QModelIndexH index)
{
	return (unsigned int) ((QStandardItemModel *)handle)->flags(*(const QModelIndex*)index);
}

unsigned int QStandardItemModel_supportedDropActions(QStandardItemModelH handle)
{
	return (unsigned int) ((QStandardItemModel *)handle)->supportedDropActions();
}

void QStandardItemModel_clear(QStandardItemModelH handle)
{
	((QStandardItemModel *)handle)->clear();
}

void QStandardItemModel_sort(QStandardItemModelH handle, int column, Qt::SortOrder order)
{
	((QStandardItemModel *)handle)->sort(column, order);
}

QStandardItemH QStandardItemModel_itemFromIndex(QStandardItemModelH handle, const QModelIndexH index)
{
	return (QStandardItemH) ((QStandardItemModel *)handle)->itemFromIndex(*(const QModelIndex*)index);
}

void QStandardItemModel_indexFromItem(QStandardItemModelH handle, QModelIndexH retval, const QStandardItemH item)
{
	*(QModelIndex *)retval = ((QStandardItemModel *)handle)->indexFromItem((const QStandardItem*)item);
}

QStandardItemH QStandardItemModel_item(QStandardItemModelH handle, int row, int column)
{
	return (QStandardItemH) ((QStandardItemModel *)handle)->item(row, column);
}

void QStandardItemModel_setItem(QStandardItemModelH handle, int row, int column, QStandardItemH item)
{
	((QStandardItemModel *)handle)->setItem(row, column, (QStandardItem*)item);
}

void QStandardItemModel_setItem2(QStandardItemModelH handle, int row, QStandardItemH item)
{
	((QStandardItemModel *)handle)->setItem(row, (QStandardItem*)item);
}

QStandardItemH QStandardItemModel_invisibleRootItem(QStandardItemModelH handle)
{
	return (QStandardItemH) ((QStandardItemModel *)handle)->invisibleRootItem();
}

QStandardItemH QStandardItemModel_horizontalHeaderItem(QStandardItemModelH handle, int column)
{
	return (QStandardItemH) ((QStandardItemModel *)handle)->horizontalHeaderItem(column);
}

void QStandardItemModel_setHorizontalHeaderItem(QStandardItemModelH handle, int column, QStandardItemH item)
{
	((QStandardItemModel *)handle)->setHorizontalHeaderItem(column, (QStandardItem*)item);
}

QStandardItemH QStandardItemModel_verticalHeaderItem(QStandardItemModelH handle, int row)
{
	return (QStandardItemH) ((QStandardItemModel *)handle)->verticalHeaderItem(row);
}

void QStandardItemModel_setVerticalHeaderItem(QStandardItemModelH handle, int row, QStandardItemH item)
{
	((QStandardItemModel *)handle)->setVerticalHeaderItem(row, (QStandardItem*)item);
}

void QStandardItemModel_setHorizontalHeaderLabels(QStandardItemModelH handle, const QStringListH labels)
{
	((QStandardItemModel *)handle)->setHorizontalHeaderLabels(*(const QStringList*)labels);
}

void QStandardItemModel_setVerticalHeaderLabels(QStandardItemModelH handle, const QStringListH labels)
{
	((QStandardItemModel *)handle)->setVerticalHeaderLabels(*(const QStringList*)labels);
}

void QStandardItemModel_setRowCount(QStandardItemModelH handle, int rows)
{
	((QStandardItemModel *)handle)->setRowCount(rows);
}

void QStandardItemModel_setColumnCount(QStandardItemModelH handle, int columns)
{
	((QStandardItemModel *)handle)->setColumnCount(columns);
}

void QStandardItemModel_appendRow(QStandardItemModelH handle, PPtrIntArray items)
{
	QList<QStandardItem*> t_items;
	copyPtrIntArrayToQListTemplate(items, t_items);
	((QStandardItemModel *)handle)->appendRow(t_items);
}

void QStandardItemModel_appendColumn(QStandardItemModelH handle, PPtrIntArray items)
{
	QList<QStandardItem*> t_items;
	copyPtrIntArrayToQListTemplate(items, t_items);
	((QStandardItemModel *)handle)->appendColumn(t_items);
}

void QStandardItemModel_appendRow2(QStandardItemModelH handle, QStandardItemH item)
{
	((QStandardItemModel *)handle)->appendRow((QStandardItem*)item);
}

void QStandardItemModel_insertRow(QStandardItemModelH handle, int row, PPtrIntArray items)
{
	QList<QStandardItem*> t_items;
	copyPtrIntArrayToQListTemplate(items, t_items);
	((QStandardItemModel *)handle)->insertRow(row, t_items);
}

void QStandardItemModel_insertColumn(QStandardItemModelH handle, int column, PPtrIntArray items)
{
	QList<QStandardItem*> t_items;
	copyPtrIntArrayToQListTemplate(items, t_items);
	((QStandardItemModel *)handle)->insertColumn(column, t_items);
}

void QStandardItemModel_insertRow2(QStandardItemModelH handle, int row, QStandardItemH item)
{
	((QStandardItemModel *)handle)->insertRow(row, (QStandardItem*)item);
}

bool QStandardItemModel_insertRow3(QStandardItemModelH handle, int row, const QModelIndexH parent)
{
	return (bool) ((QStandardItemModel *)handle)->insertRow(row, *(const QModelIndex*)parent);
}

bool QStandardItemModel_insertColumn2(QStandardItemModelH handle, int column, const QModelIndexH parent)
{
	return (bool) ((QStandardItemModel *)handle)->insertColumn(column, *(const QModelIndex*)parent);
}

QStandardItemH QStandardItemModel_takeItem(QStandardItemModelH handle, int row, int column)
{
	return (QStandardItemH) ((QStandardItemModel *)handle)->takeItem(row, column);
}

void QStandardItemModel_takeRow(QStandardItemModelH handle, PPtrIntArray retval, int row)
{
	QList<QStandardItem*> t_retval;
	t_retval = ((QStandardItemModel *)handle)->takeRow(row);
	copyQListTemplateToPtrIntArray(t_retval, retval);
}

void QStandardItemModel_takeColumn(QStandardItemModelH handle, PPtrIntArray retval, int column)
{
	QList<QStandardItem*> t_retval;
	t_retval = ((QStandardItemModel *)handle)->takeColumn(column);
	copyQListTemplateToPtrIntArray(t_retval, retval);
}

QStandardItemH QStandardItemModel_takeHorizontalHeaderItem(QStandardItemModelH handle, int column)
{
	return (QStandardItemH) ((QStandardItemModel *)handle)->takeHorizontalHeaderItem(column);
}

QStandardItemH QStandardItemModel_takeVerticalHeaderItem(QStandardItemModelH handle, int row)
{
	return (QStandardItemH) ((QStandardItemModel *)handle)->takeVerticalHeaderItem(row);
}

const QStandardItemH QStandardItemModel_itemPrototype(QStandardItemModelH handle)
{
	return (const QStandardItemH) ((QStandardItemModel *)handle)->itemPrototype();
}

void QStandardItemModel_setItemPrototype(QStandardItemModelH handle, const QStandardItemH item)
{
	((QStandardItemModel *)handle)->setItemPrototype((const QStandardItem*)item);
}

void QStandardItemModel_findItems(QStandardItemModelH handle, PPtrIntArray retval, PWideString text, unsigned int flags, int column)
{
	QList<QStandardItem*> t_retval;
	QString t_text;
	copyPWideStringToQString(text, t_text);
	t_retval = ((QStandardItemModel *)handle)->findItems(t_text, (Qt::MatchFlags)flags, column);
	copyQListTemplateToPtrIntArray(t_retval, retval);
}

int QStandardItemModel_sortRole(QStandardItemModelH handle)
{
	return (int) ((QStandardItemModel *)handle)->sortRole();
}

void QStandardItemModel_setSortRole(QStandardItemModelH handle, int role)
{
	((QStandardItemModel *)handle)->setSortRole(role);
}

void QStandardItemModel_mimeTypes(QStandardItemModelH handle, QStringListH retval)
{
	*(QStringList *)retval = ((QStandardItemModel *)handle)->mimeTypes();
}

bool QStandardItemModel_dropMimeData(QStandardItemModelH handle, const QMimeDataH data, Qt::DropAction action, int row, int column, const QModelIndexH parent)
{
	return (bool) ((QStandardItemModel *)handle)->dropMimeData((const QMimeData*)data, action, row, column, *(const QModelIndex*)parent);
}

