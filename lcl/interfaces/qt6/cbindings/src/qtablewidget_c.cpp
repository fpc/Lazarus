//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qtablewidget_c.h"

QTableWidgetSelectionRangeH QTableWidgetSelectionRange_Create()
{
	return (QTableWidgetSelectionRangeH) new QTableWidgetSelectionRange();
}

void QTableWidgetSelectionRange_Destroy(QTableWidgetSelectionRangeH handle)
{
	delete (QTableWidgetSelectionRange *)handle;
}

QTableWidgetSelectionRangeH QTableWidgetSelectionRange_Create2(int top, int left, int bottom, int right)
{
	return (QTableWidgetSelectionRangeH) new QTableWidgetSelectionRange(top, left, bottom, right);
}

QTableWidgetSelectionRangeH QTableWidgetSelectionRange_Create3(const QTableWidgetSelectionRangeH other)
{
	return (QTableWidgetSelectionRangeH) new QTableWidgetSelectionRange(*(const QTableWidgetSelectionRange*)other);
}

int QTableWidgetSelectionRange_topRow(QTableWidgetSelectionRangeH handle)
{
	return (int) ((QTableWidgetSelectionRange *)handle)->topRow();
}

int QTableWidgetSelectionRange_bottomRow(QTableWidgetSelectionRangeH handle)
{
	return (int) ((QTableWidgetSelectionRange *)handle)->bottomRow();
}

int QTableWidgetSelectionRange_leftColumn(QTableWidgetSelectionRangeH handle)
{
	return (int) ((QTableWidgetSelectionRange *)handle)->leftColumn();
}

int QTableWidgetSelectionRange_rightColumn(QTableWidgetSelectionRangeH handle)
{
	return (int) ((QTableWidgetSelectionRange *)handle)->rightColumn();
}

int QTableWidgetSelectionRange_rowCount(QTableWidgetSelectionRangeH handle)
{
	return (int) ((QTableWidgetSelectionRange *)handle)->rowCount();
}

int QTableWidgetSelectionRange_columnCount(QTableWidgetSelectionRangeH handle)
{
	return (int) ((QTableWidgetSelectionRange *)handle)->columnCount();
}

QTableWidgetItemH QTableWidgetItem_Create(int type)
{
	return (QTableWidgetItemH) new QTableWidgetItem(type);
}

void QTableWidgetItem_Destroy(QTableWidgetItemH handle)
{
	delete (QTableWidgetItem *)handle;
}

QTableWidgetItemH QTableWidgetItem_Create2(PWideString text, int type)
{
	QString t_text;
	copyPWideStringToQString(text, t_text);
	return (QTableWidgetItemH) new QTableWidgetItem(t_text, type);
}

QTableWidgetItemH QTableWidgetItem_Create3(const QIconH icon, PWideString text, int type)
{
	QString t_text;
	copyPWideStringToQString(text, t_text);
	return (QTableWidgetItemH) new QTableWidgetItem(*(const QIcon*)icon, t_text, type);
}

QTableWidgetItemH QTableWidgetItem_Create4(const QTableWidgetItemH other)
{
	return (QTableWidgetItemH) new QTableWidgetItem(*(const QTableWidgetItem*)other);
}

QTableWidgetItemH QTableWidgetItem_clone(QTableWidgetItemH handle)
{
	return (QTableWidgetItemH) ((QTableWidgetItem *)handle)->clone();
}

QTableWidgetH QTableWidgetItem_tableWidget(QTableWidgetItemH handle)
{
	return (QTableWidgetH) ((QTableWidgetItem *)handle)->tableWidget();
}

int QTableWidgetItem_row(QTableWidgetItemH handle)
{
	return (int) ((QTableWidgetItem *)handle)->row();
}

int QTableWidgetItem_column(QTableWidgetItemH handle)
{
	return (int) ((QTableWidgetItem *)handle)->column();
}

void QTableWidgetItem_setSelected(QTableWidgetItemH handle, bool select)
{
	((QTableWidgetItem *)handle)->setSelected(select);
}

bool QTableWidgetItem_isSelected(QTableWidgetItemH handle)
{
	return (bool) ((QTableWidgetItem *)handle)->isSelected();
}

unsigned int QTableWidgetItem_flags(QTableWidgetItemH handle)
{
	return (unsigned int) ((QTableWidgetItem *)handle)->flags();
}

void QTableWidgetItem_setFlags(QTableWidgetItemH handle, unsigned int flags)
{
	((QTableWidgetItem *)handle)->setFlags((Qt::ItemFlags)flags);
}

void QTableWidgetItem_text(QTableWidgetItemH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QTableWidgetItem *)handle)->text();
	copyQStringToPWideString(t_retval, retval);
}

void QTableWidgetItem_setText(QTableWidgetItemH handle, PWideString text)
{
	QString t_text;
	copyPWideStringToQString(text, t_text);
	((QTableWidgetItem *)handle)->setText(t_text);
}

void QTableWidgetItem_icon(QTableWidgetItemH handle, QIconH retval)
{
	*(QIcon *)retval = ((QTableWidgetItem *)handle)->icon();
}

void QTableWidgetItem_setIcon(QTableWidgetItemH handle, const QIconH icon)
{
	((QTableWidgetItem *)handle)->setIcon(*(const QIcon*)icon);
}

void QTableWidgetItem_statusTip(QTableWidgetItemH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QTableWidgetItem *)handle)->statusTip();
	copyQStringToPWideString(t_retval, retval);
}

void QTableWidgetItem_setStatusTip(QTableWidgetItemH handle, PWideString statusTip)
{
	QString t_statusTip;
	copyPWideStringToQString(statusTip, t_statusTip);
	((QTableWidgetItem *)handle)->setStatusTip(t_statusTip);
}

void QTableWidgetItem_toolTip(QTableWidgetItemH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QTableWidgetItem *)handle)->toolTip();
	copyQStringToPWideString(t_retval, retval);
}

void QTableWidgetItem_setToolTip(QTableWidgetItemH handle, PWideString toolTip)
{
	QString t_toolTip;
	copyPWideStringToQString(toolTip, t_toolTip);
	((QTableWidgetItem *)handle)->setToolTip(t_toolTip);
}

void QTableWidgetItem_whatsThis(QTableWidgetItemH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QTableWidgetItem *)handle)->whatsThis();
	copyQStringToPWideString(t_retval, retval);
}

void QTableWidgetItem_setWhatsThis(QTableWidgetItemH handle, PWideString whatsThis)
{
	QString t_whatsThis;
	copyPWideStringToQString(whatsThis, t_whatsThis);
	((QTableWidgetItem *)handle)->setWhatsThis(t_whatsThis);
}

void QTableWidgetItem_font(QTableWidgetItemH handle, QFontH retval)
{
	*(QFont *)retval = ((QTableWidgetItem *)handle)->font();
}

void QTableWidgetItem_setFont(QTableWidgetItemH handle, const QFontH font)
{
	((QTableWidgetItem *)handle)->setFont(*(const QFont*)font);
}

int QTableWidgetItem_textAlignment(QTableWidgetItemH handle)
{
	return (int) ((QTableWidgetItem *)handle)->textAlignment();
}

void QTableWidgetItem_setTextAlignment(QTableWidgetItemH handle, int alignment)
{
	((QTableWidgetItem *)handle)->setTextAlignment(alignment);
}

void QTableWidgetItem_background(QTableWidgetItemH handle, QBrushH retval)
{
	*(QBrush *)retval = ((QTableWidgetItem *)handle)->background();
}

void QTableWidgetItem_setBackground(QTableWidgetItemH handle, const QBrushH brush)
{
	((QTableWidgetItem *)handle)->setBackground(*(const QBrush*)brush);
}

void QTableWidgetItem_foreground(QTableWidgetItemH handle, QBrushH retval)
{
	*(QBrush *)retval = ((QTableWidgetItem *)handle)->foreground();
}

void QTableWidgetItem_setForeground(QTableWidgetItemH handle, const QBrushH brush)
{
	((QTableWidgetItem *)handle)->setForeground(*(const QBrush*)brush);
}

Qt::CheckState QTableWidgetItem_checkState(QTableWidgetItemH handle)
{
	return (Qt::CheckState) ((QTableWidgetItem *)handle)->checkState();
}

void QTableWidgetItem_setCheckState(QTableWidgetItemH handle, Qt::CheckState state)
{
	((QTableWidgetItem *)handle)->setCheckState(state);
}

void QTableWidgetItem_sizeHint(QTableWidgetItemH handle, PSize retval)
{
	*(QSize *)retval = ((QTableWidgetItem *)handle)->sizeHint();
}

void QTableWidgetItem_setSizeHint(QTableWidgetItemH handle, const QSizeH size)
{
	((QTableWidgetItem *)handle)->setSizeHint(*(const QSize*)size);
}

void QTableWidgetItem_data(QTableWidgetItemH handle, QVariantH retval, int role)
{
	*(QVariant *)retval = ((QTableWidgetItem *)handle)->data(role);
}

void QTableWidgetItem_setData(QTableWidgetItemH handle, int role, const QVariantH value)
{
	((QTableWidgetItem *)handle)->setData(role, *(const QVariant*)value);
}

void QTableWidgetItem_read(QTableWidgetItemH handle, QDataStreamH in)
{
	((QTableWidgetItem *)handle)->read(*(QDataStream*)in);
}

void QTableWidgetItem_write(QTableWidgetItemH handle, QDataStreamH out)
{
	((QTableWidgetItem *)handle)->write(*(QDataStream*)out);
}

int QTableWidgetItem_type(QTableWidgetItemH handle)
{
	return (int) ((QTableWidgetItem *)handle)->type();
}

QTableWidgetH QTableWidget_Create(QWidgetH parent)
{
	return (QTableWidgetH) new QTableWidget((QWidget*)parent);
}

void QTableWidget_Destroy(QTableWidgetH handle)
{
	delete (QTableWidget *)handle;
}

QTableWidgetH QTableWidget_Create2(int rows, int columns, QWidgetH parent)
{
	return (QTableWidgetH) new QTableWidget(rows, columns, (QWidget*)parent);
}

void QTableWidget_setRowCount(QTableWidgetH handle, int rows)
{
	((QTableWidget *)handle)->setRowCount(rows);
}

int QTableWidget_rowCount(QTableWidgetH handle)
{
	return (int) ((QTableWidget *)handle)->rowCount();
}

void QTableWidget_setColumnCount(QTableWidgetH handle, int columns)
{
	((QTableWidget *)handle)->setColumnCount(columns);
}

int QTableWidget_columnCount(QTableWidgetH handle)
{
	return (int) ((QTableWidget *)handle)->columnCount();
}

int QTableWidget_row(QTableWidgetH handle, const QTableWidgetItemH item)
{
	return (int) ((QTableWidget *)handle)->row((const QTableWidgetItem*)item);
}

int QTableWidget_column(QTableWidgetH handle, const QTableWidgetItemH item)
{
	return (int) ((QTableWidget *)handle)->column((const QTableWidgetItem*)item);
}

QTableWidgetItemH QTableWidget_item(QTableWidgetH handle, int row, int column)
{
	return (QTableWidgetItemH) ((QTableWidget *)handle)->item(row, column);
}

void QTableWidget_setItem(QTableWidgetH handle, int row, int column, QTableWidgetItemH item)
{
	((QTableWidget *)handle)->setItem(row, column, (QTableWidgetItem*)item);
}

QTableWidgetItemH QTableWidget_takeItem(QTableWidgetH handle, int row, int column)
{
	return (QTableWidgetItemH) ((QTableWidget *)handle)->takeItem(row, column);
}

QTableWidgetItemH QTableWidget_verticalHeaderItem(QTableWidgetH handle, int row)
{
	return (QTableWidgetItemH) ((QTableWidget *)handle)->verticalHeaderItem(row);
}

void QTableWidget_setVerticalHeaderItem(QTableWidgetH handle, int row, QTableWidgetItemH item)
{
	((QTableWidget *)handle)->setVerticalHeaderItem(row, (QTableWidgetItem*)item);
}

QTableWidgetItemH QTableWidget_takeVerticalHeaderItem(QTableWidgetH handle, int row)
{
	return (QTableWidgetItemH) ((QTableWidget *)handle)->takeVerticalHeaderItem(row);
}

QTableWidgetItemH QTableWidget_horizontalHeaderItem(QTableWidgetH handle, int column)
{
	return (QTableWidgetItemH) ((QTableWidget *)handle)->horizontalHeaderItem(column);
}

void QTableWidget_setHorizontalHeaderItem(QTableWidgetH handle, int column, QTableWidgetItemH item)
{
	((QTableWidget *)handle)->setHorizontalHeaderItem(column, (QTableWidgetItem*)item);
}

QTableWidgetItemH QTableWidget_takeHorizontalHeaderItem(QTableWidgetH handle, int column)
{
	return (QTableWidgetItemH) ((QTableWidget *)handle)->takeHorizontalHeaderItem(column);
}

void QTableWidget_setVerticalHeaderLabels(QTableWidgetH handle, const QStringListH labels)
{
	((QTableWidget *)handle)->setVerticalHeaderLabels(*(const QStringList*)labels);
}

void QTableWidget_setHorizontalHeaderLabels(QTableWidgetH handle, const QStringListH labels)
{
	((QTableWidget *)handle)->setHorizontalHeaderLabels(*(const QStringList*)labels);
}

int QTableWidget_currentRow(QTableWidgetH handle)
{
	return (int) ((QTableWidget *)handle)->currentRow();
}

int QTableWidget_currentColumn(QTableWidgetH handle)
{
	return (int) ((QTableWidget *)handle)->currentColumn();
}

QTableWidgetItemH QTableWidget_currentItem(QTableWidgetH handle)
{
	return (QTableWidgetItemH) ((QTableWidget *)handle)->currentItem();
}

void QTableWidget_setCurrentItem(QTableWidgetH handle, QTableWidgetItemH item)
{
	((QTableWidget *)handle)->setCurrentItem((QTableWidgetItem*)item);
}

void QTableWidget_setCurrentItem2(QTableWidgetH handle, QTableWidgetItemH item, unsigned int command)
{
	((QTableWidget *)handle)->setCurrentItem((QTableWidgetItem*)item, (QItemSelectionModel::SelectionFlags)command);
}

void QTableWidget_setCurrentCell(QTableWidgetH handle, int row, int column)
{
	((QTableWidget *)handle)->setCurrentCell(row, column);
}

void QTableWidget_setCurrentCell2(QTableWidgetH handle, int row, int column, unsigned int command)
{
	((QTableWidget *)handle)->setCurrentCell(row, column, (QItemSelectionModel::SelectionFlags)command);
}

void QTableWidget_sortItems(QTableWidgetH handle, int column, Qt::SortOrder order)
{
	((QTableWidget *)handle)->sortItems(column, order);
}

void QTableWidget_setSortingEnabled(QTableWidgetH handle, bool enable)
{
	((QTableWidget *)handle)->setSortingEnabled(enable);
}

bool QTableWidget_isSortingEnabled(QTableWidgetH handle)
{
	return (bool) ((QTableWidget *)handle)->isSortingEnabled();
}

void QTableWidget_editItem(QTableWidgetH handle, QTableWidgetItemH item)
{
	((QTableWidget *)handle)->editItem((QTableWidgetItem*)item);
}

void QTableWidget_openPersistentEditor(QTableWidgetH handle, QTableWidgetItemH item)
{
	((QTableWidget *)handle)->openPersistentEditor((QTableWidgetItem*)item);
}

void QTableWidget_closePersistentEditor(QTableWidgetH handle, QTableWidgetItemH item)
{
	((QTableWidget *)handle)->closePersistentEditor((QTableWidgetItem*)item);
}

QWidgetH QTableWidget_cellWidget(QTableWidgetH handle, int row, int column)
{
	return (QWidgetH) ((QTableWidget *)handle)->cellWidget(row, column);
}

void QTableWidget_setCellWidget(QTableWidgetH handle, int row, int column, QWidgetH widget)
{
	((QTableWidget *)handle)->setCellWidget(row, column, (QWidget*)widget);
}

void QTableWidget_removeCellWidget(QTableWidgetH handle, int row, int column)
{
	((QTableWidget *)handle)->removeCellWidget(row, column);
}

void QTableWidget_setRangeSelected(QTableWidgetH handle, const QTableWidgetSelectionRangeH range, bool select)
{
	((QTableWidget *)handle)->setRangeSelected(*(const QTableWidgetSelectionRange*)range, select);
}

void QTableWidget_selectedItems(QTableWidgetH handle, PPtrIntArray retval)
{
	QList<QTableWidgetItem*> t_retval;
	t_retval = ((QTableWidget *)handle)->selectedItems();
	copyQListTemplateToPtrIntArray(t_retval, retval);
}

void QTableWidget_findItems(QTableWidgetH handle, PPtrIntArray retval, PWideString text, unsigned int flags)
{
	QList<QTableWidgetItem*> t_retval;
	QString t_text;
	copyPWideStringToQString(text, t_text);
	t_retval = ((QTableWidget *)handle)->findItems(t_text, (Qt::MatchFlags)flags);
	copyQListTemplateToPtrIntArray(t_retval, retval);
}

int QTableWidget_visualRow(QTableWidgetH handle, int logicalRow)
{
	return (int) ((QTableWidget *)handle)->visualRow(logicalRow);
}

int QTableWidget_visualColumn(QTableWidgetH handle, int logicalColumn)
{
	return (int) ((QTableWidget *)handle)->visualColumn(logicalColumn);
}

QTableWidgetItemH QTableWidget_itemAt(QTableWidgetH handle, const QPointH p)
{
	return (QTableWidgetItemH) ((QTableWidget *)handle)->itemAt(*(const QPoint*)p);
}

QTableWidgetItemH QTableWidget_itemAt2(QTableWidgetH handle, int x, int y)
{
	return (QTableWidgetItemH) ((QTableWidget *)handle)->itemAt(x, y);
}

void QTableWidget_visualItemRect(QTableWidgetH handle, PRect retval, const QTableWidgetItemH item)
{
	QRect t_retval;
	t_retval = ((QTableWidget *)handle)->visualItemRect((const QTableWidgetItem*)item);
	copyQRectToPRect(t_retval, retval);
}

const QTableWidgetItemH QTableWidget_itemPrototype(QTableWidgetH handle)
{
	return (const QTableWidgetItemH) ((QTableWidget *)handle)->itemPrototype();
}

void QTableWidget_setItemPrototype(QTableWidgetH handle, const QTableWidgetItemH item)
{
	((QTableWidget *)handle)->setItemPrototype((const QTableWidgetItem*)item);
}

void QTableWidget_scrollToItem(QTableWidgetH handle, const QTableWidgetItemH item, QAbstractItemView::ScrollHint hint)
{
	((QTableWidget *)handle)->scrollToItem((const QTableWidgetItem*)item, hint);
}

void QTableWidget_insertRow(QTableWidgetH handle, int row)
{
	((QTableWidget *)handle)->insertRow(row);
}

void QTableWidget_insertColumn(QTableWidgetH handle, int column)
{
	((QTableWidget *)handle)->insertColumn(column);
}

void QTableWidget_removeRow(QTableWidgetH handle, int row)
{
	((QTableWidget *)handle)->removeRow(row);
}

void QTableWidget_removeColumn(QTableWidgetH handle, int column)
{
	((QTableWidget *)handle)->removeColumn(column);
}

void QTableWidget_clear(QTableWidgetH handle)
{
	((QTableWidget *)handle)->clear();
}

void QTableWidget_clearContents(QTableWidgetH handle)
{
	((QTableWidget *)handle)->clearContents();
}

