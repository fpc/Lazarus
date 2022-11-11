//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qlistwidget_c.h"

QListWidgetItemH QListWidgetItem_Create(QListWidgetH view, int type)
{
	return (QListWidgetItemH) new QListWidgetItem((QListWidget*)view, type);
}

void QListWidgetItem_Destroy(QListWidgetItemH handle)
{
	delete (QListWidgetItem *)handle;
}

QListWidgetItemH QListWidgetItem_Create2(PWideString text, QListWidgetH view, int type)
{
	QString t_text;
	copyPWideStringToQString(text, t_text);
	return (QListWidgetItemH) new QListWidgetItem(t_text, (QListWidget*)view, type);
}

QListWidgetItemH QListWidgetItem_Create3(const QIconH icon, PWideString text, QListWidgetH view, int type)
{
	QString t_text;
	copyPWideStringToQString(text, t_text);
	return (QListWidgetItemH) new QListWidgetItem(*(const QIcon*)icon, t_text, (QListWidget*)view, type);
}

QListWidgetItemH QListWidgetItem_Create4(const QListWidgetItemH other)
{
	return (QListWidgetItemH) new QListWidgetItem(*(const QListWidgetItem*)other);
}

QListWidgetItemH QListWidgetItem_clone(QListWidgetItemH handle)
{
	return (QListWidgetItemH) ((QListWidgetItem *)handle)->clone();
}

QListWidgetH QListWidgetItem_listWidget(QListWidgetItemH handle)
{
	return (QListWidgetH) ((QListWidgetItem *)handle)->listWidget();
}

void QListWidgetItem_setSelected(QListWidgetItemH handle, bool select)
{
	((QListWidgetItem *)handle)->setSelected(select);
}

bool QListWidgetItem_isSelected(QListWidgetItemH handle)
{
	return (bool) ((QListWidgetItem *)handle)->isSelected();
}

void QListWidgetItem_setHidden(QListWidgetItemH handle, bool hide)
{
	((QListWidgetItem *)handle)->setHidden(hide);
}

bool QListWidgetItem_isHidden(QListWidgetItemH handle)
{
	return (bool) ((QListWidgetItem *)handle)->isHidden();
}

unsigned int QListWidgetItem_flags(QListWidgetItemH handle)
{
	return (unsigned int) ((QListWidgetItem *)handle)->flags();
}

void QListWidgetItem_setFlags(QListWidgetItemH handle, unsigned int flags)
{
	((QListWidgetItem *)handle)->setFlags((Qt::ItemFlags)flags);
}

void QListWidgetItem_text(QListWidgetItemH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QListWidgetItem *)handle)->text();
	copyQStringToPWideString(t_retval, retval);
}

void QListWidgetItem_setText(QListWidgetItemH handle, PWideString text)
{
	QString t_text;
	copyPWideStringToQString(text, t_text);
	((QListWidgetItem *)handle)->setText(t_text);
}

void QListWidgetItem_icon(QListWidgetItemH handle, QIconH retval)
{
	*(QIcon *)retval = ((QListWidgetItem *)handle)->icon();
}

void QListWidgetItem_setIcon(QListWidgetItemH handle, const QIconH icon)
{
	((QListWidgetItem *)handle)->setIcon(*(const QIcon*)icon);
}

void QListWidgetItem_statusTip(QListWidgetItemH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QListWidgetItem *)handle)->statusTip();
	copyQStringToPWideString(t_retval, retval);
}

void QListWidgetItem_setStatusTip(QListWidgetItemH handle, PWideString statusTip)
{
	QString t_statusTip;
	copyPWideStringToQString(statusTip, t_statusTip);
	((QListWidgetItem *)handle)->setStatusTip(t_statusTip);
}

void QListWidgetItem_toolTip(QListWidgetItemH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QListWidgetItem *)handle)->toolTip();
	copyQStringToPWideString(t_retval, retval);
}

void QListWidgetItem_setToolTip(QListWidgetItemH handle, PWideString toolTip)
{
	QString t_toolTip;
	copyPWideStringToQString(toolTip, t_toolTip);
	((QListWidgetItem *)handle)->setToolTip(t_toolTip);
}

void QListWidgetItem_whatsThis(QListWidgetItemH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QListWidgetItem *)handle)->whatsThis();
	copyQStringToPWideString(t_retval, retval);
}

void QListWidgetItem_setWhatsThis(QListWidgetItemH handle, PWideString whatsThis)
{
	QString t_whatsThis;
	copyPWideStringToQString(whatsThis, t_whatsThis);
	((QListWidgetItem *)handle)->setWhatsThis(t_whatsThis);
}

void QListWidgetItem_font(QListWidgetItemH handle, QFontH retval)
{
	*(QFont *)retval = ((QListWidgetItem *)handle)->font();
}

void QListWidgetItem_setFont(QListWidgetItemH handle, const QFontH font)
{
	((QListWidgetItem *)handle)->setFont(*(const QFont*)font);
}

int QListWidgetItem_textAlignment(QListWidgetItemH handle)
{
	return (int) ((QListWidgetItem *)handle)->textAlignment();
}

void QListWidgetItem_setTextAlignment(QListWidgetItemH handle, int alignment)
{
	((QListWidgetItem *)handle)->setTextAlignment(alignment);
}

void QListWidgetItem_background(QListWidgetItemH handle, QBrushH retval)
{
	*(QBrush *)retval = ((QListWidgetItem *)handle)->background();
}

void QListWidgetItem_setBackground(QListWidgetItemH handle, const QBrushH brush)
{
	((QListWidgetItem *)handle)->setBackground(*(const QBrush*)brush);
}

void QListWidgetItem_foreground(QListWidgetItemH handle, QBrushH retval)
{
	*(QBrush *)retval = ((QListWidgetItem *)handle)->foreground();
}

void QListWidgetItem_setForeground(QListWidgetItemH handle, const QBrushH brush)
{
	((QListWidgetItem *)handle)->setForeground(*(const QBrush*)brush);
}

Qt::CheckState QListWidgetItem_checkState(QListWidgetItemH handle)
{
	return (Qt::CheckState) ((QListWidgetItem *)handle)->checkState();
}

void QListWidgetItem_setCheckState(QListWidgetItemH handle, Qt::CheckState state)
{
	((QListWidgetItem *)handle)->setCheckState(state);
}

void QListWidgetItem_sizeHint(QListWidgetItemH handle, PSize retval)
{
	*(QSize *)retval = ((QListWidgetItem *)handle)->sizeHint();
}

void QListWidgetItem_setSizeHint(QListWidgetItemH handle, const QSizeH size)
{
	((QListWidgetItem *)handle)->setSizeHint(*(const QSize*)size);
}

void QListWidgetItem_data(QListWidgetItemH handle, QVariantH retval, int role)
{
	*(QVariant *)retval = ((QListWidgetItem *)handle)->data(role);
}

void QListWidgetItem_setData(QListWidgetItemH handle, int role, const QVariantH value)
{
	((QListWidgetItem *)handle)->setData(role, *(const QVariant*)value);
}

void QListWidgetItem_read(QListWidgetItemH handle, QDataStreamH in)
{
	((QListWidgetItem *)handle)->read(*(QDataStream*)in);
}

void QListWidgetItem_write(QListWidgetItemH handle, QDataStreamH out)
{
	((QListWidgetItem *)handle)->write(*(QDataStream*)out);
}

int QListWidgetItem_type(QListWidgetItemH handle)
{
	return (int) ((QListWidgetItem *)handle)->type();
}

QListWidgetH QListWidget_Create(QWidgetH parent)
{
	return (QListWidgetH) new QListWidget((QWidget*)parent);
}

void QListWidget_Destroy(QListWidgetH handle)
{
	delete (QListWidget *)handle;
}

QListWidgetItemH QListWidget_item(QListWidgetH handle, int row)
{
	return (QListWidgetItemH) ((QListWidget *)handle)->item(row);
}

int QListWidget_row(QListWidgetH handle, const QListWidgetItemH item)
{
	return (int) ((QListWidget *)handle)->row((const QListWidgetItem*)item);
}

void QListWidget_insertItem(QListWidgetH handle, int row, QListWidgetItemH item)
{
	((QListWidget *)handle)->insertItem(row, (QListWidgetItem*)item);
}

void QListWidget_insertItem2(QListWidgetH handle, int row, PWideString label)
{
	QString t_label;
	copyPWideStringToQString(label, t_label);
	((QListWidget *)handle)->insertItem(row, t_label);
}

void QListWidget_insertItems(QListWidgetH handle, int row, const QStringListH labels)
{
	((QListWidget *)handle)->insertItems(row, *(const QStringList*)labels);
}

void QListWidget_addItem(QListWidgetH handle, PWideString label)
{
	QString t_label;
	copyPWideStringToQString(label, t_label);
	((QListWidget *)handle)->addItem(t_label);
}

void QListWidget_addItem2(QListWidgetH handle, QListWidgetItemH item)
{
	((QListWidget *)handle)->addItem((QListWidgetItem*)item);
}

void QListWidget_addItems(QListWidgetH handle, const QStringListH labels)
{
	((QListWidget *)handle)->addItems(*(const QStringList*)labels);
}

QListWidgetItemH QListWidget_takeItem(QListWidgetH handle, int row)
{
	return (QListWidgetItemH) ((QListWidget *)handle)->takeItem(row);
}

int QListWidget_count(QListWidgetH handle)
{
	return (int) ((QListWidget *)handle)->count();
}

QListWidgetItemH QListWidget_currentItem(QListWidgetH handle)
{
	return (QListWidgetItemH) ((QListWidget *)handle)->currentItem();
}

void QListWidget_setCurrentItem(QListWidgetH handle, QListWidgetItemH item)
{
	((QListWidget *)handle)->setCurrentItem((QListWidgetItem*)item);
}

void QListWidget_setCurrentItem2(QListWidgetH handle, QListWidgetItemH item, unsigned int command)
{
	((QListWidget *)handle)->setCurrentItem((QListWidgetItem*)item, (QItemSelectionModel::SelectionFlags)command);
}

int QListWidget_currentRow(QListWidgetH handle)
{
	return (int) ((QListWidget *)handle)->currentRow();
}

void QListWidget_setCurrentRow(QListWidgetH handle, int row)
{
	((QListWidget *)handle)->setCurrentRow(row);
}

void QListWidget_setCurrentRow2(QListWidgetH handle, int row, unsigned int command)
{
	((QListWidget *)handle)->setCurrentRow(row, (QItemSelectionModel::SelectionFlags)command);
}

QListWidgetItemH QListWidget_itemAt(QListWidgetH handle, const QPointH p)
{
	return (QListWidgetItemH) ((QListWidget *)handle)->itemAt(*(const QPoint*)p);
}

QListWidgetItemH QListWidget_itemAt2(QListWidgetH handle, int x, int y)
{
	return (QListWidgetItemH) ((QListWidget *)handle)->itemAt(x, y);
}

void QListWidget_visualItemRect(QListWidgetH handle, PRect retval, const QListWidgetItemH item)
{
	QRect t_retval;
	t_retval = ((QListWidget *)handle)->visualItemRect((const QListWidgetItem*)item);
	copyQRectToPRect(t_retval, retval);
}

void QListWidget_sortItems(QListWidgetH handle, Qt::SortOrder order)
{
	((QListWidget *)handle)->sortItems(order);
}

void QListWidget_setSortingEnabled(QListWidgetH handle, bool enable)
{
	((QListWidget *)handle)->setSortingEnabled(enable);
}

bool QListWidget_isSortingEnabled(QListWidgetH handle)
{
	return (bool) ((QListWidget *)handle)->isSortingEnabled();
}

void QListWidget_editItem(QListWidgetH handle, QListWidgetItemH item)
{
	((QListWidget *)handle)->editItem((QListWidgetItem*)item);
}

void QListWidget_openPersistentEditor(QListWidgetH handle, QListWidgetItemH item)
{
	((QListWidget *)handle)->openPersistentEditor((QListWidgetItem*)item);
}

void QListWidget_closePersistentEditor(QListWidgetH handle, QListWidgetItemH item)
{
	((QListWidget *)handle)->closePersistentEditor((QListWidgetItem*)item);
}

QWidgetH QListWidget_itemWidget(QListWidgetH handle, QListWidgetItemH item)
{
	return (QWidgetH) ((QListWidget *)handle)->itemWidget((QListWidgetItem*)item);
}

void QListWidget_setItemWidget(QListWidgetH handle, QListWidgetItemH item, QWidgetH widget)
{
	((QListWidget *)handle)->setItemWidget((QListWidgetItem*)item, (QWidget*)widget);
}

void QListWidget_removeItemWidget(QListWidgetH handle, QListWidgetItemH item)
{
	((QListWidget *)handle)->removeItemWidget((QListWidgetItem*)item);
}

void QListWidget_selectedItems(QListWidgetH handle, PPtrIntArray retval)
{
	QList<QListWidgetItem*> t_retval;
	t_retval = ((QListWidget *)handle)->selectedItems();
	copyQListTemplateToPtrIntArray(t_retval, retval);
}

void QListWidget_findItems(QListWidgetH handle, PPtrIntArray retval, PWideString text, unsigned int flags)
{
	QList<QListWidgetItem*> t_retval;
	QString t_text;
	copyPWideStringToQString(text, t_text);
	t_retval = ((QListWidget *)handle)->findItems(t_text, (Qt::MatchFlags)flags);
	copyQListTemplateToPtrIntArray(t_retval, retval);
}

void QListWidget_scrollToItem(QListWidgetH handle, const QListWidgetItemH item, QAbstractItemView::ScrollHint hint)
{
	((QListWidget *)handle)->scrollToItem((const QListWidgetItem*)item, hint);
}

void QListWidget_clear(QListWidgetH handle)
{
	((QListWidget *)handle)->clear();
}

void QListWidget_indexFromItem(QListWidgetH handle, const QListWidgetItemH item, QModelIndexH retval)
{
  *(QModelIndex *)retval = ((QListWidget *)handle)->indexFromItem((const QListWidgetItem*)item);
}

bool QListWidget_isPersistentEditorOpen(QListWidgetH handle, QListWidgetItemH item)
{
  return (bool) ((QListWidget *)handle)->isPersistentEditorOpen((QListWidgetItem *)item);
}
