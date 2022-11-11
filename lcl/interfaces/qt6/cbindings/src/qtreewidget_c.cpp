//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qtreewidget_c.h"

QTreeWidgetItemH QTreeWidgetItem_Create(int type)
{
	return (QTreeWidgetItemH) new QTreeWidgetItem(type);
}

void QTreeWidgetItem_Destroy(QTreeWidgetItemH handle)
{
	delete (QTreeWidgetItem *)handle;
}

QTreeWidgetItemH QTreeWidgetItem_Create2(const QStringListH strings, int type)
{
	return (QTreeWidgetItemH) new QTreeWidgetItem(*(const QStringList*)strings, type);
}

QTreeWidgetItemH QTreeWidgetItem_Create3(QTreeWidgetH view, int type)
{
	return (QTreeWidgetItemH) new QTreeWidgetItem((QTreeWidget*)view, type);
}

QTreeWidgetItemH QTreeWidgetItem_Create4(QTreeWidgetH view, const QStringListH strings, int type)
{
	return (QTreeWidgetItemH) new QTreeWidgetItem((QTreeWidget*)view, *(const QStringList*)strings, type);
}

QTreeWidgetItemH QTreeWidgetItem_Create5(QTreeWidgetH view, QTreeWidgetItemH after, int type)
{
	return (QTreeWidgetItemH) new QTreeWidgetItem((QTreeWidget*)view, (QTreeWidgetItem*)after, type);
}

QTreeWidgetItemH QTreeWidgetItem_Create6(QTreeWidgetItemH parent, int type)
{
	return (QTreeWidgetItemH) new QTreeWidgetItem((QTreeWidgetItem*)parent, type);
}

QTreeWidgetItemH QTreeWidgetItem_Create7(QTreeWidgetItemH parent, const QStringListH strings, int type)
{
	return (QTreeWidgetItemH) new QTreeWidgetItem((QTreeWidgetItem*)parent, *(const QStringList*)strings, type);
}

QTreeWidgetItemH QTreeWidgetItem_Create8(QTreeWidgetItemH parent, QTreeWidgetItemH after, int type)
{
	return (QTreeWidgetItemH) new QTreeWidgetItem((QTreeWidgetItem*)parent, (QTreeWidgetItem*)after, type);
}

QTreeWidgetItemH QTreeWidgetItem_Create9(const QTreeWidgetItemH other)
{
	return (QTreeWidgetItemH) new QTreeWidgetItem(*(const QTreeWidgetItem*)other);
}

QTreeWidgetItemH QTreeWidgetItem_clone(QTreeWidgetItemH handle)
{
	return (QTreeWidgetItemH) ((QTreeWidgetItem *)handle)->clone();
}

QTreeWidgetH QTreeWidgetItem_treeWidget(QTreeWidgetItemH handle)
{
	return (QTreeWidgetH) ((QTreeWidgetItem *)handle)->treeWidget();
}

void QTreeWidgetItem_setSelected(QTreeWidgetItemH handle, bool select)
{
	((QTreeWidgetItem *)handle)->setSelected(select);
}

bool QTreeWidgetItem_isSelected(QTreeWidgetItemH handle)
{
	return (bool) ((QTreeWidgetItem *)handle)->isSelected();
}

void QTreeWidgetItem_setHidden(QTreeWidgetItemH handle, bool hide)
{
	((QTreeWidgetItem *)handle)->setHidden(hide);
}

bool QTreeWidgetItem_isHidden(QTreeWidgetItemH handle)
{
	return (bool) ((QTreeWidgetItem *)handle)->isHidden();
}

void QTreeWidgetItem_setExpanded(QTreeWidgetItemH handle, bool expand)
{
	((QTreeWidgetItem *)handle)->setExpanded(expand);
}

bool QTreeWidgetItem_isExpanded(QTreeWidgetItemH handle)
{
	return (bool) ((QTreeWidgetItem *)handle)->isExpanded();
}

void QTreeWidgetItem_setFirstColumnSpanned(QTreeWidgetItemH handle, bool span)
{
	((QTreeWidgetItem *)handle)->setFirstColumnSpanned(span);
}

bool QTreeWidgetItem_isFirstColumnSpanned(QTreeWidgetItemH handle)
{
	return (bool) ((QTreeWidgetItem *)handle)->isFirstColumnSpanned();
}

void QTreeWidgetItem_setDisabled(QTreeWidgetItemH handle, bool disabled)
{
	((QTreeWidgetItem *)handle)->setDisabled(disabled);
}

bool QTreeWidgetItem_isDisabled(QTreeWidgetItemH handle)
{
	return (bool) ((QTreeWidgetItem *)handle)->isDisabled();
}

void QTreeWidgetItem_setChildIndicatorPolicy(QTreeWidgetItemH handle, QTreeWidgetItem::ChildIndicatorPolicy policy)
{
	((QTreeWidgetItem *)handle)->setChildIndicatorPolicy(policy);
}

QTreeWidgetItem::ChildIndicatorPolicy QTreeWidgetItem_childIndicatorPolicy(QTreeWidgetItemH handle)
{
	return (QTreeWidgetItem::ChildIndicatorPolicy) ((QTreeWidgetItem *)handle)->childIndicatorPolicy();
}

unsigned int QTreeWidgetItem_flags(QTreeWidgetItemH handle)
{
	return (unsigned int) ((QTreeWidgetItem *)handle)->flags();
}

void QTreeWidgetItem_setFlags(QTreeWidgetItemH handle, unsigned int flags)
{
	((QTreeWidgetItem *)handle)->setFlags((Qt::ItemFlags)flags);
}

void QTreeWidgetItem_text(QTreeWidgetItemH handle, PWideString retval, int column)
{
	QString t_retval;
	t_retval = ((QTreeWidgetItem *)handle)->text(column);
	copyQStringToPWideString(t_retval, retval);
}

void QTreeWidgetItem_setText(QTreeWidgetItemH handle, int column, PWideString text)
{
	QString t_text;
	copyPWideStringToQString(text, t_text);
	((QTreeWidgetItem *)handle)->setText(column, t_text);
}

void QTreeWidgetItem_icon(QTreeWidgetItemH handle, QIconH retval, int column)
{
	*(QIcon *)retval = ((QTreeWidgetItem *)handle)->icon(column);
}

void QTreeWidgetItem_setIcon(QTreeWidgetItemH handle, int column, const QIconH icon)
{
	((QTreeWidgetItem *)handle)->setIcon(column, *(const QIcon*)icon);
}

void QTreeWidgetItem_statusTip(QTreeWidgetItemH handle, PWideString retval, int column)
{
	QString t_retval;
	t_retval = ((QTreeWidgetItem *)handle)->statusTip(column);
	copyQStringToPWideString(t_retval, retval);
}

void QTreeWidgetItem_setStatusTip(QTreeWidgetItemH handle, int column, PWideString statusTip)
{
	QString t_statusTip;
	copyPWideStringToQString(statusTip, t_statusTip);
	((QTreeWidgetItem *)handle)->setStatusTip(column, t_statusTip);
}

void QTreeWidgetItem_toolTip(QTreeWidgetItemH handle, PWideString retval, int column)
{
	QString t_retval;
	t_retval = ((QTreeWidgetItem *)handle)->toolTip(column);
	copyQStringToPWideString(t_retval, retval);
}

void QTreeWidgetItem_setToolTip(QTreeWidgetItemH handle, int column, PWideString toolTip)
{
	QString t_toolTip;
	copyPWideStringToQString(toolTip, t_toolTip);
	((QTreeWidgetItem *)handle)->setToolTip(column, t_toolTip);
}

void QTreeWidgetItem_whatsThis(QTreeWidgetItemH handle, PWideString retval, int column)
{
	QString t_retval;
	t_retval = ((QTreeWidgetItem *)handle)->whatsThis(column);
	copyQStringToPWideString(t_retval, retval);
}

void QTreeWidgetItem_setWhatsThis(QTreeWidgetItemH handle, int column, PWideString whatsThis)
{
	QString t_whatsThis;
	copyPWideStringToQString(whatsThis, t_whatsThis);
	((QTreeWidgetItem *)handle)->setWhatsThis(column, t_whatsThis);
}

void QTreeWidgetItem_font(QTreeWidgetItemH handle, QFontH retval, int column)
{
	*(QFont *)retval = ((QTreeWidgetItem *)handle)->font(column);
}

void QTreeWidgetItem_setFont(QTreeWidgetItemH handle, int column, const QFontH font)
{
	((QTreeWidgetItem *)handle)->setFont(column, *(const QFont*)font);
}

int QTreeWidgetItem_textAlignment(QTreeWidgetItemH handle, int column)
{
	return (int) ((QTreeWidgetItem *)handle)->textAlignment(column);
}

void QTreeWidgetItem_setTextAlignment(QTreeWidgetItemH handle, int column, int alignment)
{
	((QTreeWidgetItem *)handle)->setTextAlignment(column, alignment);
}

void QTreeWidgetItem_background(QTreeWidgetItemH handle, QBrushH retval, int column)
{
	*(QBrush *)retval = ((QTreeWidgetItem *)handle)->background(column);
}

void QTreeWidgetItem_setBackground(QTreeWidgetItemH handle, int column, const QBrushH brush)
{
	((QTreeWidgetItem *)handle)->setBackground(column, *(const QBrush*)brush);
}

void QTreeWidgetItem_foreground(QTreeWidgetItemH handle, QBrushH retval, int column)
{
	*(QBrush *)retval = ((QTreeWidgetItem *)handle)->foreground(column);
}

void QTreeWidgetItem_setForeground(QTreeWidgetItemH handle, int column, const QBrushH brush)
{
	((QTreeWidgetItem *)handle)->setForeground(column, *(const QBrush*)brush);
}

Qt::CheckState QTreeWidgetItem_checkState(QTreeWidgetItemH handle, int column)
{
	return (Qt::CheckState) ((QTreeWidgetItem *)handle)->checkState(column);
}

void QTreeWidgetItem_setCheckState(QTreeWidgetItemH handle, int column, Qt::CheckState state)
{
	((QTreeWidgetItem *)handle)->setCheckState(column, state);
}

void QTreeWidgetItem_sizeHint(QTreeWidgetItemH handle, PSize retval, int column)
{
	*(QSize *)retval = ((QTreeWidgetItem *)handle)->sizeHint(column);
}

void QTreeWidgetItem_setSizeHint(QTreeWidgetItemH handle, int column, const QSizeH size)
{
	((QTreeWidgetItem *)handle)->setSizeHint(column, *(const QSize*)size);
}

void QTreeWidgetItem_data(QTreeWidgetItemH handle, QVariantH retval, int column, int role)
{
	*(QVariant *)retval = ((QTreeWidgetItem *)handle)->data(column, role);
}

void QTreeWidgetItem_setData(QTreeWidgetItemH handle, int column, int role, const QVariantH value)
{
	((QTreeWidgetItem *)handle)->setData(column, role, *(const QVariant*)value);
}

void QTreeWidgetItem_read(QTreeWidgetItemH handle, QDataStreamH in)
{
	((QTreeWidgetItem *)handle)->read(*(QDataStream*)in);
}

void QTreeWidgetItem_write(QTreeWidgetItemH handle, QDataStreamH out)
{
	((QTreeWidgetItem *)handle)->write(*(QDataStream*)out);
}

QTreeWidgetItemH QTreeWidgetItem_parent(QTreeWidgetItemH handle)
{
	return (QTreeWidgetItemH) ((QTreeWidgetItem *)handle)->parent();
}

QTreeWidgetItemH QTreeWidgetItem_child(QTreeWidgetItemH handle, int index)
{
	return (QTreeWidgetItemH) ((QTreeWidgetItem *)handle)->child(index);
}

int QTreeWidgetItem_childCount(QTreeWidgetItemH handle)
{
	return (int) ((QTreeWidgetItem *)handle)->childCount();
}

int QTreeWidgetItem_columnCount(QTreeWidgetItemH handle)
{
	return (int) ((QTreeWidgetItem *)handle)->columnCount();
}

int QTreeWidgetItem_indexOfChild(QTreeWidgetItemH handle, QTreeWidgetItemH child)
{
	return (int) ((QTreeWidgetItem *)handle)->indexOfChild((QTreeWidgetItem*)child);
}

void QTreeWidgetItem_addChild(QTreeWidgetItemH handle, QTreeWidgetItemH child)
{
	((QTreeWidgetItem *)handle)->addChild((QTreeWidgetItem*)child);
}

void QTreeWidgetItem_insertChild(QTreeWidgetItemH handle, int index, QTreeWidgetItemH child)
{
	((QTreeWidgetItem *)handle)->insertChild(index, (QTreeWidgetItem*)child);
}

void QTreeWidgetItem_removeChild(QTreeWidgetItemH handle, QTreeWidgetItemH child)
{
	((QTreeWidgetItem *)handle)->removeChild((QTreeWidgetItem*)child);
}

QTreeWidgetItemH QTreeWidgetItem_takeChild(QTreeWidgetItemH handle, int index)
{
	return (QTreeWidgetItemH) ((QTreeWidgetItem *)handle)->takeChild(index);
}

void QTreeWidgetItem_addChildren(QTreeWidgetItemH handle, PPtrIntArray children)
{
	QList<QTreeWidgetItem*> t_children;
	copyPtrIntArrayToQListTemplate(children, t_children);
	((QTreeWidgetItem *)handle)->addChildren(t_children);
}

void QTreeWidgetItem_insertChildren(QTreeWidgetItemH handle, int index, PPtrIntArray children)
{
	QList<QTreeWidgetItem*> t_children;
	copyPtrIntArrayToQListTemplate(children, t_children);
	((QTreeWidgetItem *)handle)->insertChildren(index, t_children);
}

void QTreeWidgetItem_takeChildren(QTreeWidgetItemH handle, PPtrIntArray retval)
{
	QList<QTreeWidgetItem*> t_retval;
	t_retval = ((QTreeWidgetItem *)handle)->takeChildren();
	copyQListTemplateToPtrIntArray(t_retval, retval);
}

int QTreeWidgetItem_type(QTreeWidgetItemH handle)
{
	return (int) ((QTreeWidgetItem *)handle)->type();
}

void QTreeWidgetItem_sortChildren(QTreeWidgetItemH handle, int column, Qt::SortOrder order)
{
	((QTreeWidgetItem *)handle)->sortChildren(column, order);
}

QTreeWidgetH QTreeWidget_Create(QWidgetH parent)
{
	return (QTreeWidgetH) new QTreeWidget((QWidget*)parent);
}

void QTreeWidget_Destroy(QTreeWidgetH handle)
{
	delete (QTreeWidget *)handle;
}

int QTreeWidget_columnCount(QTreeWidgetH handle)
{
	return (int) ((QTreeWidget *)handle)->columnCount();
}

void QTreeWidget_setColumnCount(QTreeWidgetH handle, int columns)
{
	((QTreeWidget *)handle)->setColumnCount(columns);
}

QTreeWidgetItemH QTreeWidget_invisibleRootItem(QTreeWidgetH handle)
{
	return (QTreeWidgetItemH) ((QTreeWidget *)handle)->invisibleRootItem();
}

QTreeWidgetItemH QTreeWidget_topLevelItem(QTreeWidgetH handle, int index)
{
	return (QTreeWidgetItemH) ((QTreeWidget *)handle)->topLevelItem(index);
}

int QTreeWidget_topLevelItemCount(QTreeWidgetH handle)
{
	return (int) ((QTreeWidget *)handle)->topLevelItemCount();
}

void QTreeWidget_insertTopLevelItem(QTreeWidgetH handle, int index, QTreeWidgetItemH item)
{
	((QTreeWidget *)handle)->insertTopLevelItem(index, (QTreeWidgetItem*)item);
}

void QTreeWidget_addTopLevelItem(QTreeWidgetH handle, QTreeWidgetItemH item)
{
	((QTreeWidget *)handle)->addTopLevelItem((QTreeWidgetItem*)item);
}

QTreeWidgetItemH QTreeWidget_takeTopLevelItem(QTreeWidgetH handle, int index)
{
	return (QTreeWidgetItemH) ((QTreeWidget *)handle)->takeTopLevelItem(index);
}

int QTreeWidget_indexOfTopLevelItem(QTreeWidgetH handle, QTreeWidgetItemH item)
{
	return (int) ((QTreeWidget *)handle)->indexOfTopLevelItem((QTreeWidgetItem*)item);
}

void QTreeWidget_insertTopLevelItems(QTreeWidgetH handle, int index, PPtrIntArray items)
{
	QList<QTreeWidgetItem*> t_items;
	copyPtrIntArrayToQListTemplate(items, t_items);
	((QTreeWidget *)handle)->insertTopLevelItems(index, t_items);
}

void QTreeWidget_addTopLevelItems(QTreeWidgetH handle, PPtrIntArray items)
{
	QList<QTreeWidgetItem*> t_items;
	copyPtrIntArrayToQListTemplate(items, t_items);
	((QTreeWidget *)handle)->addTopLevelItems(t_items);
}

QTreeWidgetItemH QTreeWidget_headerItem(QTreeWidgetH handle)
{
	return (QTreeWidgetItemH) ((QTreeWidget *)handle)->headerItem();
}

void QTreeWidget_setHeaderItem(QTreeWidgetH handle, QTreeWidgetItemH item)
{
	((QTreeWidget *)handle)->setHeaderItem((QTreeWidgetItem*)item);
}

void QTreeWidget_setHeaderLabels(QTreeWidgetH handle, const QStringListH labels)
{
	((QTreeWidget *)handle)->setHeaderLabels(*(const QStringList*)labels);
}

void QTreeWidget_setHeaderLabel(QTreeWidgetH handle, PWideString label)
{
	QString t_label;
	copyPWideStringToQString(label, t_label);
	((QTreeWidget *)handle)->setHeaderLabel(t_label);
}

QTreeWidgetItemH QTreeWidget_currentItem(QTreeWidgetH handle)
{
	return (QTreeWidgetItemH) ((QTreeWidget *)handle)->currentItem();
}

int QTreeWidget_currentColumn(QTreeWidgetH handle)
{
	return (int) ((QTreeWidget *)handle)->currentColumn();
}

void QTreeWidget_setCurrentItem(QTreeWidgetH handle, QTreeWidgetItemH item)
{
	((QTreeWidget *)handle)->setCurrentItem((QTreeWidgetItem*)item);
}

void QTreeWidget_setCurrentItem2(QTreeWidgetH handle, QTreeWidgetItemH item, int column)
{
	((QTreeWidget *)handle)->setCurrentItem((QTreeWidgetItem*)item, column);
}

void QTreeWidget_setCurrentItem3(QTreeWidgetH handle, QTreeWidgetItemH item, int column, unsigned int command)
{
	((QTreeWidget *)handle)->setCurrentItem((QTreeWidgetItem*)item, column, (QItemSelectionModel::SelectionFlags)command);
}

QTreeWidgetItemH QTreeWidget_itemAt(QTreeWidgetH handle, const QPointH p)
{
	return (QTreeWidgetItemH) ((QTreeWidget *)handle)->itemAt(*(const QPoint*)p);
}

QTreeWidgetItemH QTreeWidget_itemAt2(QTreeWidgetH handle, int x, int y)
{
	return (QTreeWidgetItemH) ((QTreeWidget *)handle)->itemAt(x, y);
}

void QTreeWidget_visualItemRect(QTreeWidgetH handle, PRect retval, const QTreeWidgetItemH item)
{
	QRect t_retval;
	t_retval = ((QTreeWidget *)handle)->visualItemRect((const QTreeWidgetItem*)item);
	copyQRectToPRect(t_retval, retval);
}

int QTreeWidget_sortColumn(QTreeWidgetH handle)
{
	return (int) ((QTreeWidget *)handle)->sortColumn();
}

void QTreeWidget_sortItems(QTreeWidgetH handle, int column, Qt::SortOrder order)
{
	((QTreeWidget *)handle)->sortItems(column, order);
}

void QTreeWidget_editItem(QTreeWidgetH handle, QTreeWidgetItemH item, int column)
{
	((QTreeWidget *)handle)->editItem((QTreeWidgetItem*)item, column);
}

void QTreeWidget_openPersistentEditor(QTreeWidgetH handle, QTreeWidgetItemH item, int column)
{
	((QTreeWidget *)handle)->openPersistentEditor((QTreeWidgetItem*)item, column);
}

void QTreeWidget_closePersistentEditor(QTreeWidgetH handle, QTreeWidgetItemH item, int column)
{
	((QTreeWidget *)handle)->closePersistentEditor((QTreeWidgetItem*)item, column);
}

QWidgetH QTreeWidget_itemWidget(QTreeWidgetH handle, QTreeWidgetItemH item, int column)
{
	return (QWidgetH) ((QTreeWidget *)handle)->itemWidget((QTreeWidgetItem*)item, column);
}

void QTreeWidget_setItemWidget(QTreeWidgetH handle, QTreeWidgetItemH item, int column, QWidgetH widget)
{
	((QTreeWidget *)handle)->setItemWidget((QTreeWidgetItem*)item, column, (QWidget*)widget);
}

void QTreeWidget_removeItemWidget(QTreeWidgetH handle, QTreeWidgetItemH item, int column)
{
	((QTreeWidget *)handle)->removeItemWidget((QTreeWidgetItem*)item, column);
}

void QTreeWidget_selectedItems(QTreeWidgetH handle, PPtrIntArray retval)
{
	QList<QTreeWidgetItem*> t_retval;
	t_retval = ((QTreeWidget *)handle)->selectedItems();
	copyQListTemplateToPtrIntArray(t_retval, retval);
}

void QTreeWidget_findItems(QTreeWidgetH handle, PPtrIntArray retval, PWideString text, unsigned int flags, int column)
{
	QList<QTreeWidgetItem*> t_retval;
	QString t_text;
	copyPWideStringToQString(text, t_text);
	t_retval = ((QTreeWidget *)handle)->findItems(t_text, (Qt::MatchFlags)flags, column);
	copyQListTemplateToPtrIntArray(t_retval, retval);
}

QTreeWidgetItemH QTreeWidget_itemAbove(QTreeWidgetH handle, const QTreeWidgetItemH item)
{
	return (QTreeWidgetItemH) ((QTreeWidget *)handle)->itemAbove((const QTreeWidgetItem*)item);
}

QTreeWidgetItemH QTreeWidget_itemBelow(QTreeWidgetH handle, const QTreeWidgetItemH item)
{
	return (QTreeWidgetItemH) ((QTreeWidget *)handle)->itemBelow((const QTreeWidgetItem*)item);
}

void QTreeWidget_setSelectionModel(QTreeWidgetH handle, QItemSelectionModelH selectionModel)
{
	((QTreeWidget *)handle)->setSelectionModel((QItemSelectionModel*)selectionModel);
}

void QTreeWidget_scrollToItem(QTreeWidgetH handle, const QTreeWidgetItemH item, QAbstractItemView::ScrollHint hint)
{
	((QTreeWidget *)handle)->scrollToItem((const QTreeWidgetItem*)item, hint);
}

void QTreeWidget_expandItem(QTreeWidgetH handle, const QTreeWidgetItemH item)
{
	((QTreeWidget *)handle)->expandItem((const QTreeWidgetItem*)item);
}

void QTreeWidget_collapseItem(QTreeWidgetH handle, const QTreeWidgetItemH item)
{
	((QTreeWidget *)handle)->collapseItem((const QTreeWidgetItem*)item);
}

void QTreeWidget_clear(QTreeWidgetH handle)
{
	((QTreeWidget *)handle)->clear();
}

bool QTreeWidget_isPersistentEditorOpen(QTreeWidgetH handle, QTreeWidgetItemH item, int column)
{
  return (bool) ((QTreeWidget *)handle)->isPersistentEditorOpen((QTreeWidgetItem *) item, column);
}
