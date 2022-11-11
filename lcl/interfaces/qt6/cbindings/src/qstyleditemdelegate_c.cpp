//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qstyleditemdelegate_c.h"

QStyledItemDelegateH QStyledItemDelegate_Create(QObjectH parent)
{
	return (QStyledItemDelegateH) new QStyledItemDelegate((QObject*)parent);
}

void QStyledItemDelegate_Destroy(QStyledItemDelegateH handle)
{
	delete (QStyledItemDelegate *)handle;
}

void QStyledItemDelegate_paint(QStyledItemDelegateH handle, QPainterH painter, const QStyleOptionViewItemH option, const QModelIndexH index)
{
	((QStyledItemDelegate *)handle)->paint((QPainter*)painter, *(const QStyleOptionViewItem*)option, *(const QModelIndex*)index);
}

void QStyledItemDelegate_sizeHint(QStyledItemDelegateH handle, PSize retval, const QStyleOptionViewItemH option, const QModelIndexH index)
{
	*(QSize *)retval = ((QStyledItemDelegate *)handle)->sizeHint(*(const QStyleOptionViewItem*)option, *(const QModelIndex*)index);
}

QWidgetH QStyledItemDelegate_createEditor(QStyledItemDelegateH handle, QWidgetH parent, const QStyleOptionViewItemH option, const QModelIndexH index)
{
	return (QWidgetH) ((QStyledItemDelegate *)handle)->createEditor((QWidget*)parent, *(const QStyleOptionViewItem*)option, *(const QModelIndex*)index);
}

void QStyledItemDelegate_setEditorData(QStyledItemDelegateH handle, QWidgetH editor, const QModelIndexH index)
{
	((QStyledItemDelegate *)handle)->setEditorData((QWidget*)editor, *(const QModelIndex*)index);
}

void QStyledItemDelegate_setModelData(QStyledItemDelegateH handle, QWidgetH editor, QAbstractItemModelH model, const QModelIndexH index)
{
	((QStyledItemDelegate *)handle)->setModelData((QWidget*)editor, (QAbstractItemModel*)model, *(const QModelIndex*)index);
}

void QStyledItemDelegate_updateEditorGeometry(QStyledItemDelegateH handle, QWidgetH editor, const QStyleOptionViewItemH option, const QModelIndexH index)
{
	((QStyledItemDelegate *)handle)->updateEditorGeometry((QWidget*)editor, *(const QStyleOptionViewItem*)option, *(const QModelIndex*)index);
}

QItemEditorFactoryH QStyledItemDelegate_itemEditorFactory(QStyledItemDelegateH handle)
{
	return (QItemEditorFactoryH) ((QStyledItemDelegate *)handle)->itemEditorFactory();
}

void QStyledItemDelegate_setItemEditorFactory(QStyledItemDelegateH handle, QItemEditorFactoryH factory)
{
	((QStyledItemDelegate *)handle)->setItemEditorFactory((QItemEditorFactory*)factory);
}

void QStyledItemDelegate_displayText(QStyledItemDelegateH handle, PWideString retval, const QVariantH value, const QLocaleH locale)
{
	QString t_retval;
	t_retval = ((QStyledItemDelegate *)handle)->displayText(*(const QVariant*)value, *(const QLocale*)locale);
	copyQStringToPWideString(t_retval, retval);
}

