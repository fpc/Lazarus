//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qitemdelegate_c.h"

QItemDelegateH QItemDelegate_Create(QObjectH parent)
{
	return (QItemDelegateH) new QItemDelegate((QObject*)parent);
}

void QItemDelegate_Destroy(QItemDelegateH handle)
{
	delete (QItemDelegate *)handle;
}

bool QItemDelegate_hasClipping(QItemDelegateH handle)
{
	return (bool) ((QItemDelegate *)handle)->hasClipping();
}

void QItemDelegate_setClipping(QItemDelegateH handle, bool clip)
{
	((QItemDelegate *)handle)->setClipping(clip);
}

void QItemDelegate_paint(QItemDelegateH handle, QPainterH painter, const QStyleOptionViewItemH option, const QModelIndexH index)
{
	((QItemDelegate *)handle)->paint((QPainter*)painter, *(const QStyleOptionViewItem*)option, *(const QModelIndex*)index);
}

void QItemDelegate_sizeHint(QItemDelegateH handle, PSize retval, const QStyleOptionViewItemH option, const QModelIndexH index)
{
	*(QSize *)retval = ((QItemDelegate *)handle)->sizeHint(*(const QStyleOptionViewItem*)option, *(const QModelIndex*)index);
}

QWidgetH QItemDelegate_createEditor(QItemDelegateH handle, QWidgetH parent, const QStyleOptionViewItemH option, const QModelIndexH index)
{
	return (QWidgetH) ((QItemDelegate *)handle)->createEditor((QWidget*)parent, *(const QStyleOptionViewItem*)option, *(const QModelIndex*)index);
}

void QItemDelegate_setEditorData(QItemDelegateH handle, QWidgetH editor, const QModelIndexH index)
{
	((QItemDelegate *)handle)->setEditorData((QWidget*)editor, *(const QModelIndex*)index);
}

void QItemDelegate_setModelData(QItemDelegateH handle, QWidgetH editor, QAbstractItemModelH model, const QModelIndexH index)
{
	((QItemDelegate *)handle)->setModelData((QWidget*)editor, (QAbstractItemModel*)model, *(const QModelIndex*)index);
}

void QItemDelegate_updateEditorGeometry(QItemDelegateH handle, QWidgetH editor, const QStyleOptionViewItemH option, const QModelIndexH index)
{
	((QItemDelegate *)handle)->updateEditorGeometry((QWidget*)editor, *(const QStyleOptionViewItem*)option, *(const QModelIndex*)index);
}

QItemEditorFactoryH QItemDelegate_itemEditorFactory(QItemDelegateH handle)
{
	return (QItemEditorFactoryH) ((QItemDelegate *)handle)->itemEditorFactory();
}

void QItemDelegate_setItemEditorFactory(QItemDelegateH handle, QItemEditorFactoryH factory)
{
	((QItemDelegate *)handle)->setItemEditorFactory((QItemEditorFactory*)factory);
}

