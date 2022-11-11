//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qabstractitemdelegate_c.h"

void QAbstractItemDelegate_paint(QAbstractItemDelegateH handle, QPainterH painter, const QStyleOptionViewItemH option, const QModelIndexH index)
{
	((QAbstractItemDelegate *)handle)->paint((QPainter*)painter, *(const QStyleOptionViewItem*)option, *(const QModelIndex*)index);
}

void QAbstractItemDelegate_sizeHint(QAbstractItemDelegateH handle, PSize retval, const QStyleOptionViewItemH option, const QModelIndexH index)
{
	*(QSize *)retval = ((QAbstractItemDelegate *)handle)->sizeHint(*(const QStyleOptionViewItem*)option, *(const QModelIndex*)index);
}

QWidgetH QAbstractItemDelegate_createEditor(QAbstractItemDelegateH handle, QWidgetH parent, const QStyleOptionViewItemH option, const QModelIndexH index)
{
	return (QWidgetH) ((QAbstractItemDelegate *)handle)->createEditor((QWidget*)parent, *(const QStyleOptionViewItem*)option, *(const QModelIndex*)index);
}

void QAbstractItemDelegate_destroyEditor(QAbstractItemDelegateH handle, QWidgetH editor, const QModelIndexH index)
{
	((QAbstractItemDelegate *)handle)->destroyEditor((QWidget*)editor, *(const QModelIndex*)index);
}

void QAbstractItemDelegate_setEditorData(QAbstractItemDelegateH handle, QWidgetH editor, const QModelIndexH index)
{
	((QAbstractItemDelegate *)handle)->setEditorData((QWidget*)editor, *(const QModelIndex*)index);
}

void QAbstractItemDelegate_setModelData(QAbstractItemDelegateH handle, QWidgetH editor, QAbstractItemModelH model, const QModelIndexH index)
{
	((QAbstractItemDelegate *)handle)->setModelData((QWidget*)editor, (QAbstractItemModel*)model, *(const QModelIndex*)index);
}

void QAbstractItemDelegate_updateEditorGeometry(QAbstractItemDelegateH handle, QWidgetH editor, const QStyleOptionViewItemH option, const QModelIndexH index)
{
	((QAbstractItemDelegate *)handle)->updateEditorGeometry((QWidget*)editor, *(const QStyleOptionViewItem*)option, *(const QModelIndex*)index);
}

bool QAbstractItemDelegate_editorEvent(QAbstractItemDelegateH handle, QEventH event, QAbstractItemModelH model, const QStyleOptionViewItemH option, const QModelIndexH index)
{
	return (bool) ((QAbstractItemDelegate *)handle)->editorEvent((QEvent*)event, (QAbstractItemModel*)model, *(const QStyleOptionViewItem*)option, *(const QModelIndex*)index);
}

bool QAbstractItemDelegate_helpEvent(QAbstractItemDelegateH handle, QHelpEventH event, QAbstractItemViewH view, const QStyleOptionViewItemH option, const QModelIndexH index)
{
	return (bool) ((QAbstractItemDelegate *)handle)->helpEvent((QHelpEvent*)event, (QAbstractItemView*)view, *(const QStyleOptionViewItem*)option, *(const QModelIndex*)index);
}

