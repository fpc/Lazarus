//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QSTYLEDITEMDELEGATE_C_H
#define QSTYLEDITEMDELEGATE_C_H

#include <QtWidgets>
#include "pascalbind.h"

C_EXPORT QStyledItemDelegateH QStyledItemDelegate_Create(QObjectH parent);
C_EXPORT void QStyledItemDelegate_Destroy(QStyledItemDelegateH handle);
C_EXPORT void QStyledItemDelegate_paint(QStyledItemDelegateH handle, QPainterH painter, const QStyleOptionViewItemH option, const QModelIndexH index);
C_EXPORT void QStyledItemDelegate_sizeHint(QStyledItemDelegateH handle, PSize retval, const QStyleOptionViewItemH option, const QModelIndexH index);
C_EXPORT QWidgetH QStyledItemDelegate_createEditor(QStyledItemDelegateH handle, QWidgetH parent, const QStyleOptionViewItemH option, const QModelIndexH index);
C_EXPORT void QStyledItemDelegate_setEditorData(QStyledItemDelegateH handle, QWidgetH editor, const QModelIndexH index);
C_EXPORT void QStyledItemDelegate_setModelData(QStyledItemDelegateH handle, QWidgetH editor, QAbstractItemModelH model, const QModelIndexH index);
C_EXPORT void QStyledItemDelegate_updateEditorGeometry(QStyledItemDelegateH handle, QWidgetH editor, const QStyleOptionViewItemH option, const QModelIndexH index);
C_EXPORT QItemEditorFactoryH QStyledItemDelegate_itemEditorFactory(QStyledItemDelegateH handle);
C_EXPORT void QStyledItemDelegate_setItemEditorFactory(QStyledItemDelegateH handle, QItemEditorFactoryH factory);
C_EXPORT void QStyledItemDelegate_displayText(QStyledItemDelegateH handle, PWideString retval, const QVariantH value, const QLocaleH locale);

#endif
