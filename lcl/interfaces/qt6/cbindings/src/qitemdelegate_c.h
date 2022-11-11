//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QITEMDELEGATE_C_H
#define QITEMDELEGATE_C_H

#include <QtWidgets>
#include "pascalbind.h"

C_EXPORT QItemDelegateH QItemDelegate_Create(QObjectH parent);
C_EXPORT void QItemDelegate_Destroy(QItemDelegateH handle);
C_EXPORT bool QItemDelegate_hasClipping(QItemDelegateH handle);
C_EXPORT void QItemDelegate_setClipping(QItemDelegateH handle, bool clip);
C_EXPORT void QItemDelegate_paint(QItemDelegateH handle, QPainterH painter, const QStyleOptionViewItemH option, const QModelIndexH index);
C_EXPORT void QItemDelegate_sizeHint(QItemDelegateH handle, PSize retval, const QStyleOptionViewItemH option, const QModelIndexH index);
C_EXPORT QWidgetH QItemDelegate_createEditor(QItemDelegateH handle, QWidgetH parent, const QStyleOptionViewItemH option, const QModelIndexH index);
C_EXPORT void QItemDelegate_setEditorData(QItemDelegateH handle, QWidgetH editor, const QModelIndexH index);
C_EXPORT void QItemDelegate_setModelData(QItemDelegateH handle, QWidgetH editor, QAbstractItemModelH model, const QModelIndexH index);
C_EXPORT void QItemDelegate_updateEditorGeometry(QItemDelegateH handle, QWidgetH editor, const QStyleOptionViewItemH option, const QModelIndexH index);
C_EXPORT QItemEditorFactoryH QItemDelegate_itemEditorFactory(QItemDelegateH handle);
C_EXPORT void QItemDelegate_setItemEditorFactory(QItemDelegateH handle, QItemEditorFactoryH factory);

#endif
