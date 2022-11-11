//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QABSTRACTITEMDELEGATE_C_H
#define QABSTRACTITEMDELEGATE_C_H

#include <QtWidgets>
#include "pascalbind.h"

C_EXPORT void QAbstractItemDelegate_paint(QAbstractItemDelegateH handle, QPainterH painter, const QStyleOptionViewItemH option, const QModelIndexH index);
C_EXPORT void QAbstractItemDelegate_sizeHint(QAbstractItemDelegateH handle, PSize retval, const QStyleOptionViewItemH option, const QModelIndexH index);
C_EXPORT QWidgetH QAbstractItemDelegate_createEditor(QAbstractItemDelegateH handle, QWidgetH parent, const QStyleOptionViewItemH option, const QModelIndexH index);
C_EXPORT void QAbstractItemDelegate_destroyEditor(QAbstractItemDelegateH handle, QWidgetH editor, const QModelIndexH index);
C_EXPORT void QAbstractItemDelegate_setEditorData(QAbstractItemDelegateH handle, QWidgetH editor, const QModelIndexH index);
C_EXPORT void QAbstractItemDelegate_setModelData(QAbstractItemDelegateH handle, QWidgetH editor, QAbstractItemModelH model, const QModelIndexH index);
C_EXPORT void QAbstractItemDelegate_updateEditorGeometry(QAbstractItemDelegateH handle, QWidgetH editor, const QStyleOptionViewItemH option, const QModelIndexH index);
C_EXPORT bool QAbstractItemDelegate_editorEvent(QAbstractItemDelegateH handle, QEventH event, QAbstractItemModelH model, const QStyleOptionViewItemH option, const QModelIndexH index);
C_EXPORT bool QAbstractItemDelegate_helpEvent(QAbstractItemDelegateH handle, QHelpEventH event, QAbstractItemViewH view, const QStyleOptionViewItemH option, const QModelIndexH index);

#endif
