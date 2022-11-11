//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QLAYOUT_C_H
#define QLAYOUT_C_H

#include <QtWidgets>
#include "pascalbind.h"

C_EXPORT int QLayout_spacing(QLayoutH handle);
C_EXPORT void QLayout_setSpacing(QLayoutH handle, int AnonParam1);
C_EXPORT void QLayout_setContentsMargins(QLayoutH handle, int left, int top, int right, int bottom);
C_EXPORT void QLayout_setContentsMargins2(QLayoutH handle, const QMarginsH margins);
C_EXPORT void QLayout_getContentsMargins(QLayoutH handle, int* left, int* top, int* right, int* bottom);
C_EXPORT void QLayout_contentsMargins(QLayoutH handle, QMarginsH retval);
C_EXPORT void QLayout_contentsRect(QLayoutH handle, PRect retval);
C_EXPORT bool QLayout_setAlignment(QLayoutH handle, QWidgetH w, unsigned int alignment);
C_EXPORT bool QLayout_setAlignment2(QLayoutH handle, QLayoutH l, unsigned int alignment);
C_EXPORT void QLayout_setSizeConstraint(QLayoutH handle, QLayout::SizeConstraint AnonParam1);
C_EXPORT QLayout::SizeConstraint QLayout_sizeConstraint(QLayoutH handle);
C_EXPORT void QLayout_setMenuBar(QLayoutH handle, QWidgetH w);
C_EXPORT QWidgetH QLayout_menuBar(QLayoutH handle);
C_EXPORT QWidgetH QLayout_parentWidget(QLayoutH handle);
C_EXPORT void QLayout_invalidate(QLayoutH handle);
C_EXPORT void QLayout_geometry(QLayoutH handle, PRect retval);
C_EXPORT bool QLayout_activate(QLayoutH handle);
C_EXPORT void QLayout_update(QLayoutH handle);
C_EXPORT void QLayout_addWidget(QLayoutH handle, QWidgetH w);
C_EXPORT void QLayout_addItem(QLayoutH handle, QLayoutItemH AnonParam1);
C_EXPORT void QLayout_removeWidget(QLayoutH handle, QWidgetH w);
C_EXPORT void QLayout_removeItem(QLayoutH handle, QLayoutItemH AnonParam1);
C_EXPORT unsigned int QLayout_expandingDirections(QLayoutH handle);
C_EXPORT void QLayout_minimumSize(QLayoutH handle, PSize retval);
C_EXPORT void QLayout_maximumSize(QLayoutH handle, PSize retval);
C_EXPORT void QLayout_setGeometry(QLayoutH handle, PRect AnonParam1);
C_EXPORT QLayoutItemH QLayout_itemAt(QLayoutH handle, int index);
C_EXPORT QLayoutItemH QLayout_takeAt(QLayoutH handle, int index);
C_EXPORT int QLayout_indexOf(QLayoutH handle, QWidgetH AnonParam1);
C_EXPORT int QLayout_count(QLayoutH handle);
C_EXPORT bool QLayout_isEmpty(QLayoutH handle);
C_EXPORT unsigned int QLayout_controlTypes(QLayoutH handle);
C_EXPORT int QLayout_totalHeightForWidth(QLayoutH handle, int w);
C_EXPORT void QLayout_totalMinimumSize(QLayoutH handle, PSize retval);
C_EXPORT void QLayout_totalMaximumSize(QLayoutH handle, PSize retval);
C_EXPORT void QLayout_totalSizeHint(QLayoutH handle, PSize retval);
C_EXPORT QLayoutH QLayout_layout(QLayoutH handle);
C_EXPORT void QLayout_setEnabled(QLayoutH handle, bool AnonParam1);
C_EXPORT bool QLayout_isEnabled(QLayoutH handle);
C_EXPORT void QLayout_closestAcceptableSize(PSize retval, const QWidgetH w, const QSizeH s);
C_EXPORT QLayoutItemH QLayout_to_QLayoutItem(QLayoutH handle);

#endif
