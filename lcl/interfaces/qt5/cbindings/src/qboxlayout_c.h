//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QBOXLAYOUT_C_H
#define QBOXLAYOUT_C_H

#include <QtWidgets>
#include "pascalbind.h"

C_EXPORT QBoxLayoutH QBoxLayout_Create(QBoxLayout::Direction AnonParam1, QWidgetH parent);
C_EXPORT void QBoxLayout_Destroy(QBoxLayoutH handle);
C_EXPORT QBoxLayout::Direction QBoxLayout_direction(QBoxLayoutH handle);
C_EXPORT void QBoxLayout_setDirection(QBoxLayoutH handle, QBoxLayout::Direction AnonParam1);
C_EXPORT void QBoxLayout_addSpacing(QBoxLayoutH handle, int size);
C_EXPORT void QBoxLayout_addStretch(QBoxLayoutH handle, int stretch);
C_EXPORT void QBoxLayout_addSpacerItem(QBoxLayoutH handle, QSpacerItemH spacerItem);
C_EXPORT void QBoxLayout_addWidget(QBoxLayoutH handle, QWidgetH AnonParam1, int stretch, unsigned int alignment);
C_EXPORT void QBoxLayout_addLayout(QBoxLayoutH handle, QLayoutH layout, int stretch);
C_EXPORT void QBoxLayout_addStrut(QBoxLayoutH handle, int AnonParam1);
C_EXPORT void QBoxLayout_addItem(QBoxLayoutH handle, QLayoutItemH AnonParam1);
C_EXPORT void QBoxLayout_insertSpacing(QBoxLayoutH handle, int index, int size);
C_EXPORT void QBoxLayout_insertStretch(QBoxLayoutH handle, int index, int stretch);
C_EXPORT void QBoxLayout_insertSpacerItem(QBoxLayoutH handle, int index, QSpacerItemH spacerItem);
C_EXPORT void QBoxLayout_insertWidget(QBoxLayoutH handle, int index, QWidgetH widget, int stretch, unsigned int alignment);
C_EXPORT void QBoxLayout_insertLayout(QBoxLayoutH handle, int index, QLayoutH layout, int stretch);
C_EXPORT void QBoxLayout_insertItem(QBoxLayoutH handle, int index, QLayoutItemH AnonParam2);
C_EXPORT int QBoxLayout_spacing(QBoxLayoutH handle);
C_EXPORT void QBoxLayout_setSpacing(QBoxLayoutH handle, int spacing);
C_EXPORT bool QBoxLayout_setStretchFactor(QBoxLayoutH handle, QWidgetH w, int stretch);
C_EXPORT bool QBoxLayout_setStretchFactor2(QBoxLayoutH handle, QLayoutH l, int stretch);
C_EXPORT void QBoxLayout_setStretch(QBoxLayoutH handle, int index, int stretch);
C_EXPORT int QBoxLayout_stretch(QBoxLayoutH handle, int index);
C_EXPORT void QBoxLayout_sizeHint(QBoxLayoutH handle, PSize retval);
C_EXPORT void QBoxLayout_minimumSize(QBoxLayoutH handle, PSize retval);
C_EXPORT void QBoxLayout_maximumSize(QBoxLayoutH handle, PSize retval);
C_EXPORT bool QBoxLayout_hasHeightForWidth(QBoxLayoutH handle);
C_EXPORT int QBoxLayout_heightForWidth(QBoxLayoutH handle, int AnonParam1);
C_EXPORT int QBoxLayout_minimumHeightForWidth(QBoxLayoutH handle, int AnonParam1);
C_EXPORT unsigned int QBoxLayout_expandingDirections(QBoxLayoutH handle);
C_EXPORT void QBoxLayout_invalidate(QBoxLayoutH handle);
C_EXPORT QLayoutItemH QBoxLayout_itemAt(QBoxLayoutH handle, int AnonParam1);
C_EXPORT QLayoutItemH QBoxLayout_takeAt(QBoxLayoutH handle, int AnonParam1);
C_EXPORT int QBoxLayout_count(QBoxLayoutH handle);
C_EXPORT void QBoxLayout_setGeometry(QBoxLayoutH handle, PRect AnonParam1);
C_EXPORT QHBoxLayoutH QHBoxLayout_Create();
C_EXPORT void QHBoxLayout_Destroy(QHBoxLayoutH handle);
C_EXPORT QHBoxLayoutH QHBoxLayout_Create2(QWidgetH parent);
C_EXPORT QVBoxLayoutH QVBoxLayout_Create();
C_EXPORT void QVBoxLayout_Destroy(QVBoxLayoutH handle);
C_EXPORT QVBoxLayoutH QVBoxLayout_Create2(QWidgetH parent);

#endif
