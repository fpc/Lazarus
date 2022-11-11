//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QSTACKEDLAYOUT_C_H
#define QSTACKEDLAYOUT_C_H

#include <QtWidgets>
#include "pascalbind.h"

C_EXPORT QStackedLayoutH QStackedLayout_Create();
C_EXPORT void QStackedLayout_Destroy(QStackedLayoutH handle);
C_EXPORT QStackedLayoutH QStackedLayout_Create2(QWidgetH parent);
C_EXPORT QStackedLayoutH QStackedLayout_Create3(QLayoutH parentLayout);
C_EXPORT int QStackedLayout_addWidget(QStackedLayoutH handle, QWidgetH w);
C_EXPORT int QStackedLayout_insertWidget(QStackedLayoutH handle, int index, QWidgetH w);
C_EXPORT QWidgetH QStackedLayout_currentWidget(QStackedLayoutH handle);
C_EXPORT int QStackedLayout_currentIndex(QStackedLayoutH handle);
C_EXPORT QWidgetH QStackedLayout_widget(QStackedLayoutH handle, int AnonParam1);
C_EXPORT int QStackedLayout_count(QStackedLayoutH handle);
C_EXPORT QStackedLayout::StackingMode QStackedLayout_stackingMode(QStackedLayoutH handle);
C_EXPORT void QStackedLayout_setStackingMode(QStackedLayoutH handle, QStackedLayout::StackingMode stackingMode);
C_EXPORT void QStackedLayout_addItem(QStackedLayoutH handle, QLayoutItemH item);
C_EXPORT void QStackedLayout_sizeHint(QStackedLayoutH handle, PSize retval);
C_EXPORT void QStackedLayout_minimumSize(QStackedLayoutH handle, PSize retval);
C_EXPORT QLayoutItemH QStackedLayout_itemAt(QStackedLayoutH handle, int AnonParam1);
C_EXPORT QLayoutItemH QStackedLayout_takeAt(QStackedLayoutH handle, int AnonParam1);
C_EXPORT void QStackedLayout_setGeometry(QStackedLayoutH handle, PRect rect);
C_EXPORT bool QStackedLayout_hasHeightForWidth(QStackedLayoutH handle);
C_EXPORT int QStackedLayout_heightForWidth(QStackedLayoutH handle, int width);
C_EXPORT void QStackedLayout_setCurrentIndex(QStackedLayoutH handle, int index);
C_EXPORT void QStackedLayout_setCurrentWidget(QStackedLayoutH handle, QWidgetH w);

#endif
