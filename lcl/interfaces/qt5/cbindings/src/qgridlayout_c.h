//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QGRIDLAYOUT_C_H
#define QGRIDLAYOUT_C_H

#include <QtWidgets>
#include "pascalbind.h"

C_EXPORT QGridLayoutH QGridLayout_Create(QWidgetH parent);
C_EXPORT void QGridLayout_Destroy(QGridLayoutH handle);
C_EXPORT QGridLayoutH QGridLayout_Create2();
C_EXPORT void QGridLayout_sizeHint(QGridLayoutH handle, PSize retval);
C_EXPORT void QGridLayout_minimumSize(QGridLayoutH handle, PSize retval);
C_EXPORT void QGridLayout_maximumSize(QGridLayoutH handle, PSize retval);
C_EXPORT void QGridLayout_setHorizontalSpacing(QGridLayoutH handle, int spacing);
C_EXPORT int QGridLayout_horizontalSpacing(QGridLayoutH handle);
C_EXPORT void QGridLayout_setVerticalSpacing(QGridLayoutH handle, int spacing);
C_EXPORT int QGridLayout_verticalSpacing(QGridLayoutH handle);
C_EXPORT void QGridLayout_setSpacing(QGridLayoutH handle, int spacing);
C_EXPORT int QGridLayout_spacing(QGridLayoutH handle);
C_EXPORT void QGridLayout_setRowStretch(QGridLayoutH handle, int row, int stretch);
C_EXPORT void QGridLayout_setColumnStretch(QGridLayoutH handle, int column, int stretch);
C_EXPORT int QGridLayout_rowStretch(QGridLayoutH handle, int row);
C_EXPORT int QGridLayout_columnStretch(QGridLayoutH handle, int column);
C_EXPORT void QGridLayout_setRowMinimumHeight(QGridLayoutH handle, int row, int minSize);
C_EXPORT void QGridLayout_setColumnMinimumWidth(QGridLayoutH handle, int column, int minSize);
C_EXPORT int QGridLayout_rowMinimumHeight(QGridLayoutH handle, int row);
C_EXPORT int QGridLayout_columnMinimumWidth(QGridLayoutH handle, int column);
C_EXPORT int QGridLayout_columnCount(QGridLayoutH handle);
C_EXPORT int QGridLayout_rowCount(QGridLayoutH handle);
C_EXPORT void QGridLayout_cellRect(QGridLayoutH handle, PRect retval, int row, int column);
C_EXPORT bool QGridLayout_hasHeightForWidth(QGridLayoutH handle);
C_EXPORT int QGridLayout_heightForWidth(QGridLayoutH handle, int AnonParam1);
C_EXPORT int QGridLayout_minimumHeightForWidth(QGridLayoutH handle, int AnonParam1);
C_EXPORT unsigned int QGridLayout_expandingDirections(QGridLayoutH handle);
C_EXPORT void QGridLayout_invalidate(QGridLayoutH handle);
C_EXPORT void QGridLayout_addWidget(QGridLayoutH handle, QWidgetH w);
C_EXPORT void QGridLayout_addWidget2(QGridLayoutH handle, QWidgetH AnonParam1, int row, int column, unsigned int AnonParam4);
C_EXPORT void QGridLayout_addWidget3(QGridLayoutH handle, QWidgetH AnonParam1, int row, int column, int rowSpan, int columnSpan, unsigned int AnonParam6);
C_EXPORT void QGridLayout_addLayout(QGridLayoutH handle, QLayoutH AnonParam1, int row, int column, unsigned int AnonParam4);
C_EXPORT void QGridLayout_addLayout2(QGridLayoutH handle, QLayoutH AnonParam1, int row, int column, int rowSpan, int columnSpan, unsigned int AnonParam6);
C_EXPORT void QGridLayout_setOriginCorner(QGridLayoutH handle, Qt::Corner AnonParam1);
C_EXPORT Qt::Corner QGridLayout_originCorner(QGridLayoutH handle);
C_EXPORT QLayoutItemH QGridLayout_itemAt(QGridLayoutH handle, int index);
C_EXPORT QLayoutItemH QGridLayout_itemAtPosition(QGridLayoutH handle, int row, int column);
C_EXPORT QLayoutItemH QGridLayout_takeAt(QGridLayoutH handle, int index);
C_EXPORT int QGridLayout_count(QGridLayoutH handle);
C_EXPORT void QGridLayout_setGeometry(QGridLayoutH handle, PRect AnonParam1);
C_EXPORT void QGridLayout_addItem(QGridLayoutH handle, QLayoutItemH item, int row, int column, int rowSpan, int columnSpan, unsigned int AnonParam6);
C_EXPORT void QGridLayout_setDefaultPositioning(QGridLayoutH handle, int n, Qt::Orientation orient);
C_EXPORT void QGridLayout_getItemPosition(QGridLayoutH handle, int idx, int* row, int* column, int* rowSpan, int* columnSpan);

#endif
