//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QSPLITTER_C_H
#define QSPLITTER_C_H

#include <QtWidgets>
#include "pascalbind.h"

C_EXPORT QSplitterH QSplitter_Create(QWidgetH parent);
C_EXPORT void QSplitter_Destroy(QSplitterH handle);
C_EXPORT QSplitterH QSplitter_Create2(Qt::Orientation AnonParam1, QWidgetH parent);
C_EXPORT void QSplitter_addWidget(QSplitterH handle, QWidgetH widget);
C_EXPORT void QSplitter_insertWidget(QSplitterH handle, int index, QWidgetH widget);
C_EXPORT void QSplitter_setOrientation(QSplitterH handle, Qt::Orientation AnonParam1);
C_EXPORT Qt::Orientation QSplitter_orientation(QSplitterH handle);
C_EXPORT void QSplitter_setChildrenCollapsible(QSplitterH handle, bool AnonParam1);
C_EXPORT bool QSplitter_childrenCollapsible(QSplitterH handle);
C_EXPORT void QSplitter_setCollapsible(QSplitterH handle, int index, bool AnonParam2);
C_EXPORT bool QSplitter_isCollapsible(QSplitterH handle, int index);
C_EXPORT void QSplitter_setOpaqueResize(QSplitterH handle, bool opaque);
C_EXPORT bool QSplitter_opaqueResize(QSplitterH handle);
C_EXPORT void QSplitter_refresh(QSplitterH handle);
C_EXPORT void QSplitter_sizeHint(QSplitterH handle, PSize retval);
C_EXPORT void QSplitter_minimumSizeHint(QSplitterH handle, PSize retval);
C_EXPORT void QSplitter_sizes(QSplitterH handle, PPtrIntArray retval);
C_EXPORT void QSplitter_setSizes(QSplitterH handle, PPtrIntArray list);
C_EXPORT void QSplitter_saveState(QSplitterH handle, QByteArrayH retval);
C_EXPORT bool QSplitter_restoreState(QSplitterH handle, const QByteArrayH state);
C_EXPORT int QSplitter_handleWidth(QSplitterH handle);
C_EXPORT void QSplitter_setHandleWidth(QSplitterH handle, int AnonParam1);
C_EXPORT int QSplitter_indexOf(QSplitterH handle, QWidgetH w);
C_EXPORT QWidgetH QSplitter_widget(QSplitterH handle, int index);
C_EXPORT int QSplitter_count(QSplitterH handle);
C_EXPORT void QSplitter_getRange(QSplitterH handle, int index, int* AnonParam2, int* AnonParam3);
C_EXPORT QSplitterHandleH QSplitter_handle(QSplitterH handle, int index);
C_EXPORT void QSplitter_setStretchFactor(QSplitterH handle, int index, int stretch);
C_EXPORT QSplitterHandleH QSplitterHandle_Create(Qt::Orientation o, QSplitterH parent);
C_EXPORT void QSplitterHandle_Destroy(QSplitterHandleH handle);
C_EXPORT void QSplitterHandle_setOrientation(QSplitterHandleH handle, Qt::Orientation o);
C_EXPORT Qt::Orientation QSplitterHandle_orientation(QSplitterHandleH handle);
C_EXPORT bool QSplitterHandle_opaqueResize(QSplitterHandleH handle);
C_EXPORT QSplitterH QSplitterHandle_splitter(QSplitterHandleH handle);
C_EXPORT void QSplitterHandle_sizeHint(QSplitterHandleH handle, PSize retval);

#endif
