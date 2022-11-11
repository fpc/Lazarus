//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QABSTRACTSCROLLAREA_C_H
#define QABSTRACTSCROLLAREA_C_H

#include <QtWidgets>
#include "pascalbind.h"

C_EXPORT QAbstractScrollAreaH QAbstractScrollArea_Create(QWidgetH parent);
C_EXPORT void QAbstractScrollArea_Destroy(QAbstractScrollAreaH handle);
C_EXPORT Qt::ScrollBarPolicy QAbstractScrollArea_verticalScrollBarPolicy(QAbstractScrollAreaH handle);
C_EXPORT void QAbstractScrollArea_setVerticalScrollBarPolicy(QAbstractScrollAreaH handle, Qt::ScrollBarPolicy AnonParam1);
C_EXPORT QScrollBarH QAbstractScrollArea_verticalScrollBar(QAbstractScrollAreaH handle);
C_EXPORT void QAbstractScrollArea_setVerticalScrollBar(QAbstractScrollAreaH handle, QScrollBarH scrollbar);
C_EXPORT Qt::ScrollBarPolicy QAbstractScrollArea_horizontalScrollBarPolicy(QAbstractScrollAreaH handle);
C_EXPORT void QAbstractScrollArea_setHorizontalScrollBarPolicy(QAbstractScrollAreaH handle, Qt::ScrollBarPolicy AnonParam1);
C_EXPORT QScrollBarH QAbstractScrollArea_horizontalScrollBar(QAbstractScrollAreaH handle);
C_EXPORT void QAbstractScrollArea_setHorizontalScrollBar(QAbstractScrollAreaH handle, QScrollBarH scrollbar);
C_EXPORT QWidgetH QAbstractScrollArea_cornerWidget(QAbstractScrollAreaH handle);
C_EXPORT void QAbstractScrollArea_setCornerWidget(QAbstractScrollAreaH handle, QWidgetH widget);
C_EXPORT void QAbstractScrollArea_addScrollBarWidget(QAbstractScrollAreaH handle, QWidgetH widget, unsigned int alignment);
C_EXPORT void QAbstractScrollArea_scrollBarWidgets(QAbstractScrollAreaH handle, PPtrIntArray retval, unsigned int alignment);
C_EXPORT QWidgetH QAbstractScrollArea_viewport(QAbstractScrollAreaH handle);
C_EXPORT void QAbstractScrollArea_setViewport(QAbstractScrollAreaH handle, QWidgetH widget);
C_EXPORT void QAbstractScrollArea_maximumViewportSize(QAbstractScrollAreaH handle, PSize retval);
C_EXPORT void QAbstractScrollArea_minimumSizeHint(QAbstractScrollAreaH handle, PSize retval);
C_EXPORT void QAbstractScrollArea_sizeHint(QAbstractScrollAreaH handle, PSize retval);
C_EXPORT void QAbstractScrollArea_setupViewport(QAbstractScrollAreaH handle, QWidgetH viewport);

#endif
