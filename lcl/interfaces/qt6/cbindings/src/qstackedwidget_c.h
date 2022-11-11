//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QSTACKEDWIDGET_C_H
#define QSTACKEDWIDGET_C_H

#include <QtWidgets>
#include "pascalbind.h"

C_EXPORT QStackedWidgetH QStackedWidget_Create(QWidgetH parent);
C_EXPORT void QStackedWidget_Destroy(QStackedWidgetH handle);
C_EXPORT int QStackedWidget_addWidget(QStackedWidgetH handle, QWidgetH w);
C_EXPORT int QStackedWidget_insertWidget(QStackedWidgetH handle, int index, QWidgetH w);
C_EXPORT void QStackedWidget_removeWidget(QStackedWidgetH handle, QWidgetH w);
C_EXPORT QWidgetH QStackedWidget_currentWidget(QStackedWidgetH handle);
C_EXPORT int QStackedWidget_currentIndex(QStackedWidgetH handle);
C_EXPORT int QStackedWidget_indexOf(QStackedWidgetH handle, QWidgetH AnonParam1);
C_EXPORT QWidgetH QStackedWidget_widget(QStackedWidgetH handle, int AnonParam1);
C_EXPORT int QStackedWidget_count(QStackedWidgetH handle);
C_EXPORT void QStackedWidget_setCurrentIndex(QStackedWidgetH handle, int index);
C_EXPORT void QStackedWidget_setCurrentWidget(QStackedWidgetH handle, QWidgetH w);

#endif
