//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QDOCKWIDGET_C_H
#define QDOCKWIDGET_C_H

#include <QtWidgets>
#include "pascalbind.h"

C_EXPORT QDockWidgetH QDockWidget_Create(PWideString title, QWidgetH parent, unsigned int flags);
C_EXPORT void QDockWidget_Destroy(QDockWidgetH handle);
C_EXPORT QDockWidgetH QDockWidget_Create2(QWidgetH parent, unsigned int flags);
C_EXPORT QWidgetH QDockWidget_widget(QDockWidgetH handle);
C_EXPORT void QDockWidget_setWidget(QDockWidgetH handle, QWidgetH widget);
C_EXPORT void QDockWidget_setFeatures(QDockWidgetH handle, unsigned int features);
C_EXPORT unsigned int QDockWidget_features(QDockWidgetH handle);
C_EXPORT void QDockWidget_setFloating(QDockWidgetH handle, bool floating);
C_EXPORT bool QDockWidget_isFloating(QDockWidgetH handle);
C_EXPORT void QDockWidget_setAllowedAreas(QDockWidgetH handle, unsigned int areas);
C_EXPORT unsigned int QDockWidget_allowedAreas(QDockWidgetH handle);
C_EXPORT void QDockWidget_setTitleBarWidget(QDockWidgetH handle, QWidgetH widget);
C_EXPORT QWidgetH QDockWidget_titleBarWidget(QDockWidgetH handle);
C_EXPORT bool QDockWidget_isAreaAllowed(QDockWidgetH handle, Qt::DockWidgetArea area);
C_EXPORT QActionH QDockWidget_toggleViewAction(QDockWidgetH handle);

#endif
