//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QDESKTOPWIDGET_C_H
#define QDESKTOPWIDGET_C_H

#include <QtWidgets>
#include "pascalbind.h"

C_EXPORT QDesktopWidgetH QDesktopWidget_Create();
C_EXPORT void QDesktopWidget_Destroy(QDesktopWidgetH handle);
C_EXPORT bool QDesktopWidget_isVirtualDesktop(QDesktopWidgetH handle);
C_EXPORT int QDesktopWidget_numScreens(QDesktopWidgetH handle);
C_EXPORT int QDesktopWidget_screenCount(QDesktopWidgetH handle);
C_EXPORT int QDesktopWidget_primaryScreen(QDesktopWidgetH handle);
C_EXPORT int QDesktopWidget_screenNumber(QDesktopWidgetH handle, const QWidgetH widget);
C_EXPORT int QDesktopWidget_screenNumber2(QDesktopWidgetH handle, const QPointH AnonParam1);
C_EXPORT QWidgetH QDesktopWidget_screen(QDesktopWidgetH handle, int screen);
C_EXPORT void QDesktopWidget_screenGeometry(QDesktopWidgetH handle, PRect retval, int screen);
C_EXPORT void QDesktopWidget_screenGeometry2(QDesktopWidgetH handle, PRect retval, const QWidgetH widget);
C_EXPORT void QDesktopWidget_screenGeometry3(QDesktopWidgetH handle, PRect retval, const QPointH point);
C_EXPORT void QDesktopWidget_availableGeometry(QDesktopWidgetH handle, PRect retval, int screen);
C_EXPORT void QDesktopWidget_availableGeometry2(QDesktopWidgetH handle, PRect retval, const QWidgetH widget);
C_EXPORT void QDesktopWidget_availableGeometry3(QDesktopWidgetH handle, PRect retval, const QPointH point);

#endif
