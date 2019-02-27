//******************************************************************************
//  Copyright (c) 2007 by Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QSCREEN_C_H
#define QSCREEN_C_H

#include <QtGui>
#include "pascalbind.h"

C_EXPORT void QScreen_Destroy(QScreenH handle);
C_EXPORT void QScreen_name(QScreenH handle, PWideString retval);
C_EXPORT int QScreen_depth(QScreenH handle);
C_EXPORT void QScreen_size(QScreenH handle, PSize retval);
C_EXPORT void QScreen_geometry(QScreenH handle, PRect retval);
C_EXPORT void QScreen_physicalSize(QScreenH handle, QSizeFH retval);
C_EXPORT qreal QScreen_physicalDotsPerInchX(QScreenH handle);
C_EXPORT qreal QScreen_physicalDotsPerInchY(QScreenH handle);
C_EXPORT qreal QScreen_physicalDotsPerInch(QScreenH handle);
C_EXPORT qreal QScreen_logicalDotsPerInchX(QScreenH handle);
C_EXPORT qreal QScreen_logicalDotsPerInchY(QScreenH handle);
C_EXPORT qreal QScreen_logicalDotsPerInch(QScreenH handle);
C_EXPORT qreal QScreen_devicePixelRatio(QScreenH handle);
C_EXPORT void QScreen_availableSize(QScreenH handle, PSize retval);
C_EXPORT void QScreen_availableGeometry(QScreenH handle, PRect retval);
C_EXPORT void QScreen_virtualSize(QScreenH handle, PSize retval);
C_EXPORT void QScreen_virtualGeometry(QScreenH handle, PRect retval);
C_EXPORT Qt::ScreenOrientation QScreen_primaryOrientation(QScreenH handle);
C_EXPORT Qt::ScreenOrientation QScreen_orientation(QScreenH handle);
C_EXPORT Qt::ScreenOrientation QScreen_nativeOrientation(QScreenH handle);
C_EXPORT Qt::ScreenOrientations QScreen_orientationUpdateMask(QScreenH handle);
C_EXPORT void QScreen_setOrientationUpdateMask(QScreenH handle, Qt::ScreenOrientations mask);
C_EXPORT int QScreen_angleBetween(QScreenH handle, Qt::ScreenOrientation a, Qt::ScreenOrientation b);
C_EXPORT void QScreen_virtualSiblings(QScreenH handle, PPtrIntArray retval);
C_EXPORT void QScreen_transformBetween(QScreenH handle, QTransformH retval, Qt::ScreenOrientation a, Qt::ScreenOrientation b, PRect AnonParam1);
C_EXPORT void QScreen_mapBetween(QScreenH handle, PRect retval, Qt::ScreenOrientation a, Qt::ScreenOrientation b, PRect AnonParam1);
C_EXPORT bool QScreen_isPortrait(QScreenH handle, Qt::ScreenOrientation orientation);
C_EXPORT bool QScreen_isLandscape(QScreenH handle, Qt::ScreenOrientation orientation);
C_EXPORT void QScreen_grabWindow(QScreenH handle, QPixmapH retval,WId window, int x = 0,int y = 0,int w = -1, int h = -1);
C_EXPORT qreal QScreen_refreshRate(QScreenH handle);

#endif
