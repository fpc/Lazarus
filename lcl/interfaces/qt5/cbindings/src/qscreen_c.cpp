//******************************************************************************
//  Copyright (c) 2017 by Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qscreen_c.h"

void QScreen_Destroy(QScreenH handle)
{
  delete (QScreen *)handle;
}

void QScreen_name(QScreenH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QScreen *)handle)->name();
	copyQStringToPWideString(t_retval, retval);
}

int QScreen_depth(QScreenH handle)
{
  return (int) ((QScreen *)handle)->depth();
}

void QScreen_size(QScreenH handle, PSize retval)
{
	*(QSize *)retval = ((QScreen *)handle)->size();
}

void QScreen_geometry(QScreenH handle, PRect retval)
{
	QRect t_retval;
	t_retval = ((QScreen *)handle)->geometry();
	copyQRectToPRect(t_retval, retval);
}

void QScreen_physicalSize(QScreenH handle, QSizeFH retval)
{
	*(QSizeF *)retval = ((QScreen *)handle)->physicalSize();
}

qreal QScreen_physicalDotsPerInchX(QScreenH handle)
{
  return (qreal) ((QScreen *)handle)->physicalDotsPerInchX();
}

qreal QScreen_physicalDotsPerInchY(QScreenH handle)
{
  return (qreal) ((QScreen *)handle)->physicalDotsPerInchY();
}

qreal QScreen_physicalDotsPerInch(QScreenH handle)
{
  return (qreal) ((QScreen *)handle)->physicalDotsPerInch();
}

qreal QScreen_logicalDotsPerInchX(QScreenH handle)
{
  return (qreal) ((QScreen *)handle)->logicalDotsPerInchX();
}

qreal QScreen_logicalDotsPerInchY(QScreenH handle)
{
  return (qreal) ((QScreen *)handle)->logicalDotsPerInchY();
}

qreal QScreen_logicalDotsPerInch(QScreenH handle)
{
  return (qreal) ((QScreen *)handle)->logicalDotsPerInch();
}

qreal QScreen_devicePixelRatio(QScreenH handle)
{
  return (qreal) ((QScreen *)handle)->devicePixelRatio();
}

void QScreen_availableSize(QScreenH handle, PSize retval)
{
	*(QSize *)retval = ((QScreen *)handle)->availableSize();
}

void QScreen_availableGeometry(QScreenH handle, PRect retval)
{
	QRect t_retval;
	t_retval = ((QScreen *)handle)->availableGeometry();
	copyQRectToPRect(t_retval, retval);
}

void QScreen_virtualSize(QScreenH handle, PSize retval)
{
	*(QSize *)retval = ((QScreen *)handle)->virtualSize();
}

void QScreen_virtualGeometry(QScreenH handle, PRect retval)
{
	QRect t_retval;
	t_retval = ((QScreen *)handle)->virtualGeometry();
	copyQRectToPRect(t_retval, retval);
}

Qt::ScreenOrientation QScreen_primaryOrientation(QScreenH handle)
{
  return (Qt::ScreenOrientation) ((QScreen *)handle)->primaryOrientation();
}

Qt::ScreenOrientation QScreen_orientation(QScreenH handle)
{
  return (Qt::ScreenOrientation) ((QScreen *)handle)->orientation();
}

Qt::ScreenOrientation QScreen_nativeOrientation(QScreenH handle)
{
  return (Qt::ScreenOrientation) ((QScreen *)handle)->nativeOrientation();
}

Qt::ScreenOrientations QScreen_orientationUpdateMask(QScreenH handle)
{
  return (Qt::ScreenOrientations) ((QScreen *)handle)->orientationUpdateMask();
}

void QScreen_setOrientationUpdateMask(QScreenH handle, Qt::ScreenOrientations mask)
{
  ((QScreen *)handle)->setOrientationUpdateMask((Qt::ScreenOrientations) mask);
}

int QScreen_angleBetween(QScreenH handle, Qt::ScreenOrientation a, Qt::ScreenOrientation b)
{
  return (int) ((QScreen *)handle)->angleBetween((Qt::ScreenOrientation) a, (Qt::ScreenOrientation) b);
}

void QScreen_virtualSiblings(QScreenH handle, PPtrIntArray retval)
{
	QList<QScreen*> t_retval;
	t_retval = ((QScreen *)handle)->virtualSiblings();
	copyQListTemplateToPtrIntArray(t_retval, retval);
}

void QScreen_transformBetween(QScreenH handle, QTransformH retval, Qt::ScreenOrientation a, Qt::ScreenOrientation b, PRect AnonParam1)
{
	QRect t_AnonParam1;
	copyPRectToQRect(AnonParam1, t_AnonParam1);
	*(QTransform *)retval = ((QScreen *)handle)->transformBetween((Qt::ScreenOrientation) a, (Qt::ScreenOrientation) b, t_AnonParam1);
}

void QScreen_mapBetween(QScreenH handle, PRect retval, Qt::ScreenOrientation a, Qt::ScreenOrientation b, PRect AnonParam1)
{
	QRect t_AnonParam1;
  QRect t_retval;
	copyPRectToQRect(AnonParam1, t_AnonParam1);
  t_retval = ((QScreen *)handle)->mapBetween((Qt::ScreenOrientation) a, (Qt::ScreenOrientation) b, t_AnonParam1);
  copyQRectToPRect(t_retval, retval);
}

bool QScreen_isPortrait(QScreenH handle, Qt::ScreenOrientation orientation)
{
  return (bool) ((QScreen *)handle)->isPortrait((Qt::ScreenOrientation) orientation);
}

bool QScreen_isLandscape(QScreenH handle, Qt::ScreenOrientation orientation)
{
  return (bool) ((QScreen *)handle)->isLandscape((Qt::ScreenOrientation) orientation);
}

void QScreen_grabWindow(QScreenH handle, QPixmapH retval,WId window, int x,int y,int w, int h)
{
  *(QPixmap *)retval = ((QScreen *)handle)->grabWindow((WId) window, x, y, w, h);
}

qreal QScreen_refreshRate(QScreenH handle)
{
  return (qreal) ((QScreen *)handle)->refreshRate();
}
