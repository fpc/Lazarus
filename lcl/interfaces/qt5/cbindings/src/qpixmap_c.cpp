//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qpixmap_c.h"

QPixmapH QPixmap_Create()
{
	return (QPixmapH) new QPixmap();
}

void QPixmap_Destroy(QPixmapH handle)
{
	delete (QPixmap *)handle;
}

QPixmapH QPixmap_Create2(int w, int h)
{
	return (QPixmapH) new QPixmap(w, h);
}

QPixmapH QPixmap_Create3(const QSizeH AnonParam1)
{
	return (QPixmapH) new QPixmap(*(const QSize*)AnonParam1);
}

QPixmapH QPixmap_Create4(PWideString fileName, const char* format, unsigned int flags)
{
	QString t_fileName;
	copyPWideStringToQString(fileName, t_fileName);
	return (QPixmapH) new QPixmap(t_fileName, format, (Qt::ImageConversionFlags)flags);
}

QPixmapH QPixmap_Create5(const char* const xpm)
{
	return (QPixmapH) new QPixmap(xpm);
}

QPixmapH QPixmap_Create6(const QPixmapH AnonParam1)
{
	return (QPixmapH) new QPixmap(*(const QPixmap*)AnonParam1);
}

void QPixmap_swap(QPixmapH handle, QPixmapH other)
{
	((QPixmap *)handle)->swap(*(QPixmap*)other);
}

bool QPixmap_isNull(QPixmapH handle)
{
	return (bool) ((QPixmap *)handle)->isNull();
}

int QPixmap_devType(QPixmapH handle)
{
	return (int) ((QPixmap *)handle)->devType();
}

int QPixmap_width(QPixmapH handle)
{
	return (int) ((QPixmap *)handle)->width();
}

int QPixmap_height(QPixmapH handle)
{
	return (int) ((QPixmap *)handle)->height();
}

void QPixmap_size(QPixmapH handle, PSize retval)
{
	*(QSize *)retval = ((QPixmap *)handle)->size();
}

void QPixmap_rect(QPixmapH handle, PRect retval)
{
	QRect t_retval;
	t_retval = ((QPixmap *)handle)->rect();
	copyQRectToPRect(t_retval, retval);
}

int QPixmap_depth(QPixmapH handle)
{
	return (int) ((QPixmap *)handle)->depth();
}

int QPixmap_defaultDepth()
{
	return (int) QPixmap::defaultDepth();
}

void QPixmap_fill(QPixmapH handle, const QColorH fillColor)
{
	((QPixmap *)handle)->fill(*(const QColor*)fillColor);
}

void QPixmap_fill2(QPixmapH handle, const QPaintDeviceH device, const QPointH ofs)
{
	((QPixmap *)handle)->fill((const QPaintDevice*)device, *(const QPoint*)ofs);
}

void QPixmap_fill3(QPixmapH handle, const QPaintDeviceH device, int xofs, int yofs)
{
	((QPixmap *)handle)->fill((const QPaintDevice*)device, xofs, yofs);
}

void QPixmap_mask(QPixmapH handle, QBitmapH retval)
{
	*(QBitmap *)retval = ((QPixmap *)handle)->mask();
}

void QPixmap_setMask(QPixmapH handle, const QBitmapH AnonParam1)
{
	((QPixmap *)handle)->setMask(*(const QBitmap*)AnonParam1);
}

qreal QPixmap_devicePixelRatio(QPixmapH handle)
{
	return (qreal) ((QPixmap *)handle)->devicePixelRatio();
}

void QPixmap_setDevicePixelRatio(QPixmapH handle, qreal scaleFactor)
{
	((QPixmap *)handle)->setDevicePixelRatio(scaleFactor);
}

bool QPixmap_hasAlpha(QPixmapH handle)
{
	return (bool) ((QPixmap *)handle)->hasAlpha();
}

bool QPixmap_hasAlphaChannel(QPixmapH handle)
{
	return (bool) ((QPixmap *)handle)->hasAlphaChannel();
}

void QPixmap_createHeuristicMask(QPixmapH handle, QBitmapH retval, bool clipTight)
{
	*(QBitmap *)retval = ((QPixmap *)handle)->createHeuristicMask(clipTight);
}

void QPixmap_createMaskFromColor(QPixmapH handle, QBitmapH retval, const QColorH maskColor, Qt::MaskMode mode)
{
	*(QBitmap *)retval = ((QPixmap *)handle)->createMaskFromColor(*(const QColor*)maskColor, mode);
}

void QPixmap_grabWindow(QPixmapH retval, WId AnonParam1, int x, int y, int w, int h)
{
	*(QPixmap *)retval = QPixmap::grabWindow((WId)AnonParam1, x, y, w, h);
}

void QPixmap_grabWidget(QPixmapH retval, QObjectH widget, PRect rect)
{
	QRect t_rect;
	copyPRectToQRect(rect, t_rect);
	*(QPixmap *)retval = QPixmap::grabWidget((QObject*)widget, t_rect);
}

void QPixmap_grabWidget2(QPixmapH retval, QObjectH widget, int x, int y, int w, int h)
{
	*(QPixmap *)retval = QPixmap::grabWidget((QObject*)widget, x, y, w, h);
}

void QPixmap_scaled(QPixmapH handle, QPixmapH retval, int w, int h, Qt::AspectRatioMode aspectMode, Qt::TransformationMode mode)
{
	*(QPixmap *)retval = ((QPixmap *)handle)->scaled(w, h, aspectMode, mode);
}

void QPixmap_scaled2(QPixmapH handle, QPixmapH retval, const QSizeH s, Qt::AspectRatioMode aspectMode, Qt::TransformationMode mode)
{
	*(QPixmap *)retval = ((QPixmap *)handle)->scaled(*(const QSize*)s, aspectMode, mode);
}

void QPixmap_scaledToWidth(QPixmapH handle, QPixmapH retval, int w, Qt::TransformationMode mode)
{
	*(QPixmap *)retval = ((QPixmap *)handle)->scaledToWidth(w, mode);
}

void QPixmap_scaledToHeight(QPixmapH handle, QPixmapH retval, int h, Qt::TransformationMode mode)
{
	*(QPixmap *)retval = ((QPixmap *)handle)->scaledToHeight(h, mode);
}

void QPixmap_transformed(QPixmapH handle, QPixmapH retval, const QMatrixH AnonParam1, Qt::TransformationMode mode)
{
	*(QPixmap *)retval = ((QPixmap *)handle)->transformed(*(const QMatrix*)AnonParam1, mode);
}

void QPixmap_trueMatrix(QMatrixH retval, const QMatrixH m, int w, int h)
{
	*(QMatrix *)retval = QPixmap::trueMatrix(*(const QMatrix*)m, w, h);
}

void QPixmap_transformed2(QPixmapH handle, QPixmapH retval, const QTransformH AnonParam1, Qt::TransformationMode mode)
{
	*(QPixmap *)retval = ((QPixmap *)handle)->transformed(*(const QTransform*)AnonParam1, mode);
}

void QPixmap_trueMatrix2(QTransformH retval, const QTransformH m, int w, int h)
{
	*(QTransform *)retval = QPixmap::trueMatrix(*(const QTransform*)m, w, h);
}

void QPixmap_toImage(QPixmapH handle, QImageH retval)
{
	*(QImage *)retval = ((QPixmap *)handle)->toImage();
}

void QPixmap_fromImage(QPixmapH retval, const QImageH image, unsigned int flags)
{
	*(QPixmap *)retval = QPixmap::fromImage(*(const QImage*)image, (Qt::ImageConversionFlags)flags);
}

void QPixmap_fromImageReader(QPixmapH retval, QImageReaderH imageReader, unsigned int flags)
{
	*(QPixmap *)retval = QPixmap::fromImageReader((QImageReader*)imageReader, (Qt::ImageConversionFlags)flags);
}

bool QPixmap_load(QPixmapH handle, PWideString fileName, const char* format, unsigned int flags)
{
	QString t_fileName;
	copyPWideStringToQString(fileName, t_fileName);
	return (bool) ((QPixmap *)handle)->load(t_fileName, format, (Qt::ImageConversionFlags)flags);
}

bool QPixmap_loadFromData(QPixmapH handle, const uchar* buf, uint len, const char* format, unsigned int flags)
{
	return (bool) ((QPixmap *)handle)->loadFromData(buf, len, format, (Qt::ImageConversionFlags)flags);
}

bool QPixmap_loadFromData2(QPixmapH handle, const QByteArrayH data, const char* format, unsigned int flags)
{
	return (bool) ((QPixmap *)handle)->loadFromData(*(const QByteArray*)data, format, (Qt::ImageConversionFlags)flags);
}

bool QPixmap_save(QPixmapH handle, PWideString fileName, const char* format, int quality)
{
	QString t_fileName;
	copyPWideStringToQString(fileName, t_fileName);
	return (bool) ((QPixmap *)handle)->save(t_fileName, format, quality);
}

bool QPixmap_save2(QPixmapH handle, QIODeviceH device, const char* format, int quality)
{
	return (bool) ((QPixmap *)handle)->save((QIODevice*)device, format, quality);
}

bool QPixmap_convertFromImage(QPixmapH handle, const QImageH img, unsigned int flags)
{
	return (bool) ((QPixmap *)handle)->convertFromImage(*(const QImage*)img, (Qt::ImageConversionFlags)flags);
}

void QPixmap_copy(QPixmapH handle, QPixmapH retval, int x, int y, int width, int height)
{
	*(QPixmap *)retval = ((QPixmap *)handle)->copy(x, y, width, height);
}

void QPixmap_copy2(QPixmapH handle, QPixmapH retval, PRect rect)
{
	QRect t_rect;
	copyPRectToQRect(rect, t_rect);
	*(QPixmap *)retval = ((QPixmap *)handle)->copy(t_rect);
}

void QPixmap_scroll(QPixmapH handle, int dx, int dy, int x, int y, int width, int height, QRegionH exposed)
{
	((QPixmap *)handle)->scroll(dx, dy, x, y, width, height, (QRegion*)exposed);
}

void QPixmap_scroll2(QPixmapH handle, int dx, int dy, PRect rect, QRegionH exposed)
{
	QRect t_rect;
	copyPRectToQRect(rect, t_rect);
	((QPixmap *)handle)->scroll(dx, dy, t_rect, (QRegion*)exposed);
}

qint64 QPixmap_cacheKey(QPixmapH handle)
{
	return (qint64) ((QPixmap *)handle)->cacheKey();
}

bool QPixmap_isDetached(QPixmapH handle)
{
	return (bool) ((QPixmap *)handle)->isDetached();
}

void QPixmap_detach(QPixmapH handle)
{
	((QPixmap *)handle)->detach();
}

bool QPixmap_isQBitmap(QPixmapH handle)
{
	return (bool) ((QPixmap *)handle)->isQBitmap();
}

QPaintEngineH QPixmap_paintEngine(QPixmapH handle)
{
	return (QPaintEngineH) ((QPixmap *)handle)->paintEngine();
}

