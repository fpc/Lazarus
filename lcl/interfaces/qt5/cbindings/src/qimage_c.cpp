//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qimage_c.h"

QImageH QImage_Create()
{
	return (QImageH) new QImage();
}

void QImage_Destroy(QImageH handle)
{
	delete (QImage *)handle;
}

QImageH QImage_Create2(const QSizeH size, QImage::Format format)
{
	return (QImageH) new QImage(*(const QSize*)size, format);
}

QImageH QImage_Create3(int width, int height, QImage::Format format)
{
	return (QImageH) new QImage(width, height, format);
}

QImageH QImage_Create4(uchar* data, int width, int height, QImage::Format format, QImageCleanupFunction cleanupFunction, void* cleanupInfo)
{
	return (QImageH) new QImage(data, width, height, format, cleanupFunction, cleanupInfo);
}

QImageH QImage_Create6(uchar* data, int width, int height, int bytesPerLine, QImage::Format format, QImageCleanupFunction cleanupFunction, void* cleanupInfo)
{
	return (QImageH) new QImage(data, width, height, bytesPerLine, format, cleanupFunction, cleanupInfo);
}

QImageH QImage_Create8(const char* const xpm)
{
	return (QImageH) new QImage(xpm);
}

QImageH QImage_Create9(PWideString fileName, const char* format)
{
	QString t_fileName;
	copyPWideStringToQString(fileName, t_fileName);
	return (QImageH) new QImage(t_fileName, format);
}

QImageH QImage_Create10(const QImageH AnonParam1)
{
	return (QImageH) new QImage(*(const QImage*)AnonParam1);
}

void QImage_swap(QImageH handle, QImageH other)
{
	((QImage *)handle)->swap(*(QImage*)other);
}

bool QImage_isNull(QImageH handle)
{
	return (bool) ((QImage *)handle)->isNull();
}

int QImage_devType(QImageH handle)
{
	return (int) ((QImage *)handle)->devType();
}

void QImage_detach(QImageH handle)
{
	((QImage *)handle)->detach();
}

bool QImage_isDetached(QImageH handle)
{
	return (bool) ((QImage *)handle)->isDetached();
}

void QImage_copy(QImageH handle, QImageH retval, PRect rect)
{
	QRect t_rect;
	copyPRectToQRect(rect, t_rect);
	*(QImage *)retval = ((QImage *)handle)->copy(t_rect);
}

void QImage_copy2(QImageH handle, QImageH retval, int x, int y, int w, int h)
{
	*(QImage *)retval = ((QImage *)handle)->copy(x, y, w, h);
}

QImage::Format QImage_format(QImageH handle)
{
	return (QImage::Format) ((QImage *)handle)->format();
}

void QImage_convertToFormat(QImageH handle, QImageH retval, QImage::Format f, unsigned int flags)
{
	*(QImage *)retval = ((QImage *)handle)->convertToFormat(f, (Qt::ImageConversionFlags)flags);
}

int QImage_width(QImageH handle)
{
	return (int) ((QImage *)handle)->width();
}

int QImage_height(QImageH handle)
{
	return (int) ((QImage *)handle)->height();
}

void QImage_size(QImageH handle, PSize retval)
{
	*(QSize *)retval = ((QImage *)handle)->size();
}

void QImage_rect(QImageH handle, PRect retval)
{
	QRect t_retval;
	t_retval = ((QImage *)handle)->rect();
	copyQRectToPRect(t_retval, retval);
}

int QImage_depth(QImageH handle)
{
	return (int) ((QImage *)handle)->depth();
}

int QImage_colorCount(QImageH handle)
{
	return (int) ((QImage *)handle)->colorCount();
}

int QImage_bitPlaneCount(QImageH handle)
{
	return (int) ((QImage *)handle)->bitPlaneCount();
}

QRgb QImage_color(QImageH handle, int i)
{
	return (QRgb) ((QImage *)handle)->color(i);
}

void QImage_setColor(QImageH handle, int i, QRgb c)
{
	((QImage *)handle)->setColor(i, c);
}

void QImage_setColorCount(QImageH handle, int AnonParam1)
{
	((QImage *)handle)->setColorCount(AnonParam1);
}

bool QImage_allGray(QImageH handle)
{
	return (bool) ((QImage *)handle)->allGray();
}

bool QImage_isGrayscale(QImageH handle)
{
	return (bool) ((QImage *)handle)->isGrayscale();
}

uchar* QImage_bits(QImageH handle)
{
	return (uchar*) ((QImage *)handle)->bits();
}

const uchar* QImage_constBits(QImageH handle)
{
	return (const uchar*) ((QImage *)handle)->constBits();
}

int QImage_byteCount(QImageH handle)
{
	return (int) ((QImage *)handle)->byteCount();
}

uchar* QImage_scanLine(QImageH handle, int AnonParam1)
{
	return (uchar*) ((QImage *)handle)->scanLine(AnonParam1);
}

const uchar* QImage_constScanLine(QImageH handle, int AnonParam1)
{
	return (const uchar*) ((QImage *)handle)->constScanLine(AnonParam1);
}

int QImage_bytesPerLine(QImageH handle)
{
	return (int) ((QImage *)handle)->bytesPerLine();
}

bool QImage_valid(QImageH handle, int x, int y)
{
	return (bool) ((QImage *)handle)->valid(x, y);
}

bool QImage_valid2(QImageH handle, const QPointH pt)
{
	return (bool) ((QImage *)handle)->valid(*(const QPoint*)pt);
}

int QImage_pixelIndex(QImageH handle, int x, int y)
{
	return (int) ((QImage *)handle)->pixelIndex(x, y);
}

int QImage_pixelIndex2(QImageH handle, const QPointH pt)
{
	return (int) ((QImage *)handle)->pixelIndex(*(const QPoint*)pt);
}

QRgb QImage_pixel(QImageH handle, int x, int y)
{
	return (QRgb) ((QImage *)handle)->pixel(x, y);
}

QRgb QImage_pixel2(QImageH handle, const QPointH pt)
{
	return (QRgb) ((QImage *)handle)->pixel(*(const QPoint*)pt);
}

void QImage_setPixel(QImageH handle, int x, int y, uint index_or_rgb)
{
	((QImage *)handle)->setPixel(x, y, index_or_rgb);
}

void QImage_setPixel2(QImageH handle, const QPointH pt, uint index_or_rgb)
{
	((QImage *)handle)->setPixel(*(const QPoint*)pt, index_or_rgb);
}

qreal QImage_devicePixelRatio(QImageH handle)
{
	return (qreal) ((QImage *)handle)->devicePixelRatio();
}

void QImage_setDevicePixelRatio(QImageH handle, qreal scaleFactor)
{
	((QImage *)handle)->setDevicePixelRatio(scaleFactor);
}

void QImage_fill(QImageH handle, uint pixel)
{
	((QImage *)handle)->fill(pixel);
}

void QImage_fill2(QImageH handle, const QColorH color)
{
	((QImage *)handle)->fill(*(const QColor*)color);
}

void QImage_fill3(QImageH handle, Qt::GlobalColor color)
{
	((QImage *)handle)->fill(color);
}

bool QImage_hasAlphaChannel(QImageH handle)
{
	return (bool) ((QImage *)handle)->hasAlphaChannel();
}

void QImage_setAlphaChannel(QImageH handle, const QImageH alphaChannel)
{
	((QImage *)handle)->setAlphaChannel(*(const QImage*)alphaChannel);
}

void QImage_alphaChannel(QImageH handle, QImageH retval)
{
	*(QImage *)retval = ((QImage *)handle)->alphaChannel();
}

void QImage_createAlphaMask(QImageH handle, QImageH retval, unsigned int flags)
{
	*(QImage *)retval = ((QImage *)handle)->createAlphaMask((Qt::ImageConversionFlags)flags);
}

void QImage_createHeuristicMask(QImageH handle, QImageH retval, bool clipTight)
{
	*(QImage *)retval = ((QImage *)handle)->createHeuristicMask(clipTight);
}

void QImage_createMaskFromColor(QImageH handle, QImageH retval, QRgb color, Qt::MaskMode mode)
{
	*(QImage *)retval = ((QImage *)handle)->createMaskFromColor(color, mode);
}

void QImage_scaled(QImageH handle, QImageH retval, int w, int h, Qt::AspectRatioMode aspectMode, Qt::TransformationMode mode)
{
	*(QImage *)retval = ((QImage *)handle)->scaled(w, h, aspectMode, mode);
}

void QImage_scaled2(QImageH handle, QImageH retval, const QSizeH s, Qt::AspectRatioMode aspectMode, Qt::TransformationMode mode)
{
	*(QImage *)retval = ((QImage *)handle)->scaled(*(const QSize*)s, aspectMode, mode);
}

void QImage_scaledToWidth(QImageH handle, QImageH retval, int w, Qt::TransformationMode mode)
{
	*(QImage *)retval = ((QImage *)handle)->scaledToWidth(w, mode);
}

void QImage_scaledToHeight(QImageH handle, QImageH retval, int h, Qt::TransformationMode mode)
{
	*(QImage *)retval = ((QImage *)handle)->scaledToHeight(h, mode);
}

void QImage_transformed(QImageH handle, QImageH retval, const QMatrixH matrix, Qt::TransformationMode mode)
{
	*(QImage *)retval = ((QImage *)handle)->transformed(*(const QMatrix*)matrix, mode);
}

void QImage_trueMatrix(QMatrixH retval, const QMatrixH AnonParam1, int w, int h)
{
	*(QMatrix *)retval = QImage::trueMatrix(*(const QMatrix*)AnonParam1, w, h);
}

void QImage_transformed2(QImageH handle, QImageH retval, const QTransformH matrix, Qt::TransformationMode mode)
{
	*(QImage *)retval = ((QImage *)handle)->transformed(*(const QTransform*)matrix, mode);
}

void QImage_trueMatrix2(QTransformH retval, const QTransformH AnonParam1, int w, int h)
{
	*(QTransform *)retval = QImage::trueMatrix(*(const QTransform*)AnonParam1, w, h);
}

void QImage_mirrored(QImageH handle, QImageH retval, bool horizontally, bool vertically)
{
	*(QImage *)retval = ((QImage *)handle)->mirrored(horizontally, vertically);
}

void QImage_rgbSwapped(QImageH handle, QImageH retval)
{
	*(QImage *)retval = ((QImage *)handle)->rgbSwapped();
}

void QImage_invertPixels(QImageH handle, QImage::InvertMode AnonParam1)
{
	((QImage *)handle)->invertPixels(AnonParam1);
}

bool QImage_load(QImageH handle, QIODeviceH device, const char* format)
{
	return (bool) ((QImage *)handle)->load((QIODevice*)device, format);
}

bool QImage_load2(QImageH handle, PWideString fileName, const char* format)
{
	QString t_fileName;
	copyPWideStringToQString(fileName, t_fileName);
	return (bool) ((QImage *)handle)->load(t_fileName, format);
}

bool QImage_loadFromData(QImageH handle, const uchar* buf, int len, const char* format)
{
	return (bool) ((QImage *)handle)->loadFromData(buf, len, format);
}

bool QImage_loadFromData2(QImageH handle, const QByteArrayH data, const char* aformat)
{
	return (bool) ((QImage *)handle)->loadFromData(*(const QByteArray*)data, aformat);
}

bool QImage_save(QImageH handle, PWideString fileName, const char* format, int quality)
{
	QString t_fileName;
	copyPWideStringToQString(fileName, t_fileName);
	return (bool) ((QImage *)handle)->save(t_fileName, format, quality);
}

bool QImage_save2(QImageH handle, QIODeviceH device, const char* format, int quality)
{
	return (bool) ((QImage *)handle)->save((QIODevice*)device, format, quality);
}

void QImage_fromData(QImageH retval, const uchar* data, int size, const char* format)
{
	*(QImage *)retval = QImage::fromData(data, size, format);
}

void QImage_fromData2(QImageH retval, const QByteArrayH data, const char* format)
{
	*(QImage *)retval = QImage::fromData(*(const QByteArray*)data, format);
}

qint64 QImage_cacheKey(QImageH handle)
{
	return (qint64) ((QImage *)handle)->cacheKey();
}

QPaintEngineH QImage_paintEngine(QImageH handle)
{
	return (QPaintEngineH) ((QImage *)handle)->paintEngine();
}

int QImage_dotsPerMeterX(QImageH handle)
{
	return (int) ((QImage *)handle)->dotsPerMeterX();
}

int QImage_dotsPerMeterY(QImageH handle)
{
	return (int) ((QImage *)handle)->dotsPerMeterY();
}

void QImage_setDotsPerMeterX(QImageH handle, int AnonParam1)
{
	((QImage *)handle)->setDotsPerMeterX(AnonParam1);
}

void QImage_setDotsPerMeterY(QImageH handle, int AnonParam1)
{
	((QImage *)handle)->setDotsPerMeterY(AnonParam1);
}

void QImage_offset(QImageH handle, PQtPoint retval)
{
	*(QPoint *)retval = ((QImage *)handle)->offset();
}

void QImage_setOffset(QImageH handle, const QPointH AnonParam1)
{
	((QImage *)handle)->setOffset(*(const QPoint*)AnonParam1);
}

void QImage_textKeys(QImageH handle, QStringListH retval)
{
	*(QStringList *)retval = ((QImage *)handle)->textKeys();
}

void QImage_text(QImageH handle, PWideString retval, PWideString key)
{
	QString t_retval;
	QString t_key;
	copyPWideStringToQString(key, t_key);
	t_retval = ((QImage *)handle)->text(t_key);
	copyQStringToPWideString(t_retval, retval);
}

void QImage_setText(QImageH handle, PWideString key, PWideString value)
{
	QString t_key;
	QString t_value;
	copyPWideStringToQString(key, t_key);
	copyPWideStringToQString(value, t_value);
	((QImage *)handle)->setText(t_key, t_value);
}

