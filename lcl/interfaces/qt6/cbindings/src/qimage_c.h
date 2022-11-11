//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************

/* TODO pixel reading , new functions, QColorSpace class */

#ifndef QIMAGE_C_H
#define QIMAGE_C_H

#include <QtGui>
#include "pascalbind.h"

C_EXPORT QImageH QImage_Create();
C_EXPORT void QImage_Destroy(QImageH handle);
C_EXPORT QImageH QImage_Create2(const QSizeH size, QImage::Format format);
C_EXPORT QImageH QImage_Create3(int width, int height, QImage::Format format);
C_EXPORT QImageH QImage_Create4(uchar* data, int width, int height, QImage::Format format, QImageCleanupFunction cleanupFunction, void* cleanupInfo);
C_EXPORT QImageH QImage_Create6(uchar* data, int width, int height, int bytesPerLine, QImage::Format format, QImageCleanupFunction cleanupFunction, void* cleanupInfo);
C_EXPORT QImageH QImage_Create8(const char* const xpm);
C_EXPORT QImageH QImage_Create9(PWideString fileName, const char* format);
C_EXPORT QImageH QImage_Create10(const QImageH AnonParam1);

C_EXPORT bool QImage_allGray(QImageH handle);
C_EXPORT void QImage_applyColorTransform(QImageH handle, const QColorTransformH transform);
C_EXPORT int QImage_bitPlaneCount(QImageH handle);
C_EXPORT uchar* QImage_bits(QImageH handle);
C_EXPORT int QImage_bytesPerLine(QImageH handle);
C_EXPORT qint64 QImage_cacheKey(QImageH handle);
C_EXPORT QRgb QImage_color(QImageH handle, int i);
C_EXPORT int QImage_colorCount(QImageH handle);
C_EXPORT void QImage_colorSpace(QImageH handle, QColorSpaceH retval);
C_EXPORT void QImage_colorTable(QImageH handle, PPtrIntArray retval);
C_EXPORT const uchar* QImage_constBits(QImageH handle);
C_EXPORT const uchar* QImage_constScanLine(QImageH handle, int AnonParam1);
C_EXPORT void QImage_convertTo(QImageH handle, QImage::Format f, unsigned int flags);
C_EXPORT void QImage_convertToFormat(QImageH handle, QImageH retval, QImage::Format f, unsigned int flags);
C_EXPORT void QImage_convertedTo(QImageH handle, QImageH retval, QImage::Format f, unsigned int flags);
C_EXPORT void QImage_convertedToColorSpace(QImageH handle, const QColorSpaceH colorSpace, QImageH retval);
C_EXPORT void QImage_copy(QImageH handle, QImageH retval, PRect rect);
C_EXPORT void QImage_copy2(QImageH handle, QImageH retval, int x, int y, int w, int h);
C_EXPORT void QImage_createAlphaMask(QImageH handle, QImageH retval, unsigned int flags);
C_EXPORT void QImage_createHeuristicMask(QImageH handle, QImageH retval, bool clipTight);
C_EXPORT void QImage_createMaskFromColor(QImageH handle, QImageH retval, QRgb color, Qt::MaskMode mode);
C_EXPORT int QImage_depth(QImageH handle);
C_EXPORT void QImage_detach(QImageH handle);
C_EXPORT void QImage_deviceIndependentSize(QImageH handle, QSizeFH retval);
C_EXPORT qreal QImage_devicePixelRatio(QImageH handle);
C_EXPORT int QImage_devType(QImageH handle);
C_EXPORT int QImage_dotsPerMeterX(QImageH handle);
C_EXPORT int QImage_dotsPerMeterY(QImageH handle);
C_EXPORT void QImage_fill(QImageH handle, uint pixel);
C_EXPORT void QImage_fill2(QImageH handle, const QColorH color);
C_EXPORT void QImage_fill3(QImageH handle, Qt::GlobalColor color);
C_EXPORT QImage::Format QImage_format(QImageH handle);
C_EXPORT bool QImage_hasAlphaChannel(QImageH handle);
C_EXPORT int QImage_height(QImageH handle);
C_EXPORT void QImage_invertPixels(QImageH handle, QImage::InvertMode AnonParam1);
C_EXPORT bool QImage_isDetached(QImageH handle);
C_EXPORT bool QImage_isGrayscale(QImageH handle);
C_EXPORT bool QImage_isNull(QImageH handle);
C_EXPORT bool QImage_load(QImageH handle, QIODeviceH device, const char* format);
C_EXPORT bool QImage_load2(QImageH handle, PWideString fileName, const char* format);
C_EXPORT bool QImage_loadFromData(QImageH handle, const uchar* buf, int len, const char* format);
C_EXPORT bool QImage_loadFromData2(QImageH handle, const QByteArrayH data, const char* aformat);
C_EXPORT void QImage_mirror(QImageH handle, QImageH retval, bool horizontally, bool vertically);
C_EXPORT void QImage_mirrored(QImageH handle, QImageH retval, bool horizontally, bool vertically);
C_EXPORT void QImage_offset(QImageH handle, PQtPoint retval);
C_EXPORT QPaintEngineH QImage_paintEngine(QImageH handle);
C_EXPORT QRgb QImage_pixel(QImageH handle, int x, int y);
C_EXPORT QRgb QImage_pixel2(QImageH handle, const QPointH pt);
C_EXPORT void QImage_pixelColor(QImageH handle, const QPointH pt, PQColor retval);
C_EXPORT void QImage_pixelColor2(QImageH handle, int x, int y, PQColor retval);
C_EXPORT void QImage_pixelFormat(QImageH handle, QPixelFormatH retval);
C_EXPORT int QImage_pixelIndex(QImageH handle, int x, int y);
C_EXPORT int QImage_pixelIndex2(QImageH handle, const QPointH pt);
C_EXPORT void QImage_rect(QImageH handle, PRect retval);
C_EXPORT bool QImage_reinterpretAsFormat(QImageH handle, QImage::Format format);
C_EXPORT void QImage_rgbSwap(QImageH handle);
C_EXPORT void QImage_rgbSwapped(QImageH handle, QImageH retval);
C_EXPORT bool QImage_save(QImageH handle, PWideString fileName, const char* format, int quality);
C_EXPORT bool QImage_save2(QImageH handle, QIODeviceH device, const char* format, int quality);
C_EXPORT void QImage_scaled(QImageH handle, QImageH retval, int w, int h, Qt::AspectRatioMode aspectMode, Qt::TransformationMode mode);
C_EXPORT void QImage_scaled2(QImageH handle, QImageH retval, const QSizeH s, Qt::AspectRatioMode aspectMode, Qt::TransformationMode mode);
C_EXPORT void QImage_scaledToHeight(QImageH handle, QImageH retval, int h, Qt::TransformationMode mode);
C_EXPORT void QImage_scaledToWidth(QImageH handle, QImageH retval, int w, Qt::TransformationMode mode);
C_EXPORT uchar* QImage_scanLine(QImageH handle, int AnonParam1);
C_EXPORT void QImage_setAlphaChannel(QImageH handle, const QImageH alphaChannel);
C_EXPORT void QImage_setColor(QImageH handle, int i, QRgb c);
C_EXPORT void QImage_setColorCount(QImageH handle, int AnonParam1);
C_EXPORT void QImage_setColorSpace(QImageH handle, QColorSpaceH colorSpace);
C_EXPORT void QImage_setColorTable(QImageH handle, PPtrIntArray colorTable);
C_EXPORT void QImage_setDevicePixelRatio(QImageH handle, qreal scaleFactor);
C_EXPORT void QImage_setDotsPerMeterX(QImageH handle, int AnonParam1);
C_EXPORT void QImage_setDotsPerMeterY(QImageH handle, int AnonParam1);
C_EXPORT void QImage_setOffset(QImageH handle, const QPointH AnonParam1);
C_EXPORT void QImage_setPixel(QImageH handle, int x, int y, uint index_or_rgb);
C_EXPORT void QImage_setPixel2(QImageH handle, const QPointH pt, uint index_or_rgb);
C_EXPORT void QImage_setPixelColor(QImageH handle, QColorH color, int x, int y);
C_EXPORT void QImage_setPixelColor2(QImageH handle, QColorH color, const QPointH pt);
C_EXPORT void QImage_setPixelColor3(QImageH handle, QColorH color, const PQtPoint pt);
C_EXPORT void QImage_setText(QImageH handle, PWideString key, PWideString value);
C_EXPORT void QImage_size(QImageH handle, PSize retval);
C_EXPORT int QImage_sizeInBytes(QImageH handle);
C_EXPORT void QImage_swap(QImageH handle, QImageH other);
C_EXPORT void QImage_text(QImageH handle, PWideString retval, PWideString key);
C_EXPORT void QImage_textKeys(QImageH handle, QStringListH retval);
C_EXPORT void QImage_transformed(QImageH handle, QImageH retval, const QTransformH matrix, Qt::TransformationMode mode);
C_EXPORT bool QImage_valid(QImageH handle, int x, int y);
C_EXPORT bool QImage_valid2(QImageH handle, const QPointH pt);
C_EXPORT int QImage_width(QImageH handle);

// static stuff
C_EXPORT void QImage_fromData(QImageH retval, const uchar* data, int size, const char* format);
C_EXPORT void QImage_fromData2(QImageH retval, const QByteArrayH data, const char* format);
C_EXPORT void QImage_trueMatrix(QTransformH retval, const QTransformH AnonParam1, int w, int h);
C_EXPORT QImage::Format QImage_toImageFormat(QPixelFormatH format);
C_EXPORT void QImage_toPixelFormat(QImage::Format format, QPixelFormatH retval);

// QColorTransform
C_EXPORT QColorTransformH QColorTransform_Create();
C_EXPORT void QColorTransform_Destroy(QColorTransformH handle);
C_EXPORT QRgb QColorTransform_map(QColorTransformH handle, QRgb rgb);
C_EXPORT QRgba64 QColorTransform_map2(QColorTransformH handle, QRgba64 rgba64);
C_EXPORT void QColorTransform_map3(QColorTransformH handle, const QColorH color, PQColor retval);

// TODO QPixelFormat add members
C_EXPORT QPixelFormatH QPixelFormat_Create();
C_EXPORT void QPixelFormat_Destroy(QPixelFormatH handle);

#endif
