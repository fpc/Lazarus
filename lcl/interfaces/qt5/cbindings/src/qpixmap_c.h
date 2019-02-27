//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QPIXMAP_C_H
#define QPIXMAP_C_H

#include <QtGui>
#include "pascalbind.h"

C_EXPORT QPixmapH QPixmap_Create();
C_EXPORT void QPixmap_Destroy(QPixmapH handle);
C_EXPORT QPixmapH QPixmap_Create2(int w, int h);
C_EXPORT QPixmapH QPixmap_Create3(const QSizeH AnonParam1);
C_EXPORT QPixmapH QPixmap_Create4(PWideString fileName, const char* format, unsigned int flags);
C_EXPORT QPixmapH QPixmap_Create5(const char* const xpm);
C_EXPORT QPixmapH QPixmap_Create6(const QPixmapH AnonParam1);
C_EXPORT void QPixmap_swap(QPixmapH handle, QPixmapH other);
C_EXPORT bool QPixmap_isNull(QPixmapH handle);
C_EXPORT int QPixmap_devType(QPixmapH handle);
C_EXPORT int QPixmap_width(QPixmapH handle);
C_EXPORT int QPixmap_height(QPixmapH handle);
C_EXPORT void QPixmap_size(QPixmapH handle, PSize retval);
C_EXPORT void QPixmap_rect(QPixmapH handle, PRect retval);
C_EXPORT int QPixmap_depth(QPixmapH handle);
C_EXPORT int QPixmap_defaultDepth();
C_EXPORT void QPixmap_fill(QPixmapH handle, const QColorH fillColor);
C_EXPORT void QPixmap_fill2(QPixmapH handle, const QPaintDeviceH device, const QPointH ofs);
C_EXPORT void QPixmap_fill3(QPixmapH handle, const QPaintDeviceH device, int xofs, int yofs);
C_EXPORT void QPixmap_mask(QPixmapH handle, QBitmapH retval);
C_EXPORT void QPixmap_setMask(QPixmapH handle, const QBitmapH AnonParam1);
C_EXPORT qreal QPixmap_devicePixelRatio(QPixmapH handle);
C_EXPORT void QPixmap_setDevicePixelRatio(QPixmapH handle, qreal scaleFactor);
C_EXPORT bool QPixmap_hasAlpha(QPixmapH handle);
C_EXPORT bool QPixmap_hasAlphaChannel(QPixmapH handle);
C_EXPORT void QPixmap_createHeuristicMask(QPixmapH handle, QBitmapH retval, bool clipTight);
C_EXPORT void QPixmap_createMaskFromColor(QPixmapH handle, QBitmapH retval, const QColorH maskColor, Qt::MaskMode mode);
C_EXPORT void QPixmap_grabWindow(QPixmapH retval, WId AnonParam1, int x, int y, int w, int h);
C_EXPORT void QPixmap_grabWidget(QPixmapH retval, QObjectH widget, PRect rect);
C_EXPORT void QPixmap_grabWidget2(QPixmapH retval, QObjectH widget, int x, int y, int w, int h);
C_EXPORT void QPixmap_scaled(QPixmapH handle, QPixmapH retval, int w, int h, Qt::AspectRatioMode aspectMode, Qt::TransformationMode mode);
C_EXPORT void QPixmap_scaled2(QPixmapH handle, QPixmapH retval, const QSizeH s, Qt::AspectRatioMode aspectMode, Qt::TransformationMode mode);
C_EXPORT void QPixmap_scaledToWidth(QPixmapH handle, QPixmapH retval, int w, Qt::TransformationMode mode);
C_EXPORT void QPixmap_scaledToHeight(QPixmapH handle, QPixmapH retval, int h, Qt::TransformationMode mode);
C_EXPORT void QPixmap_transformed(QPixmapH handle, QPixmapH retval, const QMatrixH AnonParam1, Qt::TransformationMode mode);
C_EXPORT void QPixmap_trueMatrix(QMatrixH retval, const QMatrixH m, int w, int h);
C_EXPORT void QPixmap_transformed2(QPixmapH handle, QPixmapH retval, const QTransformH AnonParam1, Qt::TransformationMode mode);
C_EXPORT void QPixmap_trueMatrix2(QTransformH retval, const QTransformH m, int w, int h);
C_EXPORT void QPixmap_toImage(QPixmapH handle, QImageH retval);
C_EXPORT void QPixmap_fromImage(QPixmapH retval, const QImageH image, unsigned int flags);
C_EXPORT void QPixmap_fromImageReader(QPixmapH retval, QImageReaderH imageReader, unsigned int flags);
C_EXPORT bool QPixmap_load(QPixmapH handle, PWideString fileName, const char* format, unsigned int flags);
C_EXPORT bool QPixmap_loadFromData(QPixmapH handle, const uchar* buf, uint len, const char* format, unsigned int flags);
C_EXPORT bool QPixmap_loadFromData2(QPixmapH handle, const QByteArrayH data, const char* format, unsigned int flags);
C_EXPORT bool QPixmap_save(QPixmapH handle, PWideString fileName, const char* format, int quality);
C_EXPORT bool QPixmap_save2(QPixmapH handle, QIODeviceH device, const char* format, int quality);
C_EXPORT bool QPixmap_convertFromImage(QPixmapH handle, const QImageH img, unsigned int flags);
C_EXPORT void QPixmap_copy(QPixmapH handle, QPixmapH retval, int x, int y, int width, int height);
C_EXPORT void QPixmap_copy2(QPixmapH handle, QPixmapH retval, PRect rect);
C_EXPORT void QPixmap_scroll(QPixmapH handle, int dx, int dy, int x, int y, int width, int height, QRegionH exposed);
C_EXPORT void QPixmap_scroll2(QPixmapH handle, int dx, int dy, PRect rect, QRegionH exposed);
C_EXPORT qint64 QPixmap_cacheKey(QPixmapH handle);
C_EXPORT bool QPixmap_isDetached(QPixmapH handle);
C_EXPORT void QPixmap_detach(QPixmapH handle);
C_EXPORT bool QPixmap_isQBitmap(QPixmapH handle);
C_EXPORT QPaintEngineH QPixmap_paintEngine(QPixmapH handle);

#endif
