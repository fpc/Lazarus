//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QBITMAP_C_H
#define QBITMAP_C_H

#include <QtGui>
#include "pascalbind.h"

C_EXPORT QBitmapH QBitmap_Create();
C_EXPORT void QBitmap_Destroy(QBitmapH handle);
C_EXPORT QBitmapH QBitmap_Create2(const QPixmapH AnonParam1);
C_EXPORT QBitmapH QBitmap_Create3(int w, int h);
C_EXPORT QBitmapH QBitmap_Create4(const QSizeH AnonParam1);
C_EXPORT QBitmapH QBitmap_Create5(PWideString fileName, const char* format);
C_EXPORT void QBitmap_swap(QBitmapH handle, QBitmapH other);
C_EXPORT void QBitmap_clear(QBitmapH handle);
C_EXPORT void QBitmap_fromData(QBitmapH retval, const QSizeH size, const uchar* bits, QImage::Format monoFormat);
C_EXPORT void QBitmap_fromImage(QBitmapH retval, const QImageH image, unsigned int flags);
C_EXPORT void QBitmap_fromPixmap(QBitmapH retval, const QPixmapH pixmap);
C_EXPORT void QBitmap_transformed(QBitmapH handle, QBitmapH retval, const QTransformH matrix);

#endif
