//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qbitmap_c.h"

QBitmapH QBitmap_Create()
{
	return (QBitmapH) new QBitmap();
}

void QBitmap_Destroy(QBitmapH handle)
{
	delete (QBitmap *)handle;
}

QBitmapH QBitmap_Create2(const QPixmapH AnonParam1)
{
	return (QBitmapH) new QBitmap(*(const QPixmap*)AnonParam1);
}

QBitmapH QBitmap_Create3(int w, int h)
{
	return (QBitmapH) new QBitmap(w, h);
}

QBitmapH QBitmap_Create4(const QSizeH AnonParam1)
{
	return (QBitmapH) new QBitmap(*(const QSize*)AnonParam1);
}

QBitmapH QBitmap_Create5(PWideString fileName, const char* format)
{
	QString t_fileName;
	copyPWideStringToQString(fileName, t_fileName);
	return (QBitmapH) new QBitmap(t_fileName, format);
}

void QBitmap_swap(QBitmapH handle, QBitmapH other)
{
	((QBitmap *)handle)->swap(*(QBitmap*)other);
}

void QBitmap_clear(QBitmapH handle)
{
	((QBitmap *)handle)->clear();
}

void QBitmap_fromData(QBitmapH retval, const QSizeH size, const uchar* bits, QImage::Format monoFormat)
{
	*(QBitmap *)retval = QBitmap::fromData(*(const QSize*)size, bits, monoFormat);
}

void QBitmap_fromImage(QBitmapH retval, const QImageH image, unsigned int flags)
{
	*(QBitmap *)retval = QBitmap::fromImage(*(const QImage*)image, (Qt::ImageConversionFlags)flags);
}

void QBitmap_fromPixmap(QBitmapH retval, const QPixmapH pixmap)
{
	*(QBitmap *)retval = QBitmap::fromPixmap(*(const QPixmap*)pixmap);
}

void QBitmap_transformed(QBitmapH handle, QBitmapH retval, const QTransformH matrix)
{
	*(QBitmap *)retval = ((QBitmap *)handle)->transformed(*(const QTransform*)matrix);
}

