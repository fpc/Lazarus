//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qimageiohandler_c.h"

void QImageIOHandler_setDevice(QImageIOHandlerH handle, QIODeviceH device)
{
	((QImageIOHandler *)handle)->setDevice((QIODevice*)device);
}

QIODeviceH QImageIOHandler_device(QImageIOHandlerH handle)
{
	return (QIODeviceH) ((QImageIOHandler *)handle)->device();
}

void QImageIOHandler_setFormat(QImageIOHandlerH handle, const QByteArrayH format)
{
	((QImageIOHandler *)handle)->setFormat(*(const QByteArray*)format);
}

void QImageIOHandler_format(QImageIOHandlerH handle, QByteArrayH retval)
{
	*(QByteArray *)retval = ((QImageIOHandler *)handle)->format();
}

bool QImageIOHandler_canRead(QImageIOHandlerH handle)
{
	return (bool) ((QImageIOHandler *)handle)->canRead();
}

bool QImageIOHandler_read(QImageIOHandlerH handle, QImageH image)
{
	return (bool) ((QImageIOHandler *)handle)->read((QImage*)image);
}

bool QImageIOHandler_write(QImageIOHandlerH handle, const QImageH image)
{
	return (bool) ((QImageIOHandler *)handle)->write(*(const QImage*)image);
}

void QImageIOHandler_option(QImageIOHandlerH handle, QVariantH retval, QImageIOHandler::ImageOption option)
{
	*(QVariant *)retval = ((QImageIOHandler *)handle)->option(option);
}

void QImageIOHandler_setOption(QImageIOHandlerH handle, QImageIOHandler::ImageOption option, const QVariantH value)
{
	((QImageIOHandler *)handle)->setOption(option, *(const QVariant*)value);
}

bool QImageIOHandler_supportsOption(QImageIOHandlerH handle, QImageIOHandler::ImageOption option)
{
	return (bool) ((QImageIOHandler *)handle)->supportsOption(option);
}

bool QImageIOHandler_jumpToNextImage(QImageIOHandlerH handle)
{
	return (bool) ((QImageIOHandler *)handle)->jumpToNextImage();
}

bool QImageIOHandler_jumpToImage(QImageIOHandlerH handle, int imageNumber)
{
	return (bool) ((QImageIOHandler *)handle)->jumpToImage(imageNumber);
}

int QImageIOHandler_loopCount(QImageIOHandlerH handle)
{
	return (int) ((QImageIOHandler *)handle)->loopCount();
}

int QImageIOHandler_imageCount(QImageIOHandlerH handle)
{
	return (int) ((QImageIOHandler *)handle)->imageCount();
}

int QImageIOHandler_nextImageDelay(QImageIOHandlerH handle)
{
	return (int) ((QImageIOHandler *)handle)->nextImageDelay();
}

int QImageIOHandler_currentImageNumber(QImageIOHandlerH handle)
{
	return (int) ((QImageIOHandler *)handle)->currentImageNumber();
}

void QImageIOHandler_currentImageRect(QImageIOHandlerH handle, PRect retval)
{
	QRect t_retval;
	t_retval = ((QImageIOHandler *)handle)->currentImageRect();
	copyQRectToPRect(t_retval, retval);
}

