//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qimagereader_c.h"

QImageReaderH QImageReader_Create()
{
	return (QImageReaderH) new QImageReader();
}

void QImageReader_Destroy(QImageReaderH handle)
{
	delete (QImageReader *)handle;
}

QImageReaderH QImageReader_Create2(QIODeviceH device, const QByteArrayH format)
{
	return (QImageReaderH) new QImageReader((QIODevice*)device, *(const QByteArray*)format);
}

QImageReaderH QImageReader_Create3(PWideString fileName, const QByteArrayH format)
{
	QString t_fileName;
	copyPWideStringToQString(fileName, t_fileName);
	return (QImageReaderH) new QImageReader(t_fileName, *(const QByteArray*)format);
}

void QImageReader_setFormat(QImageReaderH handle, const QByteArrayH format)
{
	((QImageReader *)handle)->setFormat(*(const QByteArray*)format);
}

void QImageReader_format(QImageReaderH handle, QByteArrayH retval)
{
	*(QByteArray *)retval = ((QImageReader *)handle)->format();
}

void QImageReader_setAutoDetectImageFormat(QImageReaderH handle, bool enabled)
{
	((QImageReader *)handle)->setAutoDetectImageFormat(enabled);
}

bool QImageReader_autoDetectImageFormat(QImageReaderH handle)
{
	return (bool) ((QImageReader *)handle)->autoDetectImageFormat();
}

void QImageReader_setDecideFormatFromContent(QImageReaderH handle, bool ignored)
{
	((QImageReader *)handle)->setDecideFormatFromContent(ignored);
}

bool QImageReader_decideFormatFromContent(QImageReaderH handle)
{
	return (bool) ((QImageReader *)handle)->decideFormatFromContent();
}

void QImageReader_setDevice(QImageReaderH handle, QIODeviceH device)
{
	((QImageReader *)handle)->setDevice((QIODevice*)device);
}

QIODeviceH QImageReader_device(QImageReaderH handle)
{
	return (QIODeviceH) ((QImageReader *)handle)->device();
}

void QImageReader_setFileName(QImageReaderH handle, PWideString fileName)
{
	QString t_fileName;
	copyPWideStringToQString(fileName, t_fileName);
	((QImageReader *)handle)->setFileName(t_fileName);
}

void QImageReader_fileName(QImageReaderH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QImageReader *)handle)->fileName();
	copyQStringToPWideString(t_retval, retval);
}

void QImageReader_size(QImageReaderH handle, PSize retval)
{
	*(QSize *)retval = ((QImageReader *)handle)->size();
}

QImage::Format QImageReader_imageFormat(QImageReaderH handle)
{
	return (QImage::Format) ((QImageReader *)handle)->imageFormat();
}

void QImageReader_textKeys(QImageReaderH handle, QStringListH retval)
{
	*(QStringList *)retval = ((QImageReader *)handle)->textKeys();
}

void QImageReader_text(QImageReaderH handle, PWideString retval, PWideString key)
{
	QString t_retval;
	QString t_key;
	copyPWideStringToQString(key, t_key);
	t_retval = ((QImageReader *)handle)->text(t_key);
	copyQStringToPWideString(t_retval, retval);
}

void QImageReader_setClipRect(QImageReaderH handle, PRect rect)
{
	QRect t_rect;
	copyPRectToQRect(rect, t_rect);
	((QImageReader *)handle)->setClipRect(t_rect);
}

void QImageReader_clipRect(QImageReaderH handle, PRect retval)
{
	QRect t_retval;
	t_retval = ((QImageReader *)handle)->clipRect();
	copyQRectToPRect(t_retval, retval);
}

void QImageReader_setScaledSize(QImageReaderH handle, const QSizeH size)
{
	((QImageReader *)handle)->setScaledSize(*(const QSize*)size);
}

void QImageReader_scaledSize(QImageReaderH handle, PSize retval)
{
	*(QSize *)retval = ((QImageReader *)handle)->scaledSize();
}

void QImageReader_setQuality(QImageReaderH handle, int quality)
{
	((QImageReader *)handle)->setQuality(quality);
}

int QImageReader_quality(QImageReaderH handle)
{
	return (int) ((QImageReader *)handle)->quality();
}

void QImageReader_setScaledClipRect(QImageReaderH handle, PRect rect)
{
	QRect t_rect;
	copyPRectToQRect(rect, t_rect);
	((QImageReader *)handle)->setScaledClipRect(t_rect);
}

void QImageReader_scaledClipRect(QImageReaderH handle, PRect retval)
{
	QRect t_retval;
	t_retval = ((QImageReader *)handle)->scaledClipRect();
	copyQRectToPRect(t_retval, retval);
}

void QImageReader_setBackgroundColor(QImageReaderH handle, const QColorH color)
{
	((QImageReader *)handle)->setBackgroundColor(*(const QColor*)color);
}

void QImageReader_backgroundColor(QImageReaderH handle, PQColor retval)
{
	*(QColor *)retval = ((QImageReader *)handle)->backgroundColor();
}

bool QImageReader_supportsAnimation(QImageReaderH handle)
{
	return (bool) ((QImageReader *)handle)->supportsAnimation();
}

bool QImageReader_canRead(QImageReaderH handle)
{
	return (bool) ((QImageReader *)handle)->canRead();
}

void QImageReader_read(QImageReaderH handle, QImageH retval)
{
	*(QImage *)retval = ((QImageReader *)handle)->read();
}

bool QImageReader_jumpToNextImage(QImageReaderH handle)
{
	return (bool) ((QImageReader *)handle)->jumpToNextImage();
}

bool QImageReader_jumpToImage(QImageReaderH handle, int imageNumber)
{
	return (bool) ((QImageReader *)handle)->jumpToImage(imageNumber);
}

int QImageReader_loopCount(QImageReaderH handle)
{
	return (int) ((QImageReader *)handle)->loopCount();
}

int QImageReader_imageCount(QImageReaderH handle)
{
	return (int) ((QImageReader *)handle)->imageCount();
}

int QImageReader_nextImageDelay(QImageReaderH handle)
{
	return (int) ((QImageReader *)handle)->nextImageDelay();
}

int QImageReader_currentImageNumber(QImageReaderH handle)
{
	return (int) ((QImageReader *)handle)->currentImageNumber();
}

void QImageReader_currentImageRect(QImageReaderH handle, PRect retval)
{
	QRect t_retval;
	t_retval = ((QImageReader *)handle)->currentImageRect();
	copyQRectToPRect(t_retval, retval);
}

QImageReader::ImageReaderError QImageReader_error(QImageReaderH handle)
{
	return (QImageReader::ImageReaderError) ((QImageReader *)handle)->error();
}

void QImageReader_errorString(QImageReaderH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QImageReader *)handle)->errorString();
	copyQStringToPWideString(t_retval, retval);
}

bool QImageReader_supportsOption(QImageReaderH handle, QImageIOHandler::ImageOption option)
{
	return (bool) ((QImageReader *)handle)->supportsOption(option);
}

void QImageReader_imageFormat2(QByteArrayH retval, PWideString fileName)
{
	QString t_fileName;
	copyPWideStringToQString(fileName, t_fileName);
	*(QByteArray *)retval = QImageReader::imageFormat(t_fileName);
}

void QImageReader_imageFormat3(QByteArrayH retval, QIODeviceH device)
{
	*(QByteArray *)retval = QImageReader::imageFormat((QIODevice*)device);
}

