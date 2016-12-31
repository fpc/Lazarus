//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QIMAGEREADER_C_H
#define QIMAGEREADER_C_H

#include <QtGui>
#include "pascalbind.h"

C_EXPORT QImageReaderH QImageReader_Create();
C_EXPORT void QImageReader_Destroy(QImageReaderH handle);
C_EXPORT QImageReaderH QImageReader_Create2(QIODeviceH device, const QByteArrayH format);
C_EXPORT QImageReaderH QImageReader_Create3(PWideString fileName, const QByteArrayH format);
C_EXPORT void QImageReader_setFormat(QImageReaderH handle, const QByteArrayH format);
C_EXPORT void QImageReader_format(QImageReaderH handle, QByteArrayH retval);
C_EXPORT void QImageReader_setAutoDetectImageFormat(QImageReaderH handle, bool enabled);
C_EXPORT bool QImageReader_autoDetectImageFormat(QImageReaderH handle);
C_EXPORT void QImageReader_setDecideFormatFromContent(QImageReaderH handle, bool ignored);
C_EXPORT bool QImageReader_decideFormatFromContent(QImageReaderH handle);
C_EXPORT void QImageReader_setDevice(QImageReaderH handle, QIODeviceH device);
C_EXPORT QIODeviceH QImageReader_device(QImageReaderH handle);
C_EXPORT void QImageReader_setFileName(QImageReaderH handle, PWideString fileName);
C_EXPORT void QImageReader_fileName(QImageReaderH handle, PWideString retval);
C_EXPORT void QImageReader_size(QImageReaderH handle, PSize retval);
C_EXPORT QImage::Format QImageReader_imageFormat(QImageReaderH handle);
C_EXPORT void QImageReader_textKeys(QImageReaderH handle, QStringListH retval);
C_EXPORT void QImageReader_text(QImageReaderH handle, PWideString retval, PWideString key);
C_EXPORT void QImageReader_setClipRect(QImageReaderH handle, PRect rect);
C_EXPORT void QImageReader_clipRect(QImageReaderH handle, PRect retval);
C_EXPORT void QImageReader_setScaledSize(QImageReaderH handle, const QSizeH size);
C_EXPORT void QImageReader_scaledSize(QImageReaderH handle, PSize retval);
C_EXPORT void QImageReader_setQuality(QImageReaderH handle, int quality);
C_EXPORT int QImageReader_quality(QImageReaderH handle);
C_EXPORT void QImageReader_setScaledClipRect(QImageReaderH handle, PRect rect);
C_EXPORT void QImageReader_scaledClipRect(QImageReaderH handle, PRect retval);
C_EXPORT void QImageReader_setBackgroundColor(QImageReaderH handle, const QColorH color);
C_EXPORT void QImageReader_backgroundColor(QImageReaderH handle, PQColor retval);
C_EXPORT bool QImageReader_supportsAnimation(QImageReaderH handle);
C_EXPORT bool QImageReader_canRead(QImageReaderH handle);
C_EXPORT void QImageReader_read(QImageReaderH handle, QImageH retval);
C_EXPORT bool QImageReader_jumpToNextImage(QImageReaderH handle);
C_EXPORT bool QImageReader_jumpToImage(QImageReaderH handle, int imageNumber);
C_EXPORT int QImageReader_loopCount(QImageReaderH handle);
C_EXPORT int QImageReader_imageCount(QImageReaderH handle);
C_EXPORT int QImageReader_nextImageDelay(QImageReaderH handle);
C_EXPORT int QImageReader_currentImageNumber(QImageReaderH handle);
C_EXPORT void QImageReader_currentImageRect(QImageReaderH handle, PRect retval);
C_EXPORT QImageReader::ImageReaderError QImageReader_error(QImageReaderH handle);
C_EXPORT void QImageReader_errorString(QImageReaderH handle, PWideString retval);
C_EXPORT bool QImageReader_supportsOption(QImageReaderH handle, QImageIOHandler::ImageOption option);
C_EXPORT void QImageReader_imageFormat2(QByteArrayH retval, PWideString fileName);
C_EXPORT void QImageReader_imageFormat3(QByteArrayH retval, QIODeviceH device);

#endif
