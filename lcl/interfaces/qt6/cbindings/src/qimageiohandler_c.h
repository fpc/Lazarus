//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QIMAGEIOHANDLER_C_H
#define QIMAGEIOHANDLER_C_H

#include <QtGui>
#include "pascalbind.h"

C_EXPORT void QImageIOHandler_setDevice(QImageIOHandlerH handle, QIODeviceH device);
C_EXPORT QIODeviceH QImageIOHandler_device(QImageIOHandlerH handle);
C_EXPORT void QImageIOHandler_setFormat(QImageIOHandlerH handle, const QByteArrayH format);
C_EXPORT void QImageIOHandler_format(QImageIOHandlerH handle, QByteArrayH retval);
C_EXPORT bool QImageIOHandler_canRead(QImageIOHandlerH handle);
C_EXPORT bool QImageIOHandler_read(QImageIOHandlerH handle, QImageH image);
C_EXPORT bool QImageIOHandler_write(QImageIOHandlerH handle, const QImageH image);
C_EXPORT void QImageIOHandler_option(QImageIOHandlerH handle, QVariantH retval, QImageIOHandler::ImageOption option);
C_EXPORT void QImageIOHandler_setOption(QImageIOHandlerH handle, QImageIOHandler::ImageOption option, const QVariantH value);
C_EXPORT bool QImageIOHandler_supportsOption(QImageIOHandlerH handle, QImageIOHandler::ImageOption option);
C_EXPORT bool QImageIOHandler_jumpToNextImage(QImageIOHandlerH handle);
C_EXPORT bool QImageIOHandler_jumpToImage(QImageIOHandlerH handle, int imageNumber);
C_EXPORT int QImageIOHandler_loopCount(QImageIOHandlerH handle);
C_EXPORT int QImageIOHandler_imageCount(QImageIOHandlerH handle);
C_EXPORT int QImageIOHandler_nextImageDelay(QImageIOHandlerH handle);
C_EXPORT int QImageIOHandler_currentImageNumber(QImageIOHandlerH handle);
C_EXPORT void QImageIOHandler_currentImageRect(QImageIOHandlerH handle, PRect retval);

#endif
