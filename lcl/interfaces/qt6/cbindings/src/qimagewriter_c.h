//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QIMAGEWRITER_C_H
#define QIMAGEWRITER_C_H

#include <QtGui>
#include "pascalbind.h"

C_EXPORT QImageWriterH QImageWriter_Create();
C_EXPORT void QImageWriter_Destroy(QImageWriterH handle);
C_EXPORT QImageWriterH QImageWriter_Create2(QIODeviceH device, const QByteArrayH format);
C_EXPORT QImageWriterH QImageWriter_Create3(PWideString fileName, const QByteArrayH format);
C_EXPORT void QImageWriter_setFormat(QImageWriterH handle, const QByteArrayH format);
C_EXPORT void QImageWriter_format(QImageWriterH handle, QByteArrayH retval);
C_EXPORT void QImageWriter_setDevice(QImageWriterH handle, QIODeviceH device);
C_EXPORT QIODeviceH QImageWriter_device(QImageWriterH handle);
C_EXPORT void QImageWriter_setFileName(QImageWriterH handle, PWideString fileName);
C_EXPORT void QImageWriter_fileName(QImageWriterH handle, PWideString retval);
C_EXPORT void QImageWriter_setQuality(QImageWriterH handle, int quality);
C_EXPORT int QImageWriter_quality(QImageWriterH handle);
C_EXPORT void QImageWriter_setCompression(QImageWriterH handle, int compression);
C_EXPORT int QImageWriter_compression(QImageWriterH handle);

C_EXPORT void QImageWriter_setText(QImageWriterH handle, PWideString key, PWideString text);
C_EXPORT bool QImageWriter_canWrite(QImageWriterH handle);
C_EXPORT bool QImageWriter_optimizedWrite(QImageWriterH handle);
C_EXPORT void QImageWriter_setOptimizedWrite(QImageWriterH handle, bool ok);
C_EXPORT bool QImageWriter_progressiveScanWrite(QImageWriterH handle);
C_EXPORT void QImageWriter_setProgressiveScanWrite(QImageWriterH handle, bool ok);
C_EXPORT bool QImageWriter_write(QImageWriterH handle, const QImageH image);
C_EXPORT QImageWriter::ImageWriterError QImageWriter_error(QImageWriterH handle);
C_EXPORT void QImageWriter_errorString(QImageWriterH handle, PWideString retval);
C_EXPORT bool QImageWriter_supportsOption(QImageWriterH handle, QImageIOHandler::ImageOption option);

C_EXPORT void QImageWriter_subType(QImageWriterH handle, QByteArrayH retval);
C_EXPORT void QImageWriter_setSubType(QImageWriterH handle, const QByteArrayH subtype);

#endif
