//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QIODEVICE_C_H
#define QIODEVICE_C_H

#include <QtCore>
#include "pascalbind.h"

C_EXPORT unsigned int QIODevice_openMode(QIODeviceH handle);
C_EXPORT void QIODevice_setTextModeEnabled(QIODeviceH handle, bool enabled);
C_EXPORT bool QIODevice_isTextModeEnabled(QIODeviceH handle);
C_EXPORT bool QIODevice_isOpen(QIODeviceH handle);
C_EXPORT bool QIODevice_isReadable(QIODeviceH handle);
C_EXPORT bool QIODevice_isWritable(QIODeviceH handle);
C_EXPORT bool QIODevice_isSequential(QIODeviceH handle);
C_EXPORT bool QIODevice_open(QIODeviceH handle, unsigned int mode);
C_EXPORT void QIODevice_close(QIODeviceH handle);
C_EXPORT qint64 QIODevice_pos(QIODeviceH handle);
C_EXPORT qint64 QIODevice_size(QIODeviceH handle);
C_EXPORT bool QIODevice_seek(QIODeviceH handle, qint64 pos);
C_EXPORT bool QIODevice_atEnd(QIODeviceH handle);
C_EXPORT bool QIODevice_reset(QIODeviceH handle);
C_EXPORT qint64 QIODevice_bytesAvailable(QIODeviceH handle);
C_EXPORT qint64 QIODevice_bytesToWrite(QIODeviceH handle);
C_EXPORT qint64 QIODevice_read(QIODeviceH handle, char* data, qint64 maxlen);
C_EXPORT void QIODevice_read2(QIODeviceH handle, QByteArrayH retval, qint64 maxlen);
C_EXPORT void QIODevice_readAll(QIODeviceH handle, QByteArrayH retval);
C_EXPORT qint64 QIODevice_readLine(QIODeviceH handle, char* data, qint64 maxlen);
C_EXPORT void QIODevice_readLine2(QIODeviceH handle, QByteArrayH retval, qint64 maxlen);
C_EXPORT bool QIODevice_canReadLine(QIODeviceH handle);
C_EXPORT qint64 QIODevice_write(QIODeviceH handle, const char* data, qint64 len);
C_EXPORT qint64 QIODevice_write2(QIODeviceH handle, const char* data);
C_EXPORT qint64 QIODevice_write3(QIODeviceH handle, const QByteArrayH data);
C_EXPORT qint64 QIODevice_peek(QIODeviceH handle, char* data, qint64 maxlen);
C_EXPORT void QIODevice_peek2(QIODeviceH handle, QByteArrayH retval, qint64 maxlen);
C_EXPORT bool QIODevice_waitForReadyRead(QIODeviceH handle, int msecs);
C_EXPORT bool QIODevice_waitForBytesWritten(QIODeviceH handle, int msecs);
C_EXPORT void QIODevice_ungetChar(QIODeviceH handle, char c);
C_EXPORT bool QIODevice_putChar(QIODeviceH handle, char c);
C_EXPORT bool QIODevice_getChar(QIODeviceH handle, char* c);
C_EXPORT void QIODevice_errorString(QIODeviceH handle, PWideString retval);

#endif
