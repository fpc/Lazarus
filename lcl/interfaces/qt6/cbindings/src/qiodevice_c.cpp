//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qiodevice_c.h"

unsigned int QIODevice_openMode(QIODeviceH handle)
{
	return (unsigned int) ((QIODevice *)handle)->openMode();
}

void QIODevice_setTextModeEnabled(QIODeviceH handle, bool enabled)
{
	((QIODevice *)handle)->setTextModeEnabled(enabled);
}

bool QIODevice_isTextModeEnabled(QIODeviceH handle)
{
	return (bool) ((QIODevice *)handle)->isTextModeEnabled();
}

bool QIODevice_isOpen(QIODeviceH handle)
{
	return (bool) ((QIODevice *)handle)->isOpen();
}

bool QIODevice_isReadable(QIODeviceH handle)
{
	return (bool) ((QIODevice *)handle)->isReadable();
}

bool QIODevice_isWritable(QIODeviceH handle)
{
	return (bool) ((QIODevice *)handle)->isWritable();
}

bool QIODevice_isSequential(QIODeviceH handle)
{
	return (bool) ((QIODevice *)handle)->isSequential();
}

bool QIODevice_open(QIODeviceH handle, unsigned int mode)
{
	return (bool) ((QIODevice *)handle)->open((QIODevice::OpenMode)mode);
}

void QIODevice_close(QIODeviceH handle)
{
	((QIODevice *)handle)->close();
}

qint64 QIODevice_pos(QIODeviceH handle)
{
	return (qint64) ((QIODevice *)handle)->pos();
}

qint64 QIODevice_size(QIODeviceH handle)
{
	return (qint64) ((QIODevice *)handle)->size();
}

bool QIODevice_seek(QIODeviceH handle, qint64 pos)
{
	return (bool) ((QIODevice *)handle)->seek(pos);
}

bool QIODevice_atEnd(QIODeviceH handle)
{
	return (bool) ((QIODevice *)handle)->atEnd();
}

bool QIODevice_reset(QIODeviceH handle)
{
	return (bool) ((QIODevice *)handle)->reset();
}

qint64 QIODevice_bytesAvailable(QIODeviceH handle)
{
	return (qint64) ((QIODevice *)handle)->bytesAvailable();
}

qint64 QIODevice_bytesToWrite(QIODeviceH handle)
{
	return (qint64) ((QIODevice *)handle)->bytesToWrite();
}

qint64 QIODevice_read(QIODeviceH handle, char* data, qint64 maxlen)
{
	return (qint64) ((QIODevice *)handle)->read(data, maxlen);
}

void QIODevice_read2(QIODeviceH handle, QByteArrayH retval, qint64 maxlen)
{
	*(QByteArray *)retval = ((QIODevice *)handle)->read(maxlen);
}

void QIODevice_readAll(QIODeviceH handle, QByteArrayH retval)
{
	*(QByteArray *)retval = ((QIODevice *)handle)->readAll();
}

qint64 QIODevice_readLine(QIODeviceH handle, char* data, qint64 maxlen)
{
	return (qint64) ((QIODevice *)handle)->readLine(data, maxlen);
}

void QIODevice_readLine2(QIODeviceH handle, QByteArrayH retval, qint64 maxlen)
{
	*(QByteArray *)retval = ((QIODevice *)handle)->readLine(maxlen);
}

bool QIODevice_canReadLine(QIODeviceH handle)
{
	return (bool) ((QIODevice *)handle)->canReadLine();
}

qint64 QIODevice_write(QIODeviceH handle, const char* data, qint64 len)
{
	return (qint64) ((QIODevice *)handle)->write(data, len);
}

qint64 QIODevice_write2(QIODeviceH handle, const char* data)
{
	return (qint64) ((QIODevice *)handle)->write(data);
}

qint64 QIODevice_write3(QIODeviceH handle, const QByteArrayH data)
{
	return (qint64) ((QIODevice *)handle)->write(*(const QByteArray*)data);
}

qint64 QIODevice_peek(QIODeviceH handle, char* data, qint64 maxlen)
{
	return (qint64) ((QIODevice *)handle)->peek(data, maxlen);
}

void QIODevice_peek2(QIODeviceH handle, QByteArrayH retval, qint64 maxlen)
{
	*(QByteArray *)retval = ((QIODevice *)handle)->peek(maxlen);
}

bool QIODevice_waitForReadyRead(QIODeviceH handle, int msecs)
{
	return (bool) ((QIODevice *)handle)->waitForReadyRead(msecs);
}

bool QIODevice_waitForBytesWritten(QIODeviceH handle, int msecs)
{
	return (bool) ((QIODevice *)handle)->waitForBytesWritten(msecs);
}

void QIODevice_ungetChar(QIODeviceH handle, char c)
{
	((QIODevice *)handle)->ungetChar(c);
}

bool QIODevice_putChar(QIODeviceH handle, char c)
{
	return (bool) ((QIODevice *)handle)->putChar(c);
}

bool QIODevice_getChar(QIODeviceH handle, char* c)
{
	return (bool) ((QIODevice *)handle)->getChar(c);
}

void QIODevice_errorString(QIODeviceH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QIODevice *)handle)->errorString();
	copyQStringToPWideString(t_retval, retval);
}

