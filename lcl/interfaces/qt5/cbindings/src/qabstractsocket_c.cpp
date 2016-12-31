//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qabstractsocket_c.h"

QAbstractSocketH QAbstractSocket_Create(QAbstractSocket::SocketType socketType, QObjectH parent)
{
	return (QAbstractSocketH) new QAbstractSocket(socketType, (QObject*)parent);
}

void QAbstractSocket_Destroy(QAbstractSocketH handle)
{
	delete (QAbstractSocket *)handle;
}

void QAbstractSocket_resume(QAbstractSocketH handle)
{
	((QAbstractSocket *)handle)->resume();
}

unsigned int QAbstractSocket_pauseMode(QAbstractSocketH handle)
{
	return (unsigned int) ((QAbstractSocket *)handle)->pauseMode();
}

void QAbstractSocket_setPauseMode(QAbstractSocketH handle, unsigned int pauseMode)
{
	((QAbstractSocket *)handle)->setPauseMode((QAbstractSocket::PauseModes)pauseMode);
}

bool QAbstractSocket_bind(QAbstractSocketH handle, const QHostAddressH address, quint16 port, unsigned int mode)
{
	return (bool) ((QAbstractSocket *)handle)->bind(*(const QHostAddress*)address, port, (QAbstractSocket::BindMode)mode);
}

bool QAbstractSocket_bind2(QAbstractSocketH handle, quint16 port, unsigned int mode)
{
	return (bool) ((QAbstractSocket *)handle)->bind(port, (QAbstractSocket::BindMode)mode);
}

void QAbstractSocket_connectToHost(QAbstractSocketH handle, PWideString hostName, quint16 port, unsigned int mode, QAbstractSocket::NetworkLayerProtocol protocol)
{
	QString t_hostName;
	copyPWideStringToQString(hostName, t_hostName);
	((QAbstractSocket *)handle)->connectToHost(t_hostName, port, (QIODevice::OpenMode)mode, protocol);
}

void QAbstractSocket_connectToHost2(QAbstractSocketH handle, const QHostAddressH address, quint16 port, unsigned int mode)
{
	((QAbstractSocket *)handle)->connectToHost(*(const QHostAddress*)address, port, (QIODevice::OpenMode)mode);
}

void QAbstractSocket_disconnectFromHost(QAbstractSocketH handle)
{
	((QAbstractSocket *)handle)->disconnectFromHost();
}

bool QAbstractSocket_isValid(QAbstractSocketH handle)
{
	return (bool) ((QAbstractSocket *)handle)->isValid();
}

qint64 QAbstractSocket_bytesAvailable(QAbstractSocketH handle)
{
	return (qint64) ((QAbstractSocket *)handle)->bytesAvailable();
}

qint64 QAbstractSocket_bytesToWrite(QAbstractSocketH handle)
{
	return (qint64) ((QAbstractSocket *)handle)->bytesToWrite();
}

bool QAbstractSocket_canReadLine(QAbstractSocketH handle)
{
	return (bool) ((QAbstractSocket *)handle)->canReadLine();
}

quint16 QAbstractSocket_localPort(QAbstractSocketH handle)
{
	return (quint16) ((QAbstractSocket *)handle)->localPort();
}

void QAbstractSocket_localAddress(QAbstractSocketH handle, QHostAddressH retval)
{
	*(QHostAddress *)retval = ((QAbstractSocket *)handle)->localAddress();
}

quint16 QAbstractSocket_peerPort(QAbstractSocketH handle)
{
	return (quint16) ((QAbstractSocket *)handle)->peerPort();
}

void QAbstractSocket_peerAddress(QAbstractSocketH handle, QHostAddressH retval)
{
	*(QHostAddress *)retval = ((QAbstractSocket *)handle)->peerAddress();
}

void QAbstractSocket_peerName(QAbstractSocketH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QAbstractSocket *)handle)->peerName();
	copyQStringToPWideString(t_retval, retval);
}

qint64 QAbstractSocket_readBufferSize(QAbstractSocketH handle)
{
	return (qint64) ((QAbstractSocket *)handle)->readBufferSize();
}

void QAbstractSocket_setReadBufferSize(QAbstractSocketH handle, qint64 size)
{
	((QAbstractSocket *)handle)->setReadBufferSize(size);
}

void QAbstractSocket_abort(QAbstractSocketH handle)
{
	((QAbstractSocket *)handle)->abort();
}

qintptr QAbstractSocket_socketDescriptor(QAbstractSocketH handle)
{
	return (qintptr) ((QAbstractSocket *)handle)->socketDescriptor();
}

void QAbstractSocket_setSocketOption(QAbstractSocketH handle, QAbstractSocket::SocketOption option, const QVariantH value)
{
	((QAbstractSocket *)handle)->setSocketOption(option, *(const QVariant*)value);
}

void QAbstractSocket_socketOption(QAbstractSocketH handle, QVariantH retval, QAbstractSocket::SocketOption option)
{
	*(QVariant *)retval = ((QAbstractSocket *)handle)->socketOption(option);
}

QAbstractSocket::SocketType QAbstractSocket_socketType(QAbstractSocketH handle)
{
	return (QAbstractSocket::SocketType) ((QAbstractSocket *)handle)->socketType();
}

QAbstractSocket::SocketError QAbstractSocket_error(QAbstractSocketH handle)
{
	return (QAbstractSocket::SocketError) ((QAbstractSocket *)handle)->error();
}

void QAbstractSocket_close(QAbstractSocketH handle)
{
	((QAbstractSocket *)handle)->close();
}

bool QAbstractSocket_isSequential(QAbstractSocketH handle)
{
	return (bool) ((QAbstractSocket *)handle)->isSequential();
}

bool QAbstractSocket_atEnd(QAbstractSocketH handle)
{
	return (bool) ((QAbstractSocket *)handle)->atEnd();
}

bool QAbstractSocket_flush(QAbstractSocketH handle)
{
	return (bool) ((QAbstractSocket *)handle)->flush();
}

bool QAbstractSocket_waitForConnected(QAbstractSocketH handle, int msecs)
{
	return (bool) ((QAbstractSocket *)handle)->waitForConnected(msecs);
}

bool QAbstractSocket_waitForReadyRead(QAbstractSocketH handle, int msecs)
{
	return (bool) ((QAbstractSocket *)handle)->waitForReadyRead(msecs);
}

bool QAbstractSocket_waitForBytesWritten(QAbstractSocketH handle, int msecs)
{
	return (bool) ((QAbstractSocket *)handle)->waitForBytesWritten(msecs);
}

bool QAbstractSocket_waitForDisconnected(QAbstractSocketH handle, int msecs)
{
	return (bool) ((QAbstractSocket *)handle)->waitForDisconnected(msecs);
}

void QAbstractSocket_setProxy(QAbstractSocketH handle, const QNetworkProxyH networkProxy)
{
	((QAbstractSocket *)handle)->setProxy(*(const QNetworkProxy*)networkProxy);
}

void QAbstractSocket_proxy(QAbstractSocketH handle, QNetworkProxyH retval)
{
	*(QNetworkProxy *)retval = ((QAbstractSocket *)handle)->proxy();
}

