//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QABSTRACTSOCKET_C_H
#define QABSTRACTSOCKET_C_H

#include <QtNetwork>
#include "pascalbind.h"

C_EXPORT QAbstractSocketH QAbstractSocket_Create(QAbstractSocket::SocketType socketType, QObjectH parent);
C_EXPORT void QAbstractSocket_Destroy(QAbstractSocketH handle);
C_EXPORT void QAbstractSocket_resume(QAbstractSocketH handle);
C_EXPORT unsigned int QAbstractSocket_pauseMode(QAbstractSocketH handle);
C_EXPORT void QAbstractSocket_setPauseMode(QAbstractSocketH handle, unsigned int pauseMode);
C_EXPORT bool QAbstractSocket_bind(QAbstractSocketH handle, const QHostAddressH address, quint16 port, unsigned int mode);
C_EXPORT bool QAbstractSocket_bind2(QAbstractSocketH handle, quint16 port, unsigned int mode);
C_EXPORT void QAbstractSocket_connectToHost(QAbstractSocketH handle, PWideString hostName, quint16 port, unsigned int mode, QAbstractSocket::NetworkLayerProtocol protocol);
C_EXPORT void QAbstractSocket_connectToHost2(QAbstractSocketH handle, const QHostAddressH address, quint16 port, unsigned int mode);
C_EXPORT void QAbstractSocket_disconnectFromHost(QAbstractSocketH handle);
C_EXPORT bool QAbstractSocket_isValid(QAbstractSocketH handle);
C_EXPORT qint64 QAbstractSocket_bytesAvailable(QAbstractSocketH handle);
C_EXPORT qint64 QAbstractSocket_bytesToWrite(QAbstractSocketH handle);
C_EXPORT bool QAbstractSocket_canReadLine(QAbstractSocketH handle);
C_EXPORT quint16 QAbstractSocket_localPort(QAbstractSocketH handle);
C_EXPORT void QAbstractSocket_localAddress(QAbstractSocketH handle, QHostAddressH retval);
C_EXPORT quint16 QAbstractSocket_peerPort(QAbstractSocketH handle);
C_EXPORT void QAbstractSocket_peerAddress(QAbstractSocketH handle, QHostAddressH retval);
C_EXPORT void QAbstractSocket_peerName(QAbstractSocketH handle, PWideString retval);
C_EXPORT qint64 QAbstractSocket_readBufferSize(QAbstractSocketH handle);
C_EXPORT void QAbstractSocket_setReadBufferSize(QAbstractSocketH handle, qint64 size);
C_EXPORT void QAbstractSocket_abort(QAbstractSocketH handle);
C_EXPORT qintptr QAbstractSocket_socketDescriptor(QAbstractSocketH handle);
C_EXPORT void QAbstractSocket_setSocketOption(QAbstractSocketH handle, QAbstractSocket::SocketOption option, const QVariantH value);
C_EXPORT void QAbstractSocket_socketOption(QAbstractSocketH handle, QVariantH retval, QAbstractSocket::SocketOption option);
C_EXPORT QAbstractSocket::SocketType QAbstractSocket_socketType(QAbstractSocketH handle);
C_EXPORT QAbstractSocket::SocketError QAbstractSocket_error(QAbstractSocketH handle);
C_EXPORT void QAbstractSocket_close(QAbstractSocketH handle);
C_EXPORT bool QAbstractSocket_isSequential(QAbstractSocketH handle);
C_EXPORT bool QAbstractSocket_atEnd(QAbstractSocketH handle);
C_EXPORT bool QAbstractSocket_flush(QAbstractSocketH handle);
C_EXPORT bool QAbstractSocket_waitForConnected(QAbstractSocketH handle, int msecs);
C_EXPORT bool QAbstractSocket_waitForReadyRead(QAbstractSocketH handle, int msecs);
C_EXPORT bool QAbstractSocket_waitForBytesWritten(QAbstractSocketH handle, int msecs);
C_EXPORT bool QAbstractSocket_waitForDisconnected(QAbstractSocketH handle, int msecs);
C_EXPORT void QAbstractSocket_setProxy(QAbstractSocketH handle, const QNetworkProxyH networkProxy);
C_EXPORT void QAbstractSocket_proxy(QAbstractSocketH handle, QNetworkProxyH retval);

#endif
