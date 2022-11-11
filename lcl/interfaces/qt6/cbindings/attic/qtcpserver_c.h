//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QTCPSERVER_C_H
#define QTCPSERVER_C_H

#include <QtNetwork>
#include "pascalbind.h"

C_EXPORT QTcpServerH QTcpServer_Create(QObjectH parent);
C_EXPORT void QTcpServer_Destroy(QTcpServerH handle);
C_EXPORT bool QTcpServer_listen(QTcpServerH handle, const QHostAddressH address, quint16 port);
C_EXPORT void QTcpServer_close(QTcpServerH handle);
C_EXPORT bool QTcpServer_isListening(QTcpServerH handle);
C_EXPORT void QTcpServer_setMaxPendingConnections(QTcpServerH handle, int numConnections);
C_EXPORT int QTcpServer_maxPendingConnections(QTcpServerH handle);
C_EXPORT quint16 QTcpServer_serverPort(QTcpServerH handle);
C_EXPORT void QTcpServer_serverAddress(QTcpServerH handle, QHostAddressH retval);
C_EXPORT qintptr QTcpServer_socketDescriptor(QTcpServerH handle);
C_EXPORT bool QTcpServer_setSocketDescriptor(QTcpServerH handle, qintptr socketDescriptor);
C_EXPORT bool QTcpServer_waitForNewConnection(QTcpServerH handle, int msec, bool* timedOut);
C_EXPORT bool QTcpServer_hasPendingConnections(QTcpServerH handle);
C_EXPORT QTcpSocketH QTcpServer_nextPendingConnection(QTcpServerH handle);
C_EXPORT QAbstractSocket::SocketError QTcpServer_serverError(QTcpServerH handle);
C_EXPORT void QTcpServer_errorString(QTcpServerH handle, PWideString retval);
C_EXPORT void QTcpServer_pauseAccepting(QTcpServerH handle);
C_EXPORT void QTcpServer_resumeAccepting(QTcpServerH handle);
C_EXPORT void QTcpServer_setProxy(QTcpServerH handle, const QNetworkProxyH networkProxy);
C_EXPORT void QTcpServer_proxy(QTcpServerH handle, QNetworkProxyH retval);

#endif
