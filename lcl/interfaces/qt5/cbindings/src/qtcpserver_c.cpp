//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qtcpserver_c.h"

QTcpServerH QTcpServer_Create(QObjectH parent)
{
	return (QTcpServerH) new QTcpServer((QObject*)parent);
}

void QTcpServer_Destroy(QTcpServerH handle)
{
	delete (QTcpServer *)handle;
}

bool QTcpServer_listen(QTcpServerH handle, const QHostAddressH address, quint16 port)
{
	return (bool) ((QTcpServer *)handle)->listen(*(const QHostAddress*)address, port);
}

void QTcpServer_close(QTcpServerH handle)
{
	((QTcpServer *)handle)->close();
}

bool QTcpServer_isListening(QTcpServerH handle)
{
	return (bool) ((QTcpServer *)handle)->isListening();
}

void QTcpServer_setMaxPendingConnections(QTcpServerH handle, int numConnections)
{
	((QTcpServer *)handle)->setMaxPendingConnections(numConnections);
}

int QTcpServer_maxPendingConnections(QTcpServerH handle)
{
	return (int) ((QTcpServer *)handle)->maxPendingConnections();
}

quint16 QTcpServer_serverPort(QTcpServerH handle)
{
	return (quint16) ((QTcpServer *)handle)->serverPort();
}

void QTcpServer_serverAddress(QTcpServerH handle, QHostAddressH retval)
{
	*(QHostAddress *)retval = ((QTcpServer *)handle)->serverAddress();
}

qintptr QTcpServer_socketDescriptor(QTcpServerH handle)
{
	return (qintptr) ((QTcpServer *)handle)->socketDescriptor();
}

bool QTcpServer_setSocketDescriptor(QTcpServerH handle, qintptr socketDescriptor)
{
	return (bool) ((QTcpServer *)handle)->setSocketDescriptor(socketDescriptor);
}

bool QTcpServer_waitForNewConnection(QTcpServerH handle, int msec, bool* timedOut)
{
	return (bool) ((QTcpServer *)handle)->waitForNewConnection(msec, timedOut);
}

bool QTcpServer_hasPendingConnections(QTcpServerH handle)
{
	return (bool) ((QTcpServer *)handle)->hasPendingConnections();
}

QTcpSocketH QTcpServer_nextPendingConnection(QTcpServerH handle)
{
	return (QTcpSocketH) ((QTcpServer *)handle)->nextPendingConnection();
}

QAbstractSocket::SocketError QTcpServer_serverError(QTcpServerH handle)
{
	return (QAbstractSocket::SocketError) ((QTcpServer *)handle)->serverError();
}

void QTcpServer_errorString(QTcpServerH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QTcpServer *)handle)->errorString();
	copyQStringToPWideString(t_retval, retval);
}

void QTcpServer_pauseAccepting(QTcpServerH handle)
{
	((QTcpServer *)handle)->pauseAccepting();
}

void QTcpServer_resumeAccepting(QTcpServerH handle)
{
	((QTcpServer *)handle)->resumeAccepting();
}

void QTcpServer_setProxy(QTcpServerH handle, const QNetworkProxyH networkProxy)
{
	((QTcpServer *)handle)->setProxy(*(const QNetworkProxy*)networkProxy);
}

void QTcpServer_proxy(QTcpServerH handle, QNetworkProxyH retval)
{
	*(QNetworkProxy *)retval = ((QTcpServer *)handle)->proxy();
}

