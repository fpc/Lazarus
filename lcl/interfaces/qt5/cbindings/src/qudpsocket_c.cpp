//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qudpsocket_c.h"

QUdpSocketH QUdpSocket_Create(QObjectH parent)
{
	return (QUdpSocketH) new QUdpSocket((QObject*)parent);
}

void QUdpSocket_Destroy(QUdpSocketH handle)
{
	delete (QUdpSocket *)handle;
}

bool QUdpSocket_joinMulticastGroup(QUdpSocketH handle, const QHostAddressH groupAddress)
{
	return (bool) ((QUdpSocket *)handle)->joinMulticastGroup(*(const QHostAddress*)groupAddress);
}

bool QUdpSocket_joinMulticastGroup2(QUdpSocketH handle, const QHostAddressH groupAddress, const QNetworkInterfaceH iface)
{
	return (bool) ((QUdpSocket *)handle)->joinMulticastGroup(*(const QHostAddress*)groupAddress, *(const QNetworkInterface*)iface);
}

bool QUdpSocket_leaveMulticastGroup(QUdpSocketH handle, const QHostAddressH groupAddress)
{
	return (bool) ((QUdpSocket *)handle)->leaveMulticastGroup(*(const QHostAddress*)groupAddress);
}

bool QUdpSocket_leaveMulticastGroup2(QUdpSocketH handle, const QHostAddressH groupAddress, const QNetworkInterfaceH iface)
{
	return (bool) ((QUdpSocket *)handle)->leaveMulticastGroup(*(const QHostAddress*)groupAddress, *(const QNetworkInterface*)iface);
}

void QUdpSocket_multicastInterface(QUdpSocketH handle, QNetworkInterfaceH retval)
{
	*(QNetworkInterface *)retval = ((QUdpSocket *)handle)->multicastInterface();
}

void QUdpSocket_setMulticastInterface(QUdpSocketH handle, const QNetworkInterfaceH iface)
{
	((QUdpSocket *)handle)->setMulticastInterface(*(const QNetworkInterface*)iface);
}

bool QUdpSocket_hasPendingDatagrams(QUdpSocketH handle)
{
	return (bool) ((QUdpSocket *)handle)->hasPendingDatagrams();
}

qint64 QUdpSocket_pendingDatagramSize(QUdpSocketH handle)
{
	return (qint64) ((QUdpSocket *)handle)->pendingDatagramSize();
}

qint64 QUdpSocket_readDatagram(QUdpSocketH handle, char* data, qint64 maxlen, QHostAddressH host, quint16* port)
{
	return (qint64) ((QUdpSocket *)handle)->readDatagram(data, maxlen, (QHostAddress*)host, port);
}

qint64 QUdpSocket_writeDatagram(QUdpSocketH handle, const char* data, qint64 len, const QHostAddressH host, quint16 port)
{
	return (qint64) ((QUdpSocket *)handle)->writeDatagram(data, len, *(const QHostAddress*)host, port);
}

qint64 QUdpSocket_writeDatagram2(QUdpSocketH handle, const QByteArrayH datagram, const QHostAddressH host, quint16 port)
{
	return (qint64) ((QUdpSocket *)handle)->writeDatagram(*(const QByteArray*)datagram, *(const QHostAddress*)host, port);
}

