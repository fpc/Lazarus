//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QUDPSOCKET_C_H
#define QUDPSOCKET_C_H

#include <QtNetwork>
#include "pascalbind.h"

C_EXPORT QUdpSocketH QUdpSocket_Create(QObjectH parent);
C_EXPORT void QUdpSocket_Destroy(QUdpSocketH handle);
C_EXPORT bool QUdpSocket_joinMulticastGroup(QUdpSocketH handle, const QHostAddressH groupAddress);
C_EXPORT bool QUdpSocket_joinMulticastGroup2(QUdpSocketH handle, const QHostAddressH groupAddress, const QNetworkInterfaceH iface);
C_EXPORT bool QUdpSocket_leaveMulticastGroup(QUdpSocketH handle, const QHostAddressH groupAddress);
C_EXPORT bool QUdpSocket_leaveMulticastGroup2(QUdpSocketH handle, const QHostAddressH groupAddress, const QNetworkInterfaceH iface);
C_EXPORT void QUdpSocket_multicastInterface(QUdpSocketH handle, QNetworkInterfaceH retval);
C_EXPORT void QUdpSocket_setMulticastInterface(QUdpSocketH handle, const QNetworkInterfaceH iface);
C_EXPORT bool QUdpSocket_hasPendingDatagrams(QUdpSocketH handle);
C_EXPORT qint64 QUdpSocket_pendingDatagramSize(QUdpSocketH handle);
C_EXPORT qint64 QUdpSocket_readDatagram(QUdpSocketH handle, char* data, qint64 maxlen, QHostAddressH host, quint16* port);
C_EXPORT qint64 QUdpSocket_writeDatagram(QUdpSocketH handle, const char* data, qint64 len, const QHostAddressH host, quint16 port);
C_EXPORT qint64 QUdpSocket_writeDatagram2(QUdpSocketH handle, const QByteArrayH datagram, const QHostAddressH host, quint16 port);

#endif
