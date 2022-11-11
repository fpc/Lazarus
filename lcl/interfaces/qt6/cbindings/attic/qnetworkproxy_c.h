//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QNETWORKPROXY_C_H
#define QNETWORKPROXY_C_H

#include <QtNetwork>
#include "pascalbind.h"

C_EXPORT QNetworkProxyQueryH QNetworkProxyQuery_Create();
C_EXPORT void QNetworkProxyQuery_Destroy(QNetworkProxyQueryH handle);
C_EXPORT QNetworkProxyQueryH QNetworkProxyQuery_Create2(const QUrlH requestUrl, QNetworkProxyQuery::QueryType queryType);
C_EXPORT QNetworkProxyQueryH QNetworkProxyQuery_Create3(PWideString hostname, int port, PWideString protocolTag, QNetworkProxyQuery::QueryType queryType);
C_EXPORT QNetworkProxyQueryH QNetworkProxyQuery_Create4(quint16 bindPort, PWideString protocolTag, QNetworkProxyQuery::QueryType queryType);
C_EXPORT QNetworkProxyQueryH QNetworkProxyQuery_Create5(const QNetworkProxyQueryH other);
C_EXPORT QNetworkProxyQueryH QNetworkProxyQuery_Create6(const QNetworkConfigurationH networkConfiguration, const QUrlH requestUrl, QNetworkProxyQuery::QueryType queryType);
C_EXPORT QNetworkProxyQueryH QNetworkProxyQuery_Create7(const QNetworkConfigurationH networkConfiguration, PWideString hostname, int port, PWideString protocolTag, QNetworkProxyQuery::QueryType queryType);
C_EXPORT QNetworkProxyQueryH QNetworkProxyQuery_Create8(const QNetworkConfigurationH networkConfiguration, quint16 bindPort, PWideString protocolTag, QNetworkProxyQuery::QueryType queryType);
C_EXPORT void QNetworkProxyQuery_swap(QNetworkProxyQueryH handle, QNetworkProxyQueryH other);
C_EXPORT QNetworkProxyQuery::QueryType QNetworkProxyQuery_queryType(QNetworkProxyQueryH handle);
C_EXPORT void QNetworkProxyQuery_setQueryType(QNetworkProxyQueryH handle, QNetworkProxyQuery::QueryType type);
C_EXPORT int QNetworkProxyQuery_peerPort(QNetworkProxyQueryH handle);
C_EXPORT void QNetworkProxyQuery_setPeerPort(QNetworkProxyQueryH handle, int port);
C_EXPORT void QNetworkProxyQuery_peerHostName(QNetworkProxyQueryH handle, PWideString retval);
C_EXPORT void QNetworkProxyQuery_setPeerHostName(QNetworkProxyQueryH handle, PWideString hostname);
C_EXPORT int QNetworkProxyQuery_localPort(QNetworkProxyQueryH handle);
C_EXPORT void QNetworkProxyQuery_setLocalPort(QNetworkProxyQueryH handle, int port);
C_EXPORT void QNetworkProxyQuery_protocolTag(QNetworkProxyQueryH handle, PWideString retval);
C_EXPORT void QNetworkProxyQuery_setProtocolTag(QNetworkProxyQueryH handle, PWideString protocolTag);
C_EXPORT void QNetworkProxyQuery_url(QNetworkProxyQueryH handle, QUrlH retval);
C_EXPORT void QNetworkProxyQuery_setUrl(QNetworkProxyQueryH handle, const QUrlH url);
C_EXPORT void QNetworkProxyQuery_networkConfiguration(QNetworkProxyQueryH handle, QNetworkConfigurationH retval);
C_EXPORT void QNetworkProxyQuery_setNetworkConfiguration(QNetworkProxyQueryH handle, const QNetworkConfigurationH networkConfiguration);
C_EXPORT QNetworkProxyH QNetworkProxy_Create();
C_EXPORT void QNetworkProxy_Destroy(QNetworkProxyH handle);
C_EXPORT QNetworkProxyH QNetworkProxy_Create2(QNetworkProxy::ProxyType type, PWideString hostName, quint16 port, PWideString user, PWideString password);
C_EXPORT QNetworkProxyH QNetworkProxy_Create3(const QNetworkProxyH other);
C_EXPORT void QNetworkProxy_swap(QNetworkProxyH handle, QNetworkProxyH other);
C_EXPORT void QNetworkProxy_setType(QNetworkProxyH handle, QNetworkProxy::ProxyType type);
C_EXPORT QNetworkProxy::ProxyType QNetworkProxy_type(QNetworkProxyH handle);
C_EXPORT void QNetworkProxy_setCapabilities(QNetworkProxyH handle, unsigned int capab);
C_EXPORT unsigned int QNetworkProxy_capabilities(QNetworkProxyH handle);
C_EXPORT bool QNetworkProxy_isCachingProxy(QNetworkProxyH handle);
C_EXPORT bool QNetworkProxy_isTransparentProxy(QNetworkProxyH handle);
C_EXPORT void QNetworkProxy_setUser(QNetworkProxyH handle, PWideString userName);
C_EXPORT void QNetworkProxy_user(QNetworkProxyH handle, PWideString retval);
C_EXPORT void QNetworkProxy_setPassword(QNetworkProxyH handle, PWideString password);
C_EXPORT void QNetworkProxy_password(QNetworkProxyH handle, PWideString retval);
C_EXPORT void QNetworkProxy_setHostName(QNetworkProxyH handle, PWideString hostName);
C_EXPORT void QNetworkProxy_hostName(QNetworkProxyH handle, PWideString retval);
C_EXPORT void QNetworkProxy_setPort(QNetworkProxyH handle, quint16 port);
C_EXPORT quint16 QNetworkProxy_port(QNetworkProxyH handle);
C_EXPORT void QNetworkProxy_setApplicationProxy(const QNetworkProxyH proxy);
C_EXPORT void QNetworkProxy_applicationProxy(QNetworkProxyH retval);
C_EXPORT void QNetworkProxy_header(QNetworkProxyH handle, QVariantH retval, QNetworkRequest::KnownHeaders header);
C_EXPORT void QNetworkProxy_setHeader(QNetworkProxyH handle, QNetworkRequest::KnownHeaders header, const QVariantH value);
C_EXPORT bool QNetworkProxy_hasRawHeader(QNetworkProxyH handle, const QByteArrayH headerName);
C_EXPORT void QNetworkProxy_rawHeader(QNetworkProxyH handle, QByteArrayH retval, const QByteArrayH headerName);
C_EXPORT void QNetworkProxy_setRawHeader(QNetworkProxyH handle, const QByteArrayH headerName, const QByteArrayH value);
C_EXPORT void QNetworkProxyFactory_setUseSystemConfiguration(bool enable);
C_EXPORT void QNetworkProxyFactory_setApplicationProxyFactory(QNetworkProxyFactoryH factory);

#endif
