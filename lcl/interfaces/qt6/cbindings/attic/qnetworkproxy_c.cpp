//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qnetworkproxy_c.h"

QNetworkProxyQueryH QNetworkProxyQuery_Create()
{
	return (QNetworkProxyQueryH) new QNetworkProxyQuery();
}

void QNetworkProxyQuery_Destroy(QNetworkProxyQueryH handle)
{
	delete (QNetworkProxyQuery *)handle;
}

QNetworkProxyQueryH QNetworkProxyQuery_Create2(const QUrlH requestUrl, QNetworkProxyQuery::QueryType queryType)
{
	return (QNetworkProxyQueryH) new QNetworkProxyQuery(*(const QUrl*)requestUrl, queryType);
}

QNetworkProxyQueryH QNetworkProxyQuery_Create3(PWideString hostname, int port, PWideString protocolTag, QNetworkProxyQuery::QueryType queryType)
{
	QString t_hostname;
	QString t_protocolTag;
	copyPWideStringToQString(hostname, t_hostname);
	copyPWideStringToQString(protocolTag, t_protocolTag);
	return (QNetworkProxyQueryH) new QNetworkProxyQuery(t_hostname, port, t_protocolTag, queryType);
}

QNetworkProxyQueryH QNetworkProxyQuery_Create4(quint16 bindPort, PWideString protocolTag, QNetworkProxyQuery::QueryType queryType)
{
	QString t_protocolTag;
	copyPWideStringToQString(protocolTag, t_protocolTag);
	return (QNetworkProxyQueryH) new QNetworkProxyQuery(bindPort, t_protocolTag, queryType);
}

QNetworkProxyQueryH QNetworkProxyQuery_Create5(const QNetworkProxyQueryH other)
{
	return (QNetworkProxyQueryH) new QNetworkProxyQuery(*(const QNetworkProxyQuery*)other);
}

QNetworkProxyQueryH QNetworkProxyQuery_Create6(const QNetworkConfigurationH networkConfiguration, const QUrlH requestUrl, QNetworkProxyQuery::QueryType queryType)
{
	return (QNetworkProxyQueryH) new QNetworkProxyQuery(*(const QNetworkConfiguration*)networkConfiguration, *(const QUrl*)requestUrl, queryType);
}

QNetworkProxyQueryH QNetworkProxyQuery_Create7(const QNetworkConfigurationH networkConfiguration, PWideString hostname, int port, PWideString protocolTag, QNetworkProxyQuery::QueryType queryType)
{
	QString t_hostname;
	QString t_protocolTag;
	copyPWideStringToQString(hostname, t_hostname);
	copyPWideStringToQString(protocolTag, t_protocolTag);
	return (QNetworkProxyQueryH) new QNetworkProxyQuery(*(const QNetworkConfiguration*)networkConfiguration, t_hostname, port, t_protocolTag, queryType);
}

QNetworkProxyQueryH QNetworkProxyQuery_Create8(const QNetworkConfigurationH networkConfiguration, quint16 bindPort, PWideString protocolTag, QNetworkProxyQuery::QueryType queryType)
{
	QString t_protocolTag;
	copyPWideStringToQString(protocolTag, t_protocolTag);
	return (QNetworkProxyQueryH) new QNetworkProxyQuery(*(const QNetworkConfiguration*)networkConfiguration, bindPort, t_protocolTag, queryType);
}

void QNetworkProxyQuery_swap(QNetworkProxyQueryH handle, QNetworkProxyQueryH other)
{
	((QNetworkProxyQuery *)handle)->swap(*(QNetworkProxyQuery*)other);
}

QNetworkProxyQuery::QueryType QNetworkProxyQuery_queryType(QNetworkProxyQueryH handle)
{
	return (QNetworkProxyQuery::QueryType) ((QNetworkProxyQuery *)handle)->queryType();
}

void QNetworkProxyQuery_setQueryType(QNetworkProxyQueryH handle, QNetworkProxyQuery::QueryType type)
{
	((QNetworkProxyQuery *)handle)->setQueryType(type);
}

int QNetworkProxyQuery_peerPort(QNetworkProxyQueryH handle)
{
	return (int) ((QNetworkProxyQuery *)handle)->peerPort();
}

void QNetworkProxyQuery_setPeerPort(QNetworkProxyQueryH handle, int port)
{
	((QNetworkProxyQuery *)handle)->setPeerPort(port);
}

void QNetworkProxyQuery_peerHostName(QNetworkProxyQueryH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QNetworkProxyQuery *)handle)->peerHostName();
	copyQStringToPWideString(t_retval, retval);
}

void QNetworkProxyQuery_setPeerHostName(QNetworkProxyQueryH handle, PWideString hostname)
{
	QString t_hostname;
	copyPWideStringToQString(hostname, t_hostname);
	((QNetworkProxyQuery *)handle)->setPeerHostName(t_hostname);
}

int QNetworkProxyQuery_localPort(QNetworkProxyQueryH handle)
{
	return (int) ((QNetworkProxyQuery *)handle)->localPort();
}

void QNetworkProxyQuery_setLocalPort(QNetworkProxyQueryH handle, int port)
{
	((QNetworkProxyQuery *)handle)->setLocalPort(port);
}

void QNetworkProxyQuery_protocolTag(QNetworkProxyQueryH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QNetworkProxyQuery *)handle)->protocolTag();
	copyQStringToPWideString(t_retval, retval);
}

void QNetworkProxyQuery_setProtocolTag(QNetworkProxyQueryH handle, PWideString protocolTag)
{
	QString t_protocolTag;
	copyPWideStringToQString(protocolTag, t_protocolTag);
	((QNetworkProxyQuery *)handle)->setProtocolTag(t_protocolTag);
}

void QNetworkProxyQuery_url(QNetworkProxyQueryH handle, QUrlH retval)
{
	*(QUrl *)retval = ((QNetworkProxyQuery *)handle)->url();
}

void QNetworkProxyQuery_setUrl(QNetworkProxyQueryH handle, const QUrlH url)
{
	((QNetworkProxyQuery *)handle)->setUrl(*(const QUrl*)url);
}

void QNetworkProxyQuery_networkConfiguration(QNetworkProxyQueryH handle, QNetworkConfigurationH retval)
{
	*(QNetworkConfiguration *)retval = ((QNetworkProxyQuery *)handle)->networkConfiguration();
}

void QNetworkProxyQuery_setNetworkConfiguration(QNetworkProxyQueryH handle, const QNetworkConfigurationH networkConfiguration)
{
	((QNetworkProxyQuery *)handle)->setNetworkConfiguration(*(const QNetworkConfiguration*)networkConfiguration);
}

QNetworkProxyH QNetworkProxy_Create()
{
	return (QNetworkProxyH) new QNetworkProxy();
}

void QNetworkProxy_Destroy(QNetworkProxyH handle)
{
	delete (QNetworkProxy *)handle;
}

QNetworkProxyH QNetworkProxy_Create2(QNetworkProxy::ProxyType type, PWideString hostName, quint16 port, PWideString user, PWideString password)
{
	QString t_hostName;
	QString t_user;
	QString t_password;
	copyPWideStringToQString(hostName, t_hostName);
	copyPWideStringToQString(user, t_user);
	copyPWideStringToQString(password, t_password);
	return (QNetworkProxyH) new QNetworkProxy(type, t_hostName, port, t_user, t_password);
}

QNetworkProxyH QNetworkProxy_Create3(const QNetworkProxyH other)
{
	return (QNetworkProxyH) new QNetworkProxy(*(const QNetworkProxy*)other);
}

void QNetworkProxy_swap(QNetworkProxyH handle, QNetworkProxyH other)
{
	((QNetworkProxy *)handle)->swap(*(QNetworkProxy*)other);
}

void QNetworkProxy_setType(QNetworkProxyH handle, QNetworkProxy::ProxyType type)
{
	((QNetworkProxy *)handle)->setType(type);
}

QNetworkProxy::ProxyType QNetworkProxy_type(QNetworkProxyH handle)
{
	return (QNetworkProxy::ProxyType) ((QNetworkProxy *)handle)->type();
}

void QNetworkProxy_setCapabilities(QNetworkProxyH handle, unsigned int capab)
{
	((QNetworkProxy *)handle)->setCapabilities((QNetworkProxy::Capabilities)capab);
}

unsigned int QNetworkProxy_capabilities(QNetworkProxyH handle)
{
	return (unsigned int) ((QNetworkProxy *)handle)->capabilities();
}

bool QNetworkProxy_isCachingProxy(QNetworkProxyH handle)
{
	return (bool) ((QNetworkProxy *)handle)->isCachingProxy();
}

bool QNetworkProxy_isTransparentProxy(QNetworkProxyH handle)
{
	return (bool) ((QNetworkProxy *)handle)->isTransparentProxy();
}

void QNetworkProxy_setUser(QNetworkProxyH handle, PWideString userName)
{
	QString t_userName;
	copyPWideStringToQString(userName, t_userName);
	((QNetworkProxy *)handle)->setUser(t_userName);
}

void QNetworkProxy_user(QNetworkProxyH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QNetworkProxy *)handle)->user();
	copyQStringToPWideString(t_retval, retval);
}

void QNetworkProxy_setPassword(QNetworkProxyH handle, PWideString password)
{
	QString t_password;
	copyPWideStringToQString(password, t_password);
	((QNetworkProxy *)handle)->setPassword(t_password);
}

void QNetworkProxy_password(QNetworkProxyH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QNetworkProxy *)handle)->password();
	copyQStringToPWideString(t_retval, retval);
}

void QNetworkProxy_setHostName(QNetworkProxyH handle, PWideString hostName)
{
	QString t_hostName;
	copyPWideStringToQString(hostName, t_hostName);
	((QNetworkProxy *)handle)->setHostName(t_hostName);
}

void QNetworkProxy_hostName(QNetworkProxyH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QNetworkProxy *)handle)->hostName();
	copyQStringToPWideString(t_retval, retval);
}

void QNetworkProxy_setPort(QNetworkProxyH handle, quint16 port)
{
	((QNetworkProxy *)handle)->setPort(port);
}

quint16 QNetworkProxy_port(QNetworkProxyH handle)
{
	return (quint16) ((QNetworkProxy *)handle)->port();
}

void QNetworkProxy_setApplicationProxy(const QNetworkProxyH proxy)
{
	QNetworkProxy::setApplicationProxy(*(const QNetworkProxy*)proxy);
}

void QNetworkProxy_applicationProxy(QNetworkProxyH retval)
{
	*(QNetworkProxy *)retval = QNetworkProxy::applicationProxy();
}

void QNetworkProxy_header(QNetworkProxyH handle, QVariantH retval, QNetworkRequest::KnownHeaders header)
{
	*(QVariant *)retval = ((QNetworkProxy *)handle)->header(header);
}

void QNetworkProxy_setHeader(QNetworkProxyH handle, QNetworkRequest::KnownHeaders header, const QVariantH value)
{
	((QNetworkProxy *)handle)->setHeader(header, *(const QVariant*)value);
}

bool QNetworkProxy_hasRawHeader(QNetworkProxyH handle, const QByteArrayH headerName)
{
	return (bool) ((QNetworkProxy *)handle)->hasRawHeader(*(const QByteArray*)headerName);
}

void QNetworkProxy_rawHeader(QNetworkProxyH handle, QByteArrayH retval, const QByteArrayH headerName)
{
	*(QByteArray *)retval = ((QNetworkProxy *)handle)->rawHeader(*(const QByteArray*)headerName);
}

void QNetworkProxy_setRawHeader(QNetworkProxyH handle, const QByteArrayH headerName, const QByteArrayH value)
{
	((QNetworkProxy *)handle)->setRawHeader(*(const QByteArray*)headerName, *(const QByteArray*)value);
}

void QNetworkProxyFactory_setUseSystemConfiguration(bool enable)
{
	QNetworkProxyFactory::setUseSystemConfiguration(enable);
}

void QNetworkProxyFactory_setApplicationProxyFactory(QNetworkProxyFactoryH factory)
{
	QNetworkProxyFactory::setApplicationProxyFactory((QNetworkProxyFactory*)factory);
}

