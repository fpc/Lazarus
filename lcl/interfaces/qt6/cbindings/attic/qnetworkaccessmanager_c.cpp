//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qnetworkaccessmanager_c.h"

QNetworkAccessManagerH QNetworkAccessManager_Create(QObjectH parent)
{
	return (QNetworkAccessManagerH) new QNetworkAccessManager((QObject*)parent);
}

void QNetworkAccessManager_Destroy(QNetworkAccessManagerH handle)
{
	delete (QNetworkAccessManager *)handle;
}

void QNetworkAccessManager_clearAccessCache(QNetworkAccessManagerH handle)
{
	((QNetworkAccessManager *)handle)->clearAccessCache();
}

void QNetworkAccessManager_proxy(QNetworkAccessManagerH handle, QNetworkProxyH retval)
{
	*(QNetworkProxy *)retval = ((QNetworkAccessManager *)handle)->proxy();
}

void QNetworkAccessManager_setProxy(QNetworkAccessManagerH handle, const QNetworkProxyH proxy)
{
	((QNetworkAccessManager *)handle)->setProxy(*(const QNetworkProxy*)proxy);
}

QNetworkProxyFactoryH QNetworkAccessManager_proxyFactory(QNetworkAccessManagerH handle)
{
	return (QNetworkProxyFactoryH) ((QNetworkAccessManager *)handle)->proxyFactory();
}

void QNetworkAccessManager_setProxyFactory(QNetworkAccessManagerH handle, QNetworkProxyFactoryH factory)
{
	((QNetworkAccessManager *)handle)->setProxyFactory((QNetworkProxyFactory*)factory);
}

QAbstractNetworkCacheH QNetworkAccessManager_cache(QNetworkAccessManagerH handle)
{
	return (QAbstractNetworkCacheH) ((QNetworkAccessManager *)handle)->cache();
}

void QNetworkAccessManager_setCache(QNetworkAccessManagerH handle, QAbstractNetworkCacheH cache)
{
	((QNetworkAccessManager *)handle)->setCache((QAbstractNetworkCache*)cache);
}

QNetworkCookieJarH QNetworkAccessManager_cookieJar(QNetworkAccessManagerH handle)
{
	return (QNetworkCookieJarH) ((QNetworkAccessManager *)handle)->cookieJar();
}

void QNetworkAccessManager_setCookieJar(QNetworkAccessManagerH handle, QNetworkCookieJarH cookieJar)
{
	((QNetworkAccessManager *)handle)->setCookieJar((QNetworkCookieJar*)cookieJar);
}

QNetworkReplyH QNetworkAccessManager_head(QNetworkAccessManagerH handle, const QNetworkRequestH request)
{
	return (QNetworkReplyH) ((QNetworkAccessManager *)handle)->head(*(const QNetworkRequest*)request);
}

QNetworkReplyH QNetworkAccessManager_get(QNetworkAccessManagerH handle, const QNetworkRequestH request)
{
	return (QNetworkReplyH) ((QNetworkAccessManager *)handle)->get(*(const QNetworkRequest*)request);
}

QNetworkReplyH QNetworkAccessManager_post(QNetworkAccessManagerH handle, const QNetworkRequestH request, QIODeviceH data)
{
	return (QNetworkReplyH) ((QNetworkAccessManager *)handle)->post(*(const QNetworkRequest*)request, (QIODevice*)data);
}

QNetworkReplyH QNetworkAccessManager_post2(QNetworkAccessManagerH handle, const QNetworkRequestH request, const QByteArrayH data)
{
	return (QNetworkReplyH) ((QNetworkAccessManager *)handle)->post(*(const QNetworkRequest*)request, *(const QByteArray*)data);
}

QNetworkReplyH QNetworkAccessManager_post3(QNetworkAccessManagerH handle, const QNetworkRequestH request, QHttpMultiPartH multiPart)
{
	return (QNetworkReplyH) ((QNetworkAccessManager *)handle)->post(*(const QNetworkRequest*)request, (QHttpMultiPart*)multiPart);
}

QNetworkReplyH QNetworkAccessManager_put(QNetworkAccessManagerH handle, const QNetworkRequestH request, QIODeviceH data)
{
	return (QNetworkReplyH) ((QNetworkAccessManager *)handle)->put(*(const QNetworkRequest*)request, (QIODevice*)data);
}

QNetworkReplyH QNetworkAccessManager_put2(QNetworkAccessManagerH handle, const QNetworkRequestH request, const QByteArrayH data)
{
	return (QNetworkReplyH) ((QNetworkAccessManager *)handle)->put(*(const QNetworkRequest*)request, *(const QByteArray*)data);
}

QNetworkReplyH QNetworkAccessManager_put3(QNetworkAccessManagerH handle, const QNetworkRequestH request, QHttpMultiPartH multiPart)
{
	return (QNetworkReplyH) ((QNetworkAccessManager *)handle)->put(*(const QNetworkRequest*)request, (QHttpMultiPart*)multiPart);
}

QNetworkReplyH QNetworkAccessManager_deleteResource(QNetworkAccessManagerH handle, const QNetworkRequestH request)
{
	return (QNetworkReplyH) ((QNetworkAccessManager *)handle)->deleteResource(*(const QNetworkRequest*)request);
}

QNetworkReplyH QNetworkAccessManager_sendCustomRequest(QNetworkAccessManagerH handle, const QNetworkRequestH request, const QByteArrayH verb, QIODeviceH data)
{
	return (QNetworkReplyH) ((QNetworkAccessManager *)handle)->sendCustomRequest(*(const QNetworkRequest*)request, *(const QByteArray*)verb, (QIODevice*)data);
}

void QNetworkAccessManager_setConfiguration(QNetworkAccessManagerH handle, const QNetworkConfigurationH config)
{
	((QNetworkAccessManager *)handle)->setConfiguration(*(const QNetworkConfiguration*)config);
}

void QNetworkAccessManager_configuration(QNetworkAccessManagerH handle, QNetworkConfigurationH retval)
{
	*(QNetworkConfiguration *)retval = ((QNetworkAccessManager *)handle)->configuration();
}

void QNetworkAccessManager_activeConfiguration(QNetworkAccessManagerH handle, QNetworkConfigurationH retval)
{
	*(QNetworkConfiguration *)retval = ((QNetworkAccessManager *)handle)->activeConfiguration();
}

void QNetworkAccessManager_setNetworkAccessible(QNetworkAccessManagerH handle, QNetworkAccessManager::NetworkAccessibility accessible)
{
	((QNetworkAccessManager *)handle)->setNetworkAccessible(accessible);
}

QNetworkAccessManager::NetworkAccessibility QNetworkAccessManager_networkAccessible(QNetworkAccessManagerH handle)
{
	return (QNetworkAccessManager::NetworkAccessibility) ((QNetworkAccessManager *)handle)->networkAccessible();
}

