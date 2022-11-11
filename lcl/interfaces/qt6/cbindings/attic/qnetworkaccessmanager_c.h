//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QNETWORKACCESSMANAGER_C_H
#define QNETWORKACCESSMANAGER_C_H

#include <QtNetwork>
#include "pascalbind.h"

C_EXPORT QNetworkAccessManagerH QNetworkAccessManager_Create(QObjectH parent);
C_EXPORT void QNetworkAccessManager_Destroy(QNetworkAccessManagerH handle);
C_EXPORT void QNetworkAccessManager_clearAccessCache(QNetworkAccessManagerH handle);
C_EXPORT void QNetworkAccessManager_proxy(QNetworkAccessManagerH handle, QNetworkProxyH retval);
C_EXPORT void QNetworkAccessManager_setProxy(QNetworkAccessManagerH handle, const QNetworkProxyH proxy);
C_EXPORT QNetworkProxyFactoryH QNetworkAccessManager_proxyFactory(QNetworkAccessManagerH handle);
C_EXPORT void QNetworkAccessManager_setProxyFactory(QNetworkAccessManagerH handle, QNetworkProxyFactoryH factory);
C_EXPORT QAbstractNetworkCacheH QNetworkAccessManager_cache(QNetworkAccessManagerH handle);
C_EXPORT void QNetworkAccessManager_setCache(QNetworkAccessManagerH handle, QAbstractNetworkCacheH cache);
C_EXPORT QNetworkCookieJarH QNetworkAccessManager_cookieJar(QNetworkAccessManagerH handle);
C_EXPORT void QNetworkAccessManager_setCookieJar(QNetworkAccessManagerH handle, QNetworkCookieJarH cookieJar);
C_EXPORT QNetworkReplyH QNetworkAccessManager_head(QNetworkAccessManagerH handle, const QNetworkRequestH request);
C_EXPORT QNetworkReplyH QNetworkAccessManager_get(QNetworkAccessManagerH handle, const QNetworkRequestH request);
C_EXPORT QNetworkReplyH QNetworkAccessManager_post(QNetworkAccessManagerH handle, const QNetworkRequestH request, QIODeviceH data);
C_EXPORT QNetworkReplyH QNetworkAccessManager_post2(QNetworkAccessManagerH handle, const QNetworkRequestH request, const QByteArrayH data);
C_EXPORT QNetworkReplyH QNetworkAccessManager_post3(QNetworkAccessManagerH handle, const QNetworkRequestH request, QHttpMultiPartH multiPart);
C_EXPORT QNetworkReplyH QNetworkAccessManager_put(QNetworkAccessManagerH handle, const QNetworkRequestH request, QIODeviceH data);
C_EXPORT QNetworkReplyH QNetworkAccessManager_put2(QNetworkAccessManagerH handle, const QNetworkRequestH request, const QByteArrayH data);
C_EXPORT QNetworkReplyH QNetworkAccessManager_put3(QNetworkAccessManagerH handle, const QNetworkRequestH request, QHttpMultiPartH multiPart);
C_EXPORT QNetworkReplyH QNetworkAccessManager_deleteResource(QNetworkAccessManagerH handle, const QNetworkRequestH request);
C_EXPORT QNetworkReplyH QNetworkAccessManager_sendCustomRequest(QNetworkAccessManagerH handle, const QNetworkRequestH request, const QByteArrayH verb, QIODeviceH data);
C_EXPORT void QNetworkAccessManager_setConfiguration(QNetworkAccessManagerH handle, const QNetworkConfigurationH config);
C_EXPORT void QNetworkAccessManager_configuration(QNetworkAccessManagerH handle, QNetworkConfigurationH retval);
C_EXPORT void QNetworkAccessManager_activeConfiguration(QNetworkAccessManagerH handle, QNetworkConfigurationH retval);
C_EXPORT void QNetworkAccessManager_setNetworkAccessible(QNetworkAccessManagerH handle, QNetworkAccessManager::NetworkAccessibility accessible);
C_EXPORT QNetworkAccessManager::NetworkAccessibility QNetworkAccessManager_networkAccessible(QNetworkAccessManagerH handle);

#endif
