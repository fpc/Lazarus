//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QNETWORKREQUEST_C_H
#define QNETWORKREQUEST_C_H

#include <QtNetwork>
#include "pascalbind.h"

C_EXPORT QNetworkRequestH QNetworkRequest_Create(const QUrlH url);
C_EXPORT void QNetworkRequest_Destroy(QNetworkRequestH handle);
C_EXPORT QNetworkRequestH QNetworkRequest_Create2(const QNetworkRequestH other);
C_EXPORT void QNetworkRequest_swap(QNetworkRequestH handle, QNetworkRequestH other);
C_EXPORT void QNetworkRequest_url(QNetworkRequestH handle, QUrlH retval);
C_EXPORT void QNetworkRequest_setUrl(QNetworkRequestH handle, const QUrlH url);
C_EXPORT void QNetworkRequest_header(QNetworkRequestH handle, QVariantH retval, QNetworkRequest::KnownHeaders header);
C_EXPORT void QNetworkRequest_setHeader(QNetworkRequestH handle, QNetworkRequest::KnownHeaders header, const QVariantH value);
C_EXPORT bool QNetworkRequest_hasRawHeader(QNetworkRequestH handle, const QByteArrayH headerName);
C_EXPORT void QNetworkRequest_rawHeader(QNetworkRequestH handle, QByteArrayH retval, const QByteArrayH headerName);
C_EXPORT void QNetworkRequest_setRawHeader(QNetworkRequestH handle, const QByteArrayH headerName, const QByteArrayH value);
C_EXPORT void QNetworkRequest_attribute(QNetworkRequestH handle, QVariantH retval, QNetworkRequest::Attribute code, const QVariantH defaultValue);
C_EXPORT void QNetworkRequest_setAttribute(QNetworkRequestH handle, QNetworkRequest::Attribute code, const QVariantH value);
C_EXPORT void QNetworkRequest_sslConfiguration(QNetworkRequestH handle, QSslConfigurationH retval);
C_EXPORT void QNetworkRequest_setSslConfiguration(QNetworkRequestH handle, const QSslConfigurationH configuration);
C_EXPORT void QNetworkRequest_setOriginatingObject(QNetworkRequestH handle, QObjectH object);
C_EXPORT QObjectH QNetworkRequest_originatingObject(QNetworkRequestH handle);
C_EXPORT QNetworkRequest::Priority QNetworkRequest_priority(QNetworkRequestH handle);
C_EXPORT void QNetworkRequest_setPriority(QNetworkRequestH handle, QNetworkRequest::Priority priority);

#endif
