//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qnetworkrequest_c.h"

QNetworkRequestH QNetworkRequest_Create(const QUrlH url)
{
	return (QNetworkRequestH) new QNetworkRequest(*(const QUrl*)url);
}

void QNetworkRequest_Destroy(QNetworkRequestH handle)
{
	delete (QNetworkRequest *)handle;
}

QNetworkRequestH QNetworkRequest_Create2(const QNetworkRequestH other)
{
	return (QNetworkRequestH) new QNetworkRequest(*(const QNetworkRequest*)other);
}

void QNetworkRequest_swap(QNetworkRequestH handle, QNetworkRequestH other)
{
	((QNetworkRequest *)handle)->swap(*(QNetworkRequest*)other);
}

void QNetworkRequest_url(QNetworkRequestH handle, QUrlH retval)
{
	*(QUrl *)retval = ((QNetworkRequest *)handle)->url();
}

void QNetworkRequest_setUrl(QNetworkRequestH handle, const QUrlH url)
{
	((QNetworkRequest *)handle)->setUrl(*(const QUrl*)url);
}

void QNetworkRequest_header(QNetworkRequestH handle, QVariantH retval, QNetworkRequest::KnownHeaders header)
{
	*(QVariant *)retval = ((QNetworkRequest *)handle)->header(header);
}

void QNetworkRequest_setHeader(QNetworkRequestH handle, QNetworkRequest::KnownHeaders header, const QVariantH value)
{
	((QNetworkRequest *)handle)->setHeader(header, *(const QVariant*)value);
}

bool QNetworkRequest_hasRawHeader(QNetworkRequestH handle, const QByteArrayH headerName)
{
	return (bool) ((QNetworkRequest *)handle)->hasRawHeader(*(const QByteArray*)headerName);
}

void QNetworkRequest_rawHeader(QNetworkRequestH handle, QByteArrayH retval, const QByteArrayH headerName)
{
	*(QByteArray *)retval = ((QNetworkRequest *)handle)->rawHeader(*(const QByteArray*)headerName);
}

void QNetworkRequest_setRawHeader(QNetworkRequestH handle, const QByteArrayH headerName, const QByteArrayH value)
{
	((QNetworkRequest *)handle)->setRawHeader(*(const QByteArray*)headerName, *(const QByteArray*)value);
}

void QNetworkRequest_attribute(QNetworkRequestH handle, QVariantH retval, QNetworkRequest::Attribute code, const QVariantH defaultValue)
{
	*(QVariant *)retval = ((QNetworkRequest *)handle)->attribute(code, *(const QVariant*)defaultValue);
}

void QNetworkRequest_setAttribute(QNetworkRequestH handle, QNetworkRequest::Attribute code, const QVariantH value)
{
	((QNetworkRequest *)handle)->setAttribute(code, *(const QVariant*)value);
}

void QNetworkRequest_sslConfiguration(QNetworkRequestH handle, QSslConfigurationH retval)
{
	*(QSslConfiguration *)retval = ((QNetworkRequest *)handle)->sslConfiguration();
}

void QNetworkRequest_setSslConfiguration(QNetworkRequestH handle, const QSslConfigurationH configuration)
{
	((QNetworkRequest *)handle)->setSslConfiguration(*(const QSslConfiguration*)configuration);
}

void QNetworkRequest_setOriginatingObject(QNetworkRequestH handle, QObjectH object)
{
	((QNetworkRequest *)handle)->setOriginatingObject((QObject*)object);
}

QObjectH QNetworkRequest_originatingObject(QNetworkRequestH handle)
{
	return (QObjectH) ((QNetworkRequest *)handle)->originatingObject();
}

QNetworkRequest::Priority QNetworkRequest_priority(QNetworkRequestH handle)
{
	return (QNetworkRequest::Priority) ((QNetworkRequest *)handle)->priority();
}

void QNetworkRequest_setPriority(QNetworkRequestH handle, QNetworkRequest::Priority priority)
{
	((QNetworkRequest *)handle)->setPriority(priority);
}

