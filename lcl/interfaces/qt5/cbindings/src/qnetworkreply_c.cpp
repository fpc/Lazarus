//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qnetworkreply_c.h"

void QNetworkReply_close(QNetworkReplyH handle)
{
	((QNetworkReply *)handle)->close();
}

bool QNetworkReply_isSequential(QNetworkReplyH handle)
{
	return (bool) ((QNetworkReply *)handle)->isSequential();
}

qint64 QNetworkReply_readBufferSize(QNetworkReplyH handle)
{
	return (qint64) ((QNetworkReply *)handle)->readBufferSize();
}

void QNetworkReply_setReadBufferSize(QNetworkReplyH handle, qint64 size)
{
	((QNetworkReply *)handle)->setReadBufferSize(size);
}

QNetworkAccessManagerH QNetworkReply_manager(QNetworkReplyH handle)
{
	return (QNetworkAccessManagerH) ((QNetworkReply *)handle)->manager();
}

QNetworkAccessManager::Operation QNetworkReply_operation(QNetworkReplyH handle)
{
	return (QNetworkAccessManager::Operation) ((QNetworkReply *)handle)->operation();
}

void QNetworkReply_request(QNetworkReplyH handle, QNetworkRequestH retval)
{
	*(QNetworkRequest *)retval = ((QNetworkReply *)handle)->request();
}

QNetworkReply::NetworkError QNetworkReply_error(QNetworkReplyH handle)
{
	return (QNetworkReply::NetworkError) ((QNetworkReply *)handle)->error();
}

bool QNetworkReply_isFinished(QNetworkReplyH handle)
{
	return (bool) ((QNetworkReply *)handle)->isFinished();
}

bool QNetworkReply_isRunning(QNetworkReplyH handle)
{
	return (bool) ((QNetworkReply *)handle)->isRunning();
}

void QNetworkReply_url(QNetworkReplyH handle, QUrlH retval)
{
	*(QUrl *)retval = ((QNetworkReply *)handle)->url();
}

void QNetworkReply_header(QNetworkReplyH handle, QVariantH retval, QNetworkRequest::KnownHeaders header)
{
	*(QVariant *)retval = ((QNetworkReply *)handle)->header(header);
}

bool QNetworkReply_hasRawHeader(QNetworkReplyH handle, const QByteArrayH headerName)
{
	return (bool) ((QNetworkReply *)handle)->hasRawHeader(*(const QByteArray*)headerName);
}

void QNetworkReply_rawHeader(QNetworkReplyH handle, QByteArrayH retval, const QByteArrayH headerName)
{
	*(QByteArray *)retval = ((QNetworkReply *)handle)->rawHeader(*(const QByteArray*)headerName);
}

void QNetworkReply_attribute(QNetworkReplyH handle, QVariantH retval, QNetworkRequest::Attribute code)
{
	*(QVariant *)retval = ((QNetworkReply *)handle)->attribute(code);
}

void QNetworkReply_sslConfiguration(QNetworkReplyH handle, QSslConfigurationH retval)
{
	*(QSslConfiguration *)retval = ((QNetworkReply *)handle)->sslConfiguration();
}

void QNetworkReply_setSslConfiguration(QNetworkReplyH handle, const QSslConfigurationH configuration)
{
	((QNetworkReply *)handle)->setSslConfiguration(*(const QSslConfiguration*)configuration);
}

void QNetworkReply_abort(QNetworkReplyH handle)
{
	((QNetworkReply *)handle)->abort();
}

void QNetworkReply_ignoreSslErrors(QNetworkReplyH handle)
{
	((QNetworkReply *)handle)->ignoreSslErrors();
}

