//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QNETWORKREPLY_C_H
#define QNETWORKREPLY_C_H

#include <QtNetwork>
#include "pascalbind.h"

C_EXPORT void QNetworkReply_close(QNetworkReplyH handle);
C_EXPORT bool QNetworkReply_isSequential(QNetworkReplyH handle);
C_EXPORT qint64 QNetworkReply_readBufferSize(QNetworkReplyH handle);
C_EXPORT void QNetworkReply_setReadBufferSize(QNetworkReplyH handle, qint64 size);
C_EXPORT QNetworkAccessManagerH QNetworkReply_manager(QNetworkReplyH handle);
C_EXPORT QNetworkAccessManager::Operation QNetworkReply_operation(QNetworkReplyH handle);
C_EXPORT void QNetworkReply_request(QNetworkReplyH handle, QNetworkRequestH retval);
C_EXPORT QNetworkReply::NetworkError QNetworkReply_error(QNetworkReplyH handle);
C_EXPORT bool QNetworkReply_isFinished(QNetworkReplyH handle);
C_EXPORT bool QNetworkReply_isRunning(QNetworkReplyH handle);
C_EXPORT void QNetworkReply_url(QNetworkReplyH handle, QUrlH retval);
C_EXPORT void QNetworkReply_header(QNetworkReplyH handle, QVariantH retval, QNetworkRequest::KnownHeaders header);
C_EXPORT bool QNetworkReply_hasRawHeader(QNetworkReplyH handle, const QByteArrayH headerName);
C_EXPORT void QNetworkReply_rawHeader(QNetworkReplyH handle, QByteArrayH retval, const QByteArrayH headerName);
C_EXPORT void QNetworkReply_attribute(QNetworkReplyH handle, QVariantH retval, QNetworkRequest::Attribute code);
C_EXPORT void QNetworkReply_sslConfiguration(QNetworkReplyH handle, QSslConfigurationH retval);
C_EXPORT void QNetworkReply_setSslConfiguration(QNetworkReplyH handle, const QSslConfigurationH configuration);
C_EXPORT void QNetworkReply_abort(QNetworkReplyH handle);
C_EXPORT void QNetworkReply_ignoreSslErrors(QNetworkReplyH handle);

#endif
