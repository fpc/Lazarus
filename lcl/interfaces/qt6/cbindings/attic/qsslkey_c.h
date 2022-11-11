//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QSSLKEY_C_H
#define QSSLKEY_C_H

#include <QtNetwork>
#include "pascalbind.h"

C_EXPORT QSslKeyH QSslKey_Create();
C_EXPORT void QSslKey_Destroy(QSslKeyH handle);
C_EXPORT QSslKeyH QSslKey_Create2(const QByteArrayH encoded, QSsl::KeyAlgorithm algorithm, QSsl::EncodingFormat format, QSsl::KeyType type, const QByteArrayH passPhrase);
C_EXPORT QSslKeyH QSslKey_Create3(QIODeviceH device, QSsl::KeyAlgorithm algorithm, QSsl::EncodingFormat format, QSsl::KeyType type, const QByteArrayH passPhrase);
C_EXPORT QSslKeyH QSslKey_Create4(Qt::HANDLE handle, QSsl::KeyType type);
C_EXPORT QSslKeyH QSslKey_Create5(const QSslKeyH other);
C_EXPORT void QSslKey_swap(QSslKeyH handle, QSslKeyH other);
C_EXPORT bool QSslKey_isNull(QSslKeyH handle);
C_EXPORT void QSslKey_clear(QSslKeyH handle);
C_EXPORT int QSslKey_length(QSslKeyH handle);
C_EXPORT QSsl::KeyType QSslKey_type(QSslKeyH handle);
C_EXPORT QSsl::KeyAlgorithm QSslKey_algorithm(QSslKeyH handle);
C_EXPORT void QSslKey_toPem(QSslKeyH handle, QByteArrayH retval, const QByteArrayH passPhrase);
C_EXPORT void QSslKey_toDer(QSslKeyH handle, QByteArrayH retval, const QByteArrayH passPhrase);
C_EXPORT Qt::HANDLE QSslKey_handle(QSslKeyH handle);

#endif
