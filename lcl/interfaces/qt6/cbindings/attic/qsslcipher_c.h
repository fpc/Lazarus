//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QSSLCIPHER_C_H
#define QSSLCIPHER_C_H

#include <QtNetwork>
#include "pascalbind.h"

C_EXPORT QSslCipherH QSslCipher_Create();
C_EXPORT void QSslCipher_Destroy(QSslCipherH handle);
C_EXPORT QSslCipherH QSslCipher_Create2(PWideString name, QSsl::SslProtocol protocol);
C_EXPORT QSslCipherH QSslCipher_Create3(const QSslCipherH other);
C_EXPORT void QSslCipher_swap(QSslCipherH handle, QSslCipherH other);
C_EXPORT bool QSslCipher_isNull(QSslCipherH handle);
C_EXPORT void QSslCipher_name(QSslCipherH handle, PWideString retval);
C_EXPORT int QSslCipher_supportedBits(QSslCipherH handle);
C_EXPORT int QSslCipher_usedBits(QSslCipherH handle);
C_EXPORT void QSslCipher_keyExchangeMethod(QSslCipherH handle, PWideString retval);
C_EXPORT void QSslCipher_authenticationMethod(QSslCipherH handle, PWideString retval);
C_EXPORT void QSslCipher_encryptionMethod(QSslCipherH handle, PWideString retval);
C_EXPORT void QSslCipher_protocolString(QSslCipherH handle, PWideString retval);
C_EXPORT QSsl::SslProtocol QSslCipher_protocol(QSslCipherH handle);

#endif
