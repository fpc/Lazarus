//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QSSLCONFIGURATION_C_H
#define QSSLCONFIGURATION_C_H

#include <QtNetwork>
#include "pascalbind.h"

C_EXPORT QSslConfigurationH QSslConfiguration_Create();
C_EXPORT void QSslConfiguration_Destroy(QSslConfigurationH handle);
C_EXPORT QSslConfigurationH QSslConfiguration_Create2(const QSslConfigurationH other);
C_EXPORT void QSslConfiguration_swap(QSslConfigurationH handle, QSslConfigurationH other);
C_EXPORT bool QSslConfiguration_isNull(QSslConfigurationH handle);
C_EXPORT QSsl::SslProtocol QSslConfiguration_protocol(QSslConfigurationH handle);
C_EXPORT void QSslConfiguration_setProtocol(QSslConfigurationH handle, QSsl::SslProtocol protocol);
C_EXPORT QSslSocket::PeerVerifyMode QSslConfiguration_peerVerifyMode(QSslConfigurationH handle);
C_EXPORT void QSslConfiguration_setPeerVerifyMode(QSslConfigurationH handle, QSslSocket::PeerVerifyMode mode);
C_EXPORT int QSslConfiguration_peerVerifyDepth(QSslConfigurationH handle);
C_EXPORT void QSslConfiguration_setPeerVerifyDepth(QSslConfigurationH handle, int depth);
C_EXPORT void QSslConfiguration_localCertificate(QSslConfigurationH handle, QSslCertificateH retval);
C_EXPORT void QSslConfiguration_setLocalCertificate(QSslConfigurationH handle, const QSslCertificateH certificate);
C_EXPORT void QSslConfiguration_peerCertificate(QSslConfigurationH handle, QSslCertificateH retval);
C_EXPORT void QSslConfiguration_sessionCipher(QSslConfigurationH handle, QSslCipherH retval);
C_EXPORT void QSslConfiguration_privateKey(QSslConfigurationH handle, QSslKeyH retval);
C_EXPORT void QSslConfiguration_setPrivateKey(QSslConfigurationH handle, const QSslKeyH key);
C_EXPORT void QSslConfiguration_setSslOption(QSslConfigurationH handle, QSsl::SslOption option, bool on);
C_EXPORT bool QSslConfiguration_testSslOption(QSslConfigurationH handle, QSsl::SslOption option);
C_EXPORT void QSslConfiguration_defaultConfiguration(QSslConfigurationH retval);
C_EXPORT void QSslConfiguration_setDefaultConfiguration(const QSslConfigurationH configuration);

#endif
