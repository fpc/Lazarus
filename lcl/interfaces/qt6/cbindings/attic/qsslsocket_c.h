//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QSSLSOCKET_C_H
#define QSSLSOCKET_C_H

#include <QtNetwork>
#include "pascalbind.h"

C_EXPORT QSslSocketH QSslSocket_Create(QObjectH parent);
C_EXPORT void QSslSocket_Destroy(QSslSocketH handle);
C_EXPORT void QSslSocket_resume(QSslSocketH handle);
C_EXPORT void QSslSocket_connectToHostEncrypted(QSslSocketH handle, PWideString hostName, quint16 port, unsigned int mode, QAbstractSocket::NetworkLayerProtocol protocol);
C_EXPORT void QSslSocket_connectToHostEncrypted2(QSslSocketH handle, PWideString hostName, quint16 port, PWideString sslPeerName, unsigned int mode, QAbstractSocket::NetworkLayerProtocol protocol);
C_EXPORT void QSslSocket_connectToHost(QSslSocketH handle, PWideString hostName, quint16 port, unsigned int openMode, QAbstractSocket::NetworkLayerProtocol protocol);
C_EXPORT void QSslSocket_disconnectFromHost(QSslSocketH handle);
C_EXPORT void QSslSocket_setSocketOption(QSslSocketH handle, QAbstractSocket::SocketOption option, const QVariantH value);
C_EXPORT void QSslSocket_socketOption(QSslSocketH handle, QVariantH retval, QAbstractSocket::SocketOption option);
C_EXPORT QSslSocket::SslMode QSslSocket_mode(QSslSocketH handle);
C_EXPORT bool QSslSocket_isEncrypted(QSslSocketH handle);
C_EXPORT QSsl::SslProtocol QSslSocket_protocol(QSslSocketH handle);
C_EXPORT void QSslSocket_setProtocol(QSslSocketH handle, QSsl::SslProtocol protocol);
C_EXPORT QSslSocket::PeerVerifyMode QSslSocket_peerVerifyMode(QSslSocketH handle);
C_EXPORT void QSslSocket_setPeerVerifyMode(QSslSocketH handle, QSslSocket::PeerVerifyMode mode);
C_EXPORT int QSslSocket_peerVerifyDepth(QSslSocketH handle);
C_EXPORT void QSslSocket_setPeerVerifyDepth(QSslSocketH handle, int depth);
C_EXPORT void QSslSocket_peerVerifyName(QSslSocketH handle, PWideString retval);
C_EXPORT void QSslSocket_setPeerVerifyName(QSslSocketH handle, PWideString hostName);
C_EXPORT qint64 QSslSocket_bytesAvailable(QSslSocketH handle);
C_EXPORT qint64 QSslSocket_bytesToWrite(QSslSocketH handle);
C_EXPORT bool QSslSocket_canReadLine(QSslSocketH handle);
C_EXPORT void QSslSocket_close(QSslSocketH handle);
C_EXPORT bool QSslSocket_atEnd(QSslSocketH handle);
C_EXPORT bool QSslSocket_flush(QSslSocketH handle);
C_EXPORT void QSslSocket_abort(QSslSocketH handle);
C_EXPORT void QSslSocket_setReadBufferSize(QSslSocketH handle, qint64 size);
C_EXPORT qint64 QSslSocket_encryptedBytesAvailable(QSslSocketH handle);
C_EXPORT qint64 QSslSocket_encryptedBytesToWrite(QSslSocketH handle);
C_EXPORT void QSslSocket_sslConfiguration(QSslSocketH handle, QSslConfigurationH retval);
C_EXPORT void QSslSocket_setSslConfiguration(QSslSocketH handle, const QSslConfigurationH config);
C_EXPORT void QSslSocket_setLocalCertificate(QSslSocketH handle, const QSslCertificateH certificate);
C_EXPORT void QSslSocket_setLocalCertificate2(QSslSocketH handle, PWideString fileName, QSsl::EncodingFormat format);
C_EXPORT void QSslSocket_localCertificate(QSslSocketH handle, QSslCertificateH retval);
C_EXPORT void QSslSocket_peerCertificate(QSslSocketH handle, QSslCertificateH retval);
C_EXPORT void QSslSocket_sessionCipher(QSslSocketH handle, QSslCipherH retval);
C_EXPORT void QSslSocket_setPrivateKey(QSslSocketH handle, const QSslKeyH key);
C_EXPORT void QSslSocket_setPrivateKey2(QSslSocketH handle, PWideString fileName, QSsl::KeyAlgorithm algorithm, QSsl::EncodingFormat format, const QByteArrayH passPhrase);
C_EXPORT void QSslSocket_privateKey(QSslSocketH handle, QSslKeyH retval);
C_EXPORT void QSslSocket_setCiphers(QSslSocketH handle, PWideString ciphers);
C_EXPORT void QSslSocket_addCaCertificate(QSslSocketH handle, const QSslCertificateH certificate);
C_EXPORT void QSslSocket_addDefaultCaCertificate(const QSslCertificateH certificate);
C_EXPORT bool QSslSocket_waitForConnected(QSslSocketH handle, int msecs);
C_EXPORT bool QSslSocket_waitForEncrypted(QSslSocketH handle, int msecs);
C_EXPORT bool QSslSocket_waitForReadyRead(QSslSocketH handle, int msecs);
C_EXPORT bool QSslSocket_waitForBytesWritten(QSslSocketH handle, int msecs);
C_EXPORT bool QSslSocket_waitForDisconnected(QSslSocketH handle, int msecs);
C_EXPORT bool QSslSocket_supportsSsl();
C_EXPORT long QSslSocket_sslLibraryVersionNumber();
C_EXPORT void QSslSocket_sslLibraryVersionString(PWideString retval);
C_EXPORT void QSslSocket_startClientEncryption(QSslSocketH handle);
C_EXPORT void QSslSocket_startServerEncryption(QSslSocketH handle);
C_EXPORT void QSslSocket_ignoreSslErrors(QSslSocketH handle);

#endif
