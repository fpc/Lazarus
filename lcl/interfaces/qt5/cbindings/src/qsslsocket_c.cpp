//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qsslsocket_c.h"

QSslSocketH QSslSocket_Create(QObjectH parent)
{
	return (QSslSocketH) new QSslSocket((QObject*)parent);
}

void QSslSocket_Destroy(QSslSocketH handle)
{
	delete (QSslSocket *)handle;
}

void QSslSocket_resume(QSslSocketH handle)
{
	((QSslSocket *)handle)->resume();
}

void QSslSocket_connectToHostEncrypted(QSslSocketH handle, PWideString hostName, quint16 port, unsigned int mode, QAbstractSocket::NetworkLayerProtocol protocol)
{
	QString t_hostName;
	copyPWideStringToQString(hostName, t_hostName);
	((QSslSocket *)handle)->connectToHostEncrypted(t_hostName, port, (QIODevice::OpenMode)mode, protocol);
}

void QSslSocket_connectToHostEncrypted2(QSslSocketH handle, PWideString hostName, quint16 port, PWideString sslPeerName, unsigned int mode, QAbstractSocket::NetworkLayerProtocol protocol)
{
	QString t_hostName;
	QString t_sslPeerName;
	copyPWideStringToQString(hostName, t_hostName);
	copyPWideStringToQString(sslPeerName, t_sslPeerName);
	((QSslSocket *)handle)->connectToHostEncrypted(t_hostName, port, t_sslPeerName, (QIODevice::OpenMode)mode, protocol);
}

void QSslSocket_connectToHost(QSslSocketH handle, PWideString hostName, quint16 port, unsigned int openMode, QAbstractSocket::NetworkLayerProtocol protocol)
{
	QString t_hostName;
	copyPWideStringToQString(hostName, t_hostName);
	((QSslSocket *)handle)->connectToHost(t_hostName, port, (QIODevice::OpenMode)openMode, protocol);
}

void QSslSocket_disconnectFromHost(QSslSocketH handle)
{
	((QSslSocket *)handle)->disconnectFromHost();
}

void QSslSocket_setSocketOption(QSslSocketH handle, QAbstractSocket::SocketOption option, const QVariantH value)
{
	((QSslSocket *)handle)->setSocketOption(option, *(const QVariant*)value);
}

void QSslSocket_socketOption(QSslSocketH handle, QVariantH retval, QAbstractSocket::SocketOption option)
{
	*(QVariant *)retval = ((QSslSocket *)handle)->socketOption(option);
}

QSslSocket::SslMode QSslSocket_mode(QSslSocketH handle)
{
	return (QSslSocket::SslMode) ((QSslSocket *)handle)->mode();
}

bool QSslSocket_isEncrypted(QSslSocketH handle)
{
	return (bool) ((QSslSocket *)handle)->isEncrypted();
}

QSsl::SslProtocol QSslSocket_protocol(QSslSocketH handle)
{
	return (QSsl::SslProtocol) ((QSslSocket *)handle)->protocol();
}

void QSslSocket_setProtocol(QSslSocketH handle, QSsl::SslProtocol protocol)
{
	((QSslSocket *)handle)->setProtocol(protocol);
}

QSslSocket::PeerVerifyMode QSslSocket_peerVerifyMode(QSslSocketH handle)
{
	return (QSslSocket::PeerVerifyMode) ((QSslSocket *)handle)->peerVerifyMode();
}

void QSslSocket_setPeerVerifyMode(QSslSocketH handle, QSslSocket::PeerVerifyMode mode)
{
	((QSslSocket *)handle)->setPeerVerifyMode(mode);
}

int QSslSocket_peerVerifyDepth(QSslSocketH handle)
{
	return (int) ((QSslSocket *)handle)->peerVerifyDepth();
}

void QSslSocket_setPeerVerifyDepth(QSslSocketH handle, int depth)
{
	((QSslSocket *)handle)->setPeerVerifyDepth(depth);
}

void QSslSocket_peerVerifyName(QSslSocketH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QSslSocket *)handle)->peerVerifyName();
	copyQStringToPWideString(t_retval, retval);
}

void QSslSocket_setPeerVerifyName(QSslSocketH handle, PWideString hostName)
{
	QString t_hostName;
	copyPWideStringToQString(hostName, t_hostName);
	((QSslSocket *)handle)->setPeerVerifyName(t_hostName);
}

qint64 QSslSocket_bytesAvailable(QSslSocketH handle)
{
	return (qint64) ((QSslSocket *)handle)->bytesAvailable();
}

qint64 QSslSocket_bytesToWrite(QSslSocketH handle)
{
	return (qint64) ((QSslSocket *)handle)->bytesToWrite();
}

bool QSslSocket_canReadLine(QSslSocketH handle)
{
	return (bool) ((QSslSocket *)handle)->canReadLine();
}

void QSslSocket_close(QSslSocketH handle)
{
	((QSslSocket *)handle)->close();
}

bool QSslSocket_atEnd(QSslSocketH handle)
{
	return (bool) ((QSslSocket *)handle)->atEnd();
}

bool QSslSocket_flush(QSslSocketH handle)
{
	return (bool) ((QSslSocket *)handle)->flush();
}

void QSslSocket_abort(QSslSocketH handle)
{
	((QSslSocket *)handle)->abort();
}

void QSslSocket_setReadBufferSize(QSslSocketH handle, qint64 size)
{
	((QSslSocket *)handle)->setReadBufferSize(size);
}

qint64 QSslSocket_encryptedBytesAvailable(QSslSocketH handle)
{
	return (qint64) ((QSslSocket *)handle)->encryptedBytesAvailable();
}

qint64 QSslSocket_encryptedBytesToWrite(QSslSocketH handle)
{
	return (qint64) ((QSslSocket *)handle)->encryptedBytesToWrite();
}

void QSslSocket_sslConfiguration(QSslSocketH handle, QSslConfigurationH retval)
{
	*(QSslConfiguration *)retval = ((QSslSocket *)handle)->sslConfiguration();
}

void QSslSocket_setSslConfiguration(QSslSocketH handle, const QSslConfigurationH config)
{
	((QSslSocket *)handle)->setSslConfiguration(*(const QSslConfiguration*)config);
}

void QSslSocket_setLocalCertificate(QSslSocketH handle, const QSslCertificateH certificate)
{
	((QSslSocket *)handle)->setLocalCertificate(*(const QSslCertificate*)certificate);
}

void QSslSocket_setLocalCertificate2(QSslSocketH handle, PWideString fileName, QSsl::EncodingFormat format)
{
	QString t_fileName;
	copyPWideStringToQString(fileName, t_fileName);
	((QSslSocket *)handle)->setLocalCertificate(t_fileName, format);
}

void QSslSocket_localCertificate(QSslSocketH handle, QSslCertificateH retval)
{
	*(QSslCertificate *)retval = ((QSslSocket *)handle)->localCertificate();
}

void QSslSocket_peerCertificate(QSslSocketH handle, QSslCertificateH retval)
{
	*(QSslCertificate *)retval = ((QSslSocket *)handle)->peerCertificate();
}

void QSslSocket_sessionCipher(QSslSocketH handle, QSslCipherH retval)
{
	*(QSslCipher *)retval = ((QSslSocket *)handle)->sessionCipher();
}

void QSslSocket_setPrivateKey(QSslSocketH handle, const QSslKeyH key)
{
	((QSslSocket *)handle)->setPrivateKey(*(const QSslKey*)key);
}

void QSslSocket_setPrivateKey2(QSslSocketH handle, PWideString fileName, QSsl::KeyAlgorithm algorithm, QSsl::EncodingFormat format, const QByteArrayH passPhrase)
{
	QString t_fileName;
	copyPWideStringToQString(fileName, t_fileName);
	((QSslSocket *)handle)->setPrivateKey(t_fileName, algorithm, format, *(const QByteArray*)passPhrase);
}

void QSslSocket_privateKey(QSslSocketH handle, QSslKeyH retval)
{
	*(QSslKey *)retval = ((QSslSocket *)handle)->privateKey();
}

void QSslSocket_setCiphers(QSslSocketH handle, PWideString ciphers)
{
	QString t_ciphers;
	copyPWideStringToQString(ciphers, t_ciphers);
	((QSslSocket *)handle)->setCiphers(t_ciphers);
}

void QSslSocket_addCaCertificate(QSslSocketH handle, const QSslCertificateH certificate)
{
	((QSslSocket *)handle)->addCaCertificate(*(const QSslCertificate*)certificate);
}

void QSslSocket_addDefaultCaCertificate(const QSslCertificateH certificate)
{
	QSslSocket::addDefaultCaCertificate(*(const QSslCertificate*)certificate);
}

bool QSslSocket_waitForConnected(QSslSocketH handle, int msecs)
{
	return (bool) ((QSslSocket *)handle)->waitForConnected(msecs);
}

bool QSslSocket_waitForEncrypted(QSslSocketH handle, int msecs)
{
	return (bool) ((QSslSocket *)handle)->waitForEncrypted(msecs);
}

bool QSslSocket_waitForReadyRead(QSslSocketH handle, int msecs)
{
	return (bool) ((QSslSocket *)handle)->waitForReadyRead(msecs);
}

bool QSslSocket_waitForBytesWritten(QSslSocketH handle, int msecs)
{
	return (bool) ((QSslSocket *)handle)->waitForBytesWritten(msecs);
}

bool QSslSocket_waitForDisconnected(QSslSocketH handle, int msecs)
{
	return (bool) ((QSslSocket *)handle)->waitForDisconnected(msecs);
}

bool QSslSocket_supportsSsl()
{
	return (bool) QSslSocket::supportsSsl();
}

long QSslSocket_sslLibraryVersionNumber()
{
	return (long) QSslSocket::sslLibraryVersionNumber();
}

void QSslSocket_sslLibraryVersionString(PWideString retval)
{
	QString t_retval;
	t_retval = QSslSocket::sslLibraryVersionString();
	copyQStringToPWideString(t_retval, retval);
}

void QSslSocket_startClientEncryption(QSslSocketH handle)
{
	((QSslSocket *)handle)->startClientEncryption();
}

void QSslSocket_startServerEncryption(QSslSocketH handle)
{
	((QSslSocket *)handle)->startServerEncryption();
}

void QSslSocket_ignoreSslErrors(QSslSocketH handle)
{
	((QSslSocket *)handle)->ignoreSslErrors();
}

