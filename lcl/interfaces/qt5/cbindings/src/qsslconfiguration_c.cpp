//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qsslconfiguration_c.h"

QSslConfigurationH QSslConfiguration_Create()
{
	return (QSslConfigurationH) new QSslConfiguration();
}

void QSslConfiguration_Destroy(QSslConfigurationH handle)
{
	delete (QSslConfiguration *)handle;
}

QSslConfigurationH QSslConfiguration_Create2(const QSslConfigurationH other)
{
	return (QSslConfigurationH) new QSslConfiguration(*(const QSslConfiguration*)other);
}

void QSslConfiguration_swap(QSslConfigurationH handle, QSslConfigurationH other)
{
	((QSslConfiguration *)handle)->swap(*(QSslConfiguration*)other);
}

bool QSslConfiguration_isNull(QSslConfigurationH handle)
{
	return (bool) ((QSslConfiguration *)handle)->isNull();
}

QSsl::SslProtocol QSslConfiguration_protocol(QSslConfigurationH handle)
{
	return (QSsl::SslProtocol) ((QSslConfiguration *)handle)->protocol();
}

void QSslConfiguration_setProtocol(QSslConfigurationH handle, QSsl::SslProtocol protocol)
{
	((QSslConfiguration *)handle)->setProtocol(protocol);
}

QSslSocket::PeerVerifyMode QSslConfiguration_peerVerifyMode(QSslConfigurationH handle)
{
	return (QSslSocket::PeerVerifyMode) ((QSslConfiguration *)handle)->peerVerifyMode();
}

void QSslConfiguration_setPeerVerifyMode(QSslConfigurationH handle, QSslSocket::PeerVerifyMode mode)
{
	((QSslConfiguration *)handle)->setPeerVerifyMode(mode);
}

int QSslConfiguration_peerVerifyDepth(QSslConfigurationH handle)
{
	return (int) ((QSslConfiguration *)handle)->peerVerifyDepth();
}

void QSslConfiguration_setPeerVerifyDepth(QSslConfigurationH handle, int depth)
{
	((QSslConfiguration *)handle)->setPeerVerifyDepth(depth);
}

void QSslConfiguration_localCertificate(QSslConfigurationH handle, QSslCertificateH retval)
{
	*(QSslCertificate *)retval = ((QSslConfiguration *)handle)->localCertificate();
}

void QSslConfiguration_setLocalCertificate(QSslConfigurationH handle, const QSslCertificateH certificate)
{
	((QSslConfiguration *)handle)->setLocalCertificate(*(const QSslCertificate*)certificate);
}

void QSslConfiguration_peerCertificate(QSslConfigurationH handle, QSslCertificateH retval)
{
	*(QSslCertificate *)retval = ((QSslConfiguration *)handle)->peerCertificate();
}

void QSslConfiguration_sessionCipher(QSslConfigurationH handle, QSslCipherH retval)
{
	*(QSslCipher *)retval = ((QSslConfiguration *)handle)->sessionCipher();
}

void QSslConfiguration_privateKey(QSslConfigurationH handle, QSslKeyH retval)
{
	*(QSslKey *)retval = ((QSslConfiguration *)handle)->privateKey();
}

void QSslConfiguration_setPrivateKey(QSslConfigurationH handle, const QSslKeyH key)
{
	((QSslConfiguration *)handle)->setPrivateKey(*(const QSslKey*)key);
}

void QSslConfiguration_setSslOption(QSslConfigurationH handle, QSsl::SslOption option, bool on)
{
	((QSslConfiguration *)handle)->setSslOption(option, on);
}

bool QSslConfiguration_testSslOption(QSslConfigurationH handle, QSsl::SslOption option)
{
	return (bool) ((QSslConfiguration *)handle)->testSslOption(option);
}

void QSslConfiguration_defaultConfiguration(QSslConfigurationH retval)
{
	*(QSslConfiguration *)retval = QSslConfiguration::defaultConfiguration();
}

void QSslConfiguration_setDefaultConfiguration(const QSslConfigurationH configuration)
{
	QSslConfiguration::setDefaultConfiguration(*(const QSslConfiguration*)configuration);
}

