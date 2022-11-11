//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qsslcipher_c.h"

QSslCipherH QSslCipher_Create()
{
	return (QSslCipherH) new QSslCipher();
}

void QSslCipher_Destroy(QSslCipherH handle)
{
	delete (QSslCipher *)handle;
}

QSslCipherH QSslCipher_Create2(PWideString name, QSsl::SslProtocol protocol)
{
	QString t_name;
	copyPWideStringToQString(name, t_name);
	return (QSslCipherH) new QSslCipher(t_name, protocol);
}

QSslCipherH QSslCipher_Create3(const QSslCipherH other)
{
	return (QSslCipherH) new QSslCipher(*(const QSslCipher*)other);
}

void QSslCipher_swap(QSslCipherH handle, QSslCipherH other)
{
	((QSslCipher *)handle)->swap(*(QSslCipher*)other);
}

bool QSslCipher_isNull(QSslCipherH handle)
{
	return (bool) ((QSslCipher *)handle)->isNull();
}

void QSslCipher_name(QSslCipherH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QSslCipher *)handle)->name();
	copyQStringToPWideString(t_retval, retval);
}

int QSslCipher_supportedBits(QSslCipherH handle)
{
	return (int) ((QSslCipher *)handle)->supportedBits();
}

int QSslCipher_usedBits(QSslCipherH handle)
{
	return (int) ((QSslCipher *)handle)->usedBits();
}

void QSslCipher_keyExchangeMethod(QSslCipherH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QSslCipher *)handle)->keyExchangeMethod();
	copyQStringToPWideString(t_retval, retval);
}

void QSslCipher_authenticationMethod(QSslCipherH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QSslCipher *)handle)->authenticationMethod();
	copyQStringToPWideString(t_retval, retval);
}

void QSslCipher_encryptionMethod(QSslCipherH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QSslCipher *)handle)->encryptionMethod();
	copyQStringToPWideString(t_retval, retval);
}

void QSslCipher_protocolString(QSslCipherH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QSslCipher *)handle)->protocolString();
	copyQStringToPWideString(t_retval, retval);
}

QSsl::SslProtocol QSslCipher_protocol(QSslCipherH handle)
{
	return (QSsl::SslProtocol) ((QSslCipher *)handle)->protocol();
}

