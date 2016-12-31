//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qsslkey_c.h"

QSslKeyH QSslKey_Create()
{
	return (QSslKeyH) new QSslKey();
}

void QSslKey_Destroy(QSslKeyH handle)
{
	delete (QSslKey *)handle;
}

QSslKeyH QSslKey_Create2(const QByteArrayH encoded, QSsl::KeyAlgorithm algorithm, QSsl::EncodingFormat format, QSsl::KeyType type, const QByteArrayH passPhrase)
{
	return (QSslKeyH) new QSslKey(*(const QByteArray*)encoded, algorithm, format, type, *(const QByteArray*)passPhrase);
}

QSslKeyH QSslKey_Create3(QIODeviceH device, QSsl::KeyAlgorithm algorithm, QSsl::EncodingFormat format, QSsl::KeyType type, const QByteArrayH passPhrase)
{
	return (QSslKeyH) new QSslKey((QIODevice*)device, algorithm, format, type, *(const QByteArray*)passPhrase);
}

QSslKeyH QSslKey_Create4(Qt::HANDLE handle, QSsl::KeyType type)
{
	return (QSslKeyH) new QSslKey(handle, type);
}

QSslKeyH QSslKey_Create5(const QSslKeyH other)
{
	return (QSslKeyH) new QSslKey(*(const QSslKey*)other);
}

void QSslKey_swap(QSslKeyH handle, QSslKeyH other)
{
	((QSslKey *)handle)->swap(*(QSslKey*)other);
}

bool QSslKey_isNull(QSslKeyH handle)
{
	return (bool) ((QSslKey *)handle)->isNull();
}

void QSslKey_clear(QSslKeyH handle)
{
	((QSslKey *)handle)->clear();
}

int QSslKey_length(QSslKeyH handle)
{
	return (int) ((QSslKey *)handle)->length();
}

QSsl::KeyType QSslKey_type(QSslKeyH handle)
{
	return (QSsl::KeyType) ((QSslKey *)handle)->type();
}

QSsl::KeyAlgorithm QSslKey_algorithm(QSslKeyH handle)
{
	return (QSsl::KeyAlgorithm) ((QSslKey *)handle)->algorithm();
}

void QSslKey_toPem(QSslKeyH handle, QByteArrayH retval, const QByteArrayH passPhrase)
{
	*(QByteArray *)retval = ((QSslKey *)handle)->toPem(*(const QByteArray*)passPhrase);
}

void QSslKey_toDer(QSslKeyH handle, QByteArrayH retval, const QByteArrayH passPhrase)
{
	*(QByteArray *)retval = ((QSslKey *)handle)->toDer(*(const QByteArray*)passPhrase);
}

Qt::HANDLE QSslKey_handle(QSslKeyH handle)
{
	return (Qt::HANDLE) ((QSslKey *)handle)->handle();
}

