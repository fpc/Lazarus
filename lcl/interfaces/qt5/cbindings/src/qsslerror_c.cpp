//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qsslerror_c.h"

QSslErrorH QSslError_Create()
{
	return (QSslErrorH) new QSslError();
}

void QSslError_Destroy(QSslErrorH handle)
{
	delete (QSslError *)handle;
}

QSslErrorH QSslError_Create2(QSslError::SslError error)
{
	return (QSslErrorH) new QSslError(error);
}

QSslErrorH QSslError_Create3(QSslError::SslError error, const QSslCertificateH certificate)
{
	return (QSslErrorH) new QSslError(error, *(const QSslCertificate*)certificate);
}

QSslErrorH QSslError_Create4(const QSslErrorH other)
{
	return (QSslErrorH) new QSslError(*(const QSslError*)other);
}

void QSslError_swap(QSslErrorH handle, QSslErrorH other)
{
	((QSslError *)handle)->swap(*(QSslError*)other);
}

QSslError::SslError QSslError_error(QSslErrorH handle)
{
	return (QSslError::SslError) ((QSslError *)handle)->error();
}

void QSslError_errorString(QSslErrorH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QSslError *)handle)->errorString();
	copyQStringToPWideString(t_retval, retval);
}

void QSslError_certificate(QSslErrorH handle, QSslCertificateH retval)
{
	*(QSslCertificate *)retval = ((QSslError *)handle)->certificate();
}

