//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QSSLERROR_C_H
#define QSSLERROR_C_H

#include <QtNetwork>
#include "pascalbind.h"

C_EXPORT QSslErrorH QSslError_Create();
C_EXPORT void QSslError_Destroy(QSslErrorH handle);
C_EXPORT QSslErrorH QSslError_Create2(QSslError::SslError error);
C_EXPORT QSslErrorH QSslError_Create3(QSslError::SslError error, const QSslCertificateH certificate);
C_EXPORT QSslErrorH QSslError_Create4(const QSslErrorH other);
C_EXPORT void QSslError_swap(QSslErrorH handle, QSslErrorH other);
C_EXPORT QSslError::SslError QSslError_error(QSslErrorH handle);
C_EXPORT void QSslError_errorString(QSslErrorH handle, PWideString retval);
C_EXPORT void QSslError_certificate(QSslErrorH handle, QSslCertificateH retval);

#endif
