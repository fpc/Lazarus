//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QNETWORKCOOKIEJAR_C_H
#define QNETWORKCOOKIEJAR_C_H

#include <QtNetwork>
#include "pascalbind.h"

C_EXPORT QNetworkCookieJarH QNetworkCookieJar_Create(QObjectH parent);
C_EXPORT void QNetworkCookieJar_Destroy(QNetworkCookieJarH handle);
C_EXPORT bool QNetworkCookieJar_insertCookie(QNetworkCookieJarH handle, const QNetworkCookieH cookie);
C_EXPORT bool QNetworkCookieJar_updateCookie(QNetworkCookieJarH handle, const QNetworkCookieH cookie);
C_EXPORT bool QNetworkCookieJar_deleteCookie(QNetworkCookieJarH handle, const QNetworkCookieH cookie);

#endif
