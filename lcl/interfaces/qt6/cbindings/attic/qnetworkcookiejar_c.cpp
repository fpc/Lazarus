//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qnetworkcookiejar_c.h"

QNetworkCookieJarH QNetworkCookieJar_Create(QObjectH parent)
{
	return (QNetworkCookieJarH) new QNetworkCookieJar((QObject*)parent);
}

void QNetworkCookieJar_Destroy(QNetworkCookieJarH handle)
{
	delete (QNetworkCookieJar *)handle;
}

bool QNetworkCookieJar_insertCookie(QNetworkCookieJarH handle, const QNetworkCookieH cookie)
{
	return (bool) ((QNetworkCookieJar *)handle)->insertCookie(*(const QNetworkCookie*)cookie);
}

bool QNetworkCookieJar_updateCookie(QNetworkCookieJarH handle, const QNetworkCookieH cookie)
{
	return (bool) ((QNetworkCookieJar *)handle)->updateCookie(*(const QNetworkCookie*)cookie);
}

bool QNetworkCookieJar_deleteCookie(QNetworkCookieJarH handle, const QNetworkCookieH cookie)
{
	return (bool) ((QNetworkCookieJar *)handle)->deleteCookie(*(const QNetworkCookie*)cookie);
}

