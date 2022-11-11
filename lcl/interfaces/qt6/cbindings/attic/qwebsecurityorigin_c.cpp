//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qwebsecurityorigin_c.h"

void QWebSecurityOrigin_allOrigins(PPtrIntArray retval)
{
	QList<QWebSecurityOrigin> t_retval;
	t_retval = QWebSecurityOrigin::allOrigins();
	copyQListTemplateToPtrIntArrayWithNew(t_retval, retval);
}

void QWebSecurityOrigin_addLocalScheme(PWideString scheme)
{
	QString t_scheme;
	copyPWideStringToQString(scheme, t_scheme);
	QWebSecurityOrigin::addLocalScheme(t_scheme);
}

void QWebSecurityOrigin_removeLocalScheme(PWideString scheme)
{
	QString t_scheme;
	copyPWideStringToQString(scheme, t_scheme);
	QWebSecurityOrigin::removeLocalScheme(t_scheme);
}

void QWebSecurityOrigin_localSchemes(QStringListH retval)
{
	*(QStringList *)retval = QWebSecurityOrigin::localSchemes();
}

void QWebSecurityOrigin_scheme(QWebSecurityOriginH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QWebSecurityOrigin *)handle)->scheme();
	copyQStringToPWideString(t_retval, retval);
}

void QWebSecurityOrigin_host(QWebSecurityOriginH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QWebSecurityOrigin *)handle)->host();
	copyQStringToPWideString(t_retval, retval);
}

int QWebSecurityOrigin_port(QWebSecurityOriginH handle)
{
	return (int) ((QWebSecurityOrigin *)handle)->port();
}

qint64 QWebSecurityOrigin_databaseUsage(QWebSecurityOriginH handle)
{
	return (qint64) ((QWebSecurityOrigin *)handle)->databaseUsage();
}

qint64 QWebSecurityOrigin_databaseQuota(QWebSecurityOriginH handle)
{
	return (qint64) ((QWebSecurityOrigin *)handle)->databaseQuota();
}

void QWebSecurityOrigin_setDatabaseQuota(QWebSecurityOriginH handle, qint64 quota)
{
	((QWebSecurityOrigin *)handle)->setDatabaseQuota(quota);
}

void QWebSecurityOrigin_setApplicationCacheQuota(QWebSecurityOriginH handle, qint64 quota)
{
	((QWebSecurityOrigin *)handle)->setApplicationCacheQuota(quota);
}

void QWebSecurityOrigin_databases(QWebSecurityOriginH handle, PPtrIntArray retval)
{
	QList<QWebDatabase> t_retval;
	t_retval = ((QWebSecurityOrigin *)handle)->databases();
	copyQListTemplateToPtrIntArrayWithNew(t_retval, retval);
}

QWebSecurityOriginH QWebSecurityOrigin_Create(const QWebSecurityOriginH other)
{
	return (QWebSecurityOriginH) new QWebSecurityOrigin(*(const QWebSecurityOrigin*)other);
}

void QWebSecurityOrigin_Destroy(QWebSecurityOriginH handle)
{
	delete (QWebSecurityOrigin *)handle;
}

