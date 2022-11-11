//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qwebdatabase_c.h"

QWebDatabaseH QWebDatabase_Create(const QWebDatabaseH other)
{
	return (QWebDatabaseH) new QWebDatabase(*(const QWebDatabase*)other);
}

void QWebDatabase_Destroy(QWebDatabaseH handle)
{
	delete (QWebDatabase *)handle;
}

void QWebDatabase_name(QWebDatabaseH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QWebDatabase *)handle)->name();
	copyQStringToPWideString(t_retval, retval);
}

void QWebDatabase_displayName(QWebDatabaseH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QWebDatabase *)handle)->displayName();
	copyQStringToPWideString(t_retval, retval);
}

qint64 QWebDatabase_expectedSize(QWebDatabaseH handle)
{
	return (qint64) ((QWebDatabase *)handle)->expectedSize();
}

qint64 QWebDatabase_size(QWebDatabaseH handle)
{
	return (qint64) ((QWebDatabase *)handle)->size();
}

void QWebDatabase_fileName(QWebDatabaseH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QWebDatabase *)handle)->fileName();
	copyQStringToPWideString(t_retval, retval);
}

void QWebDatabase_origin(QWebDatabaseH handle, QWebSecurityOriginH retval)
{
	*(QWebSecurityOrigin *)retval = ((QWebDatabase *)handle)->origin();
}

void QWebDatabase_removeDatabase(const QWebDatabaseH AnonParam1)
{
	QWebDatabase::removeDatabase(*(const QWebDatabase*)AnonParam1);
}

void QWebDatabase_removeAllDatabases()
{
	QWebDatabase::removeAllDatabases();
}

