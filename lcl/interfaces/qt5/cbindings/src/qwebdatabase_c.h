//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QWEBDATABASE_C_H
#define QWEBDATABASE_C_H

#include <QtWebKitWidgets>
#include "pascalbind.h"

C_EXPORT QWebDatabaseH QWebDatabase_Create(const QWebDatabaseH other);
C_EXPORT void QWebDatabase_Destroy(QWebDatabaseH handle);
C_EXPORT void QWebDatabase_name(QWebDatabaseH handle, PWideString retval);
C_EXPORT void QWebDatabase_displayName(QWebDatabaseH handle, PWideString retval);
C_EXPORT qint64 QWebDatabase_expectedSize(QWebDatabaseH handle);
C_EXPORT qint64 QWebDatabase_size(QWebDatabaseH handle);
C_EXPORT void QWebDatabase_fileName(QWebDatabaseH handle, PWideString retval);
C_EXPORT void QWebDatabase_origin(QWebDatabaseH handle, QWebSecurityOriginH retval);
C_EXPORT void QWebDatabase_removeDatabase(const QWebDatabaseH AnonParam1);
C_EXPORT void QWebDatabase_removeAllDatabases();

#endif
