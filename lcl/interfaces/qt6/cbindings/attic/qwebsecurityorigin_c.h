//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QWEBSECURITYORIGIN_C_H
#define QWEBSECURITYORIGIN_C_H

#include <QtWebKitWidgets>
#include "pascalbind.h"

C_EXPORT void QWebSecurityOrigin_allOrigins(PPtrIntArray retval);
C_EXPORT void QWebSecurityOrigin_addLocalScheme(PWideString scheme);
C_EXPORT void QWebSecurityOrigin_removeLocalScheme(PWideString scheme);
C_EXPORT void QWebSecurityOrigin_localSchemes(QStringListH retval);
C_EXPORT void QWebSecurityOrigin_scheme(QWebSecurityOriginH handle, PWideString retval);
C_EXPORT void QWebSecurityOrigin_host(QWebSecurityOriginH handle, PWideString retval);
C_EXPORT int QWebSecurityOrigin_port(QWebSecurityOriginH handle);
C_EXPORT qint64 QWebSecurityOrigin_databaseUsage(QWebSecurityOriginH handle);
C_EXPORT qint64 QWebSecurityOrigin_databaseQuota(QWebSecurityOriginH handle);
C_EXPORT void QWebSecurityOrigin_setDatabaseQuota(QWebSecurityOriginH handle, qint64 quota);
C_EXPORT void QWebSecurityOrigin_setApplicationCacheQuota(QWebSecurityOriginH handle, qint64 quota);
C_EXPORT void QWebSecurityOrigin_databases(QWebSecurityOriginH handle, PPtrIntArray retval);
C_EXPORT QWebSecurityOriginH QWebSecurityOrigin_Create(const QWebSecurityOriginH other);
C_EXPORT void QWebSecurityOrigin_Destroy(QWebSecurityOriginH handle);

#endif
