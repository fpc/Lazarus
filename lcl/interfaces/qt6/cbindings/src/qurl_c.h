//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QURL_C_H
#define QURL_C_H

#include <QtCore>
#include "pascalbind.h"

C_EXPORT QUrlH QUrl_Create();
C_EXPORT void QUrl_Destroy(QUrlH handle);
C_EXPORT QUrlH QUrl_Create2(const QUrlH copy);
C_EXPORT QUrlH QUrl_Create3(PWideString url, QUrl::ParsingMode mode);
C_EXPORT void QUrl_swap(QUrlH handle, QUrlH other);
C_EXPORT void QUrl_setUrl(QUrlH handle, PWideString url, QUrl::ParsingMode mode);
C_EXPORT void QUrl_url(QUrlH handle, PWideString retval, unsigned int options);
C_EXPORT void QUrl_toString(QUrlH handle, PWideString retval, unsigned int options);
C_EXPORT void QUrl_toDisplayString(QUrlH handle, PWideString retval, unsigned int options);
C_EXPORT void QUrl_toEncoded(QUrlH handle, QByteArrayH retval, unsigned int options);
C_EXPORT void QUrl_fromEncoded(QUrlH retval, const QByteArrayH url, QUrl::ParsingMode mode);
C_EXPORT void QUrl_fromUserInput(QUrlH retval, PWideString userInput);
C_EXPORT bool QUrl_isValid(QUrlH handle);
C_EXPORT void QUrl_errorString(QUrlH handle, PWideString retval);
C_EXPORT bool QUrl_isEmpty(QUrlH handle);
C_EXPORT void QUrl_clear(QUrlH handle);
C_EXPORT void QUrl_setScheme(QUrlH handle, PWideString scheme);
C_EXPORT void QUrl_scheme(QUrlH handle, PWideString retval);
C_EXPORT void QUrl_setAuthority(QUrlH handle, PWideString authority, QUrl::ParsingMode mode);
C_EXPORT void QUrl_authority(QUrlH handle, PWideString retval, unsigned int options);
C_EXPORT void QUrl_setUserInfo(QUrlH handle, PWideString userInfo, QUrl::ParsingMode mode);
C_EXPORT void QUrl_userInfo(QUrlH handle, PWideString retval, unsigned int options);
C_EXPORT void QUrl_setUserName(QUrlH handle, PWideString userName, QUrl::ParsingMode mode);
C_EXPORT void QUrl_userName(QUrlH handle, PWideString retval, unsigned int options);
C_EXPORT void QUrl_setPassword(QUrlH handle, PWideString password, QUrl::ParsingMode mode);
C_EXPORT void QUrl_password(QUrlH handle, PWideString retval, unsigned int AnonParam1);
C_EXPORT void QUrl_setHost(QUrlH handle, PWideString host, QUrl::ParsingMode mode);
C_EXPORT void QUrl_host(QUrlH handle, PWideString retval, unsigned int AnonParam1);
C_EXPORT void QUrl_setPort(QUrlH handle, int port);
C_EXPORT int QUrl_port(QUrlH handle, int defaultPort);
C_EXPORT void QUrl_setPath(QUrlH handle, PWideString path, QUrl::ParsingMode mode);
C_EXPORT void QUrl_path(QUrlH handle, PWideString retval, unsigned int options);
C_EXPORT bool QUrl_hasQuery(QUrlH handle);
C_EXPORT void QUrl_setQuery(QUrlH handle, PWideString query, QUrl::ParsingMode mode);
C_EXPORT void QUrl_setQuery2(QUrlH handle, const QUrlQueryH query);
C_EXPORT void QUrl_query(QUrlH handle, PWideString retval, unsigned int AnonParam1);
C_EXPORT bool QUrl_hasFragment(QUrlH handle);
C_EXPORT void QUrl_fragment(QUrlH handle, PWideString retval, unsigned int options);
C_EXPORT void QUrl_setFragment(QUrlH handle, PWideString fragment, QUrl::ParsingMode mode);
C_EXPORT void QUrl_resolved(QUrlH handle, QUrlH retval, const QUrlH relative);
C_EXPORT bool QUrl_isRelative(QUrlH handle);
C_EXPORT bool QUrl_isParentOf(QUrlH handle, const QUrlH url);
C_EXPORT bool QUrl_isLocalFile(QUrlH handle);
C_EXPORT void QUrl_fromLocalFile(QUrlH retval, PWideString localfile);
C_EXPORT void QUrl_toLocalFile(QUrlH handle, PWideString retval);
C_EXPORT void QUrl_detach(QUrlH handle);
C_EXPORT bool QUrl_isDetached(QUrlH handle);

C_EXPORT void QUrl_adjusted(QUrlH handle, unsigned int options, QUrlH retval);
C_EXPORT void QUrl_fileName(QUrlH handle, unsigned int options, PWideString retval);

C_EXPORT void QUrl_fromPercentEncoding(PWideString retval, const QByteArrayH AnonParam1);
C_EXPORT void QUrl_toPercentEncoding(QByteArrayH retval, PWideString AnonParam1, const QByteArrayH exclude, const QByteArrayH include);
C_EXPORT void QUrl_fromAce(PWideString retval, const QByteArrayH AnonParam1);
C_EXPORT void QUrl_toAce(QByteArrayH retval, PWideString AnonParam1);
C_EXPORT void QUrl_idnWhitelist(QStringListH retval);
C_EXPORT void QUrl_setIdnWhitelist(const QStringListH AnonParam1);

#endif
