//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qurl_c.h"

QUrlH QUrl_Create()
{
	return (QUrlH) new QUrl();
}

void QUrl_Destroy(QUrlH handle)
{
	delete (QUrl *)handle;
}

QUrlH QUrl_Create2(const QUrlH copy)
{
	return (QUrlH) new QUrl(*(const QUrl*)copy);
}

QUrlH QUrl_Create3(PWideString url, QUrl::ParsingMode mode)
{
	QString t_url;
	copyPWideStringToQString(url, t_url);
	return (QUrlH) new QUrl(t_url, mode);
}

void QUrl_swap(QUrlH handle, QUrlH other)
{
	((QUrl *)handle)->swap(*(QUrl*)other);
}

void QUrl_setUrl(QUrlH handle, PWideString url, QUrl::ParsingMode mode)
{
	QString t_url;
	copyPWideStringToQString(url, t_url);
	((QUrl *)handle)->setUrl(t_url, mode);
}

void QUrl_url(QUrlH handle, PWideString retval, unsigned int options)
{
	QString t_retval;
	t_retval = ((QUrl *)handle)->url((QUrl::FormattingOptions)options);
	copyQStringToPWideString(t_retval, retval);
}

void QUrl_toString(QUrlH handle, PWideString retval, unsigned int options)
{
	QString t_retval;
	t_retval = ((QUrl *)handle)->toString((QUrl::FormattingOptions)options);
	copyQStringToPWideString(t_retval, retval);
}

void QUrl_toDisplayString(QUrlH handle, PWideString retval, unsigned int options)
{
	QString t_retval;
	t_retval = ((QUrl *)handle)->toDisplayString((QUrl::FormattingOptions)options);
	copyQStringToPWideString(t_retval, retval);
}

void QUrl_toEncoded(QUrlH handle, QByteArrayH retval, unsigned int options)
{
	*(QByteArray *)retval = ((QUrl *)handle)->toEncoded((QUrl::FormattingOptions)options);
}

void QUrl_fromEncoded(QUrlH retval, const QByteArrayH url, QUrl::ParsingMode mode)
{
	*(QUrl *)retval = QUrl::fromEncoded(*(const QByteArray*)url, mode);
}

void QUrl_fromUserInput(QUrlH retval, PWideString userInput)
{
	QString t_userInput;
	copyPWideStringToQString(userInput, t_userInput);
	*(QUrl *)retval = QUrl::fromUserInput(t_userInput);
}

bool QUrl_isValid(QUrlH handle)
{
	return (bool) ((QUrl *)handle)->isValid();
}

void QUrl_errorString(QUrlH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QUrl *)handle)->errorString();
	copyQStringToPWideString(t_retval, retval);
}

bool QUrl_isEmpty(QUrlH handle)
{
	return (bool) ((QUrl *)handle)->isEmpty();
}

void QUrl_clear(QUrlH handle)
{
	((QUrl *)handle)->clear();
}

void QUrl_setScheme(QUrlH handle, PWideString scheme)
{
	QString t_scheme;
	copyPWideStringToQString(scheme, t_scheme);
	((QUrl *)handle)->setScheme(t_scheme);
}

void QUrl_scheme(QUrlH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QUrl *)handle)->scheme();
	copyQStringToPWideString(t_retval, retval);
}

void QUrl_setAuthority(QUrlH handle, PWideString authority, QUrl::ParsingMode mode)
{
	QString t_authority;
	copyPWideStringToQString(authority, t_authority);
	((QUrl *)handle)->setAuthority(t_authority, mode);
}

void QUrl_authority(QUrlH handle, PWideString retval, unsigned int options)
{
	QString t_retval;
	t_retval = ((QUrl *)handle)->authority((QUrl::ComponentFormattingOptions)options);
	copyQStringToPWideString(t_retval, retval);
}

void QUrl_setUserInfo(QUrlH handle, PWideString userInfo, QUrl::ParsingMode mode)
{
	QString t_userInfo;
	copyPWideStringToQString(userInfo, t_userInfo);
	((QUrl *)handle)->setUserInfo(t_userInfo, mode);
}

void QUrl_userInfo(QUrlH handle, PWideString retval, unsigned int options)
{
	QString t_retval;
	t_retval = ((QUrl *)handle)->userInfo((QUrl::ComponentFormattingOptions)options);
	copyQStringToPWideString(t_retval, retval);
}

void QUrl_setUserName(QUrlH handle, PWideString userName, QUrl::ParsingMode mode)
{
	QString t_userName;
	copyPWideStringToQString(userName, t_userName);
	((QUrl *)handle)->setUserName(t_userName, mode);
}

void QUrl_userName(QUrlH handle, PWideString retval, unsigned int options)
{
	QString t_retval;
	t_retval = ((QUrl *)handle)->userName((QUrl::ComponentFormattingOptions)options);
	copyQStringToPWideString(t_retval, retval);
}

void QUrl_setPassword(QUrlH handle, PWideString password, QUrl::ParsingMode mode)
{
	QString t_password;
	copyPWideStringToQString(password, t_password);
	((QUrl *)handle)->setPassword(t_password, mode);
}

void QUrl_password(QUrlH handle, PWideString retval, unsigned int AnonParam1)
{
	QString t_retval;
	t_retval = ((QUrl *)handle)->password((QUrl::ComponentFormattingOptions)AnonParam1);
	copyQStringToPWideString(t_retval, retval);
}

void QUrl_setHost(QUrlH handle, PWideString host, QUrl::ParsingMode mode)
{
	QString t_host;
	copyPWideStringToQString(host, t_host);
	((QUrl *)handle)->setHost(t_host, mode);
}

void QUrl_host(QUrlH handle, PWideString retval, unsigned int AnonParam1)
{
	QString t_retval;
	t_retval = ((QUrl *)handle)->host((QUrl::ComponentFormattingOptions)AnonParam1);
	copyQStringToPWideString(t_retval, retval);
}

void QUrl_setPort(QUrlH handle, int port)
{
	((QUrl *)handle)->setPort(port);
}

int QUrl_port(QUrlH handle, int defaultPort)
{
	return (int) ((QUrl *)handle)->port(defaultPort);
}

void QUrl_setPath(QUrlH handle, PWideString path, QUrl::ParsingMode mode)
{
	QString t_path;
	copyPWideStringToQString(path, t_path);
	((QUrl *)handle)->setPath(t_path, mode);
}

void QUrl_path(QUrlH handle, PWideString retval, unsigned int options)
{
	QString t_retval;
	t_retval = ((QUrl *)handle)->path((QUrl::ComponentFormattingOptions)options);
	copyQStringToPWideString(t_retval, retval);
}

bool QUrl_hasQuery(QUrlH handle)
{
	return (bool) ((QUrl *)handle)->hasQuery();
}

void QUrl_setQuery(QUrlH handle, PWideString query, QUrl::ParsingMode mode)
{
	QString t_query;
	copyPWideStringToQString(query, t_query);
	((QUrl *)handle)->setQuery(t_query, mode);
}

void QUrl_setQuery2(QUrlH handle, const QUrlQueryH query)
{
	((QUrl *)handle)->setQuery(*(const QUrlQuery*)query);
}

void QUrl_query(QUrlH handle, PWideString retval, unsigned int AnonParam1)
{
	QString t_retval;
	t_retval = ((QUrl *)handle)->query((QUrl::ComponentFormattingOptions)AnonParam1);
	copyQStringToPWideString(t_retval, retval);
}

bool QUrl_hasFragment(QUrlH handle)
{
	return (bool) ((QUrl *)handle)->hasFragment();
}

void QUrl_fragment(QUrlH handle, PWideString retval, unsigned int options)
{
	QString t_retval;
	t_retval = ((QUrl *)handle)->fragment((QUrl::ComponentFormattingOptions)options);
	copyQStringToPWideString(t_retval, retval);
}

void QUrl_setFragment(QUrlH handle, PWideString fragment, QUrl::ParsingMode mode)
{
	QString t_fragment;
	copyPWideStringToQString(fragment, t_fragment);
	((QUrl *)handle)->setFragment(t_fragment, mode);
}

void QUrl_resolved(QUrlH handle, QUrlH retval, const QUrlH relative)
{
	*(QUrl *)retval = ((QUrl *)handle)->resolved(*(const QUrl*)relative);
}

bool QUrl_isRelative(QUrlH handle)
{
	return (bool) ((QUrl *)handle)->isRelative();
}

bool QUrl_isParentOf(QUrlH handle, const QUrlH url)
{
	return (bool) ((QUrl *)handle)->isParentOf(*(const QUrl*)url);
}

bool QUrl_isLocalFile(QUrlH handle)
{
	return (bool) ((QUrl *)handle)->isLocalFile();
}

void QUrl_fromLocalFile(QUrlH retval, PWideString localfile)
{
	QString t_localfile;
	copyPWideStringToQString(localfile, t_localfile);
	*(QUrl *)retval = QUrl::fromLocalFile(t_localfile);
}

void QUrl_toLocalFile(QUrlH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QUrl *)handle)->toLocalFile();
	copyQStringToPWideString(t_retval, retval);
}

void QUrl_detach(QUrlH handle)
{
	((QUrl *)handle)->detach();
}

bool QUrl_isDetached(QUrlH handle)
{
	return (bool) ((QUrl *)handle)->isDetached();
}

void QUrl_adjusted(QUrlH handle, unsigned int options, QUrlH retval)
{
 *(QUrl *)retval = ((QUrl *)handle)->adjusted((QUrl::FormattingOptions )options);
}

void QUrl_fileName(QUrlH handle, unsigned int options, PWideString retval)
{
	QString t_retval;
	t_retval = ((QUrl *)handle)->fileName((QUrl::ComponentFormattingOptions)options);
	copyQStringToPWideString(t_retval, retval);
}

void QUrl_fromPercentEncoding(PWideString retval, const QByteArrayH AnonParam1)
{
	QString t_retval;
	t_retval = QUrl::fromPercentEncoding(*(const QByteArray*)AnonParam1);
	copyQStringToPWideString(t_retval, retval);
}

void QUrl_toPercentEncoding(QByteArrayH retval, PWideString AnonParam1, const QByteArrayH exclude, const QByteArrayH include)
{
	QString t_AnonParam1;
	copyPWideStringToQString(AnonParam1, t_AnonParam1);
	*(QByteArray *)retval = QUrl::toPercentEncoding(t_AnonParam1, *(const QByteArray*)exclude, *(const QByteArray*)include);
}

void QUrl_fromAce(PWideString retval, const QByteArrayH AnonParam1)
{
	QString t_retval;
	t_retval = QUrl::fromAce(*(const QByteArray*)AnonParam1);
	copyQStringToPWideString(t_retval, retval);
}

void QUrl_toAce(QByteArrayH retval, PWideString AnonParam1)
{
	QString t_AnonParam1;
	copyPWideStringToQString(AnonParam1, t_AnonParam1);
	*(QByteArray *)retval = QUrl::toAce(t_AnonParam1);
}

void QUrl_idnWhitelist(QStringListH retval)
{
	*(QStringList *)retval = QUrl::idnWhitelist();
}

void QUrl_setIdnWhitelist(const QStringListH AnonParam1)
{
	QUrl::setIdnWhitelist(*(const QStringList*)AnonParam1);
}

