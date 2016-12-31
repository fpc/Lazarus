//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qauthenticator_c.h"

QAuthenticatorH QAuthenticator_Create()
{
	return (QAuthenticatorH) new QAuthenticator();
}

void QAuthenticator_Destroy(QAuthenticatorH handle)
{
	delete (QAuthenticator *)handle;
}

QAuthenticatorH QAuthenticator_Create2(const QAuthenticatorH other)
{
	return (QAuthenticatorH) new QAuthenticator(*(const QAuthenticator*)other);
}

void QAuthenticator_user(QAuthenticatorH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QAuthenticator *)handle)->user();
	copyQStringToPWideString(t_retval, retval);
}

void QAuthenticator_setUser(QAuthenticatorH handle, PWideString user)
{
	QString t_user;
	copyPWideStringToQString(user, t_user);
	((QAuthenticator *)handle)->setUser(t_user);
}

void QAuthenticator_password(QAuthenticatorH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QAuthenticator *)handle)->password();
	copyQStringToPWideString(t_retval, retval);
}

void QAuthenticator_setPassword(QAuthenticatorH handle, PWideString password)
{
	QString t_password;
	copyPWideStringToQString(password, t_password);
	((QAuthenticator *)handle)->setPassword(t_password);
}

void QAuthenticator_realm(QAuthenticatorH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QAuthenticator *)handle)->realm();
	copyQStringToPWideString(t_retval, retval);
}

void QAuthenticator_option(QAuthenticatorH handle, QVariantH retval, PWideString opt)
{
	QString t_opt;
	copyPWideStringToQString(opt, t_opt);
	*(QVariant *)retval = ((QAuthenticator *)handle)->option(t_opt);
}

void QAuthenticator_setOption(QAuthenticatorH handle, PWideString opt, const QVariantH value)
{
	QString t_opt;
	copyPWideStringToQString(opt, t_opt);
	((QAuthenticator *)handle)->setOption(t_opt, *(const QVariant*)value);
}

bool QAuthenticator_isNull(QAuthenticatorH handle)
{
	return (bool) ((QAuthenticator *)handle)->isNull();
}

void QAuthenticator_detach(QAuthenticatorH handle)
{
	((QAuthenticator *)handle)->detach();
}

