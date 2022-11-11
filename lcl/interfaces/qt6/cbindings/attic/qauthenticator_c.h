//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QAUTHENTICATOR_C_H
#define QAUTHENTICATOR_C_H

#include <QtNetwork>
#include "pascalbind.h"

C_EXPORT QAuthenticatorH QAuthenticator_Create();
C_EXPORT void QAuthenticator_Destroy(QAuthenticatorH handle);
C_EXPORT QAuthenticatorH QAuthenticator_Create2(const QAuthenticatorH other);
C_EXPORT void QAuthenticator_user(QAuthenticatorH handle, PWideString retval);
C_EXPORT void QAuthenticator_setUser(QAuthenticatorH handle, PWideString user);
C_EXPORT void QAuthenticator_password(QAuthenticatorH handle, PWideString retval);
C_EXPORT void QAuthenticator_setPassword(QAuthenticatorH handle, PWideString password);
C_EXPORT void QAuthenticator_realm(QAuthenticatorH handle, PWideString retval);
C_EXPORT void QAuthenticator_option(QAuthenticatorH handle, QVariantH retval, PWideString opt);
C_EXPORT void QAuthenticator_setOption(QAuthenticatorH handle, PWideString opt, const QVariantH value);
C_EXPORT bool QAuthenticator_isNull(QAuthenticatorH handle);
C_EXPORT void QAuthenticator_detach(QAuthenticatorH handle);

#endif
