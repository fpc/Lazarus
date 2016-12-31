//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QNETWORKACCESSMANAGER_HOOK_C_H
#define QNETWORKACCESSMANAGER_HOOK_C_H

#include "qnetworkaccessmanager_hook.h"

C_EXPORT QNetworkAccessManager_hookH QNetworkAccessManager_hook_Create(QObjectH handle);
C_EXPORT void QNetworkAccessManager_hook_Destroy(QNetworkAccessManager_hookH handle);
C_EXPORT void QNetworkAccessManager_hook_hook_proxyAuthenticationRequired(QNetworkAccessManager_hookH handle, QHookH hook);
C_EXPORT void QNetworkAccessManager_hook_hook_authenticationRequired(QNetworkAccessManager_hookH handle, QHookH hook);
C_EXPORT void QNetworkAccessManager_hook_hook_finished(QNetworkAccessManager_hookH handle, QHookH hook);
C_EXPORT void QNetworkAccessManager_hook_hook_encrypted(QNetworkAccessManager_hookH handle, QHookH hook);
C_EXPORT void QNetworkAccessManager_hook_hook_networkSessionConnected(QNetworkAccessManager_hookH handle, QHookH hook);
C_EXPORT void QNetworkAccessManager_hook_hook_networkAccessibleChanged(QNetworkAccessManager_hookH handle, QHookH hook);

#endif
