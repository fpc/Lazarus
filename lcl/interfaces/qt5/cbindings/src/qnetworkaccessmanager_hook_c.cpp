//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qnetworkaccessmanager_hook_c.h"

QNetworkAccessManager_hookH QNetworkAccessManager_hook_Create(QObjectH handle)
{
	return (QNetworkAccessManager_hookH) new QNetworkAccessManager_hook((QObject*)handle);
}

void QNetworkAccessManager_hook_Destroy(QNetworkAccessManager_hookH handle)
{
	delete (QNetworkAccessManager_hook *)handle;
}

void QNetworkAccessManager_hook_hook_proxyAuthenticationRequired(QNetworkAccessManager_hookH handle, QHookH hook)
{
	((QNetworkAccessManager_hook *)handle)->hook_proxyAuthenticationRequired(hook);
}

void QNetworkAccessManager_hook_hook_authenticationRequired(QNetworkAccessManager_hookH handle, QHookH hook)
{
	((QNetworkAccessManager_hook *)handle)->hook_authenticationRequired(hook);
}

void QNetworkAccessManager_hook_hook_finished(QNetworkAccessManager_hookH handle, QHookH hook)
{
	((QNetworkAccessManager_hook *)handle)->hook_finished(hook);
}

void QNetworkAccessManager_hook_hook_encrypted(QNetworkAccessManager_hookH handle, QHookH hook)
{
	((QNetworkAccessManager_hook *)handle)->hook_encrypted(hook);
}

void QNetworkAccessManager_hook_hook_networkSessionConnected(QNetworkAccessManager_hookH handle, QHookH hook)
{
	((QNetworkAccessManager_hook *)handle)->hook_networkSessionConnected(hook);
}

void QNetworkAccessManager_hook_hook_networkAccessibleChanged(QNetworkAccessManager_hookH handle, QHookH hook)
{
	((QNetworkAccessManager_hook *)handle)->hook_networkAccessibleChanged(hook);
}

