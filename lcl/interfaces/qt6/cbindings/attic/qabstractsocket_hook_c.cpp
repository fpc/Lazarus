//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qabstractsocket_hook_c.h"

QAbstractSocket_hookH QAbstractSocket_hook_Create(QObjectH handle)
{
	return (QAbstractSocket_hookH) new QAbstractSocket_hook((QObject*)handle);
}

void QAbstractSocket_hook_Destroy(QAbstractSocket_hookH handle)
{
	delete (QAbstractSocket_hook *)handle;
}

void QAbstractSocket_hook_hook_hostFound(QAbstractSocket_hookH handle, QHookH hook)
{
	((QAbstractSocket_hook *)handle)->hook_hostFound(hook);
}

void QAbstractSocket_hook_hook_connected(QAbstractSocket_hookH handle, QHookH hook)
{
	((QAbstractSocket_hook *)handle)->hook_connected(hook);
}

void QAbstractSocket_hook_hook_disconnected(QAbstractSocket_hookH handle, QHookH hook)
{
	((QAbstractSocket_hook *)handle)->hook_disconnected(hook);
}

void QAbstractSocket_hook_hook_stateChanged(QAbstractSocket_hookH handle, QHookH hook)
{
	((QAbstractSocket_hook *)handle)->hook_stateChanged(hook);
}

void QAbstractSocket_hook_hook_error(QAbstractSocket_hookH handle, QHookH hook)
{
	((QAbstractSocket_hook *)handle)->hook_error(hook);
}

void QAbstractSocket_hook_hook_proxyAuthenticationRequired(QAbstractSocket_hookH handle, QHookH hook)
{
	((QAbstractSocket_hook *)handle)->hook_proxyAuthenticationRequired(hook);
}

