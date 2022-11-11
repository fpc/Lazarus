//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qiodevice_hook_c.h"

QIODevice_hookH QIODevice_hook_Create(QObjectH handle)
{
	return (QIODevice_hookH) new QIODevice_hook((QObject*)handle);
}

void QIODevice_hook_Destroy(QIODevice_hookH handle)
{
	delete (QIODevice_hook *)handle;
}

void QIODevice_hook_hook_readyRead(QIODevice_hookH handle, QHookH hook)
{
	((QIODevice_hook *)handle)->hook_readyRead(hook);
}

void QIODevice_hook_hook_bytesWritten(QIODevice_hookH handle, QHookH hook)
{
	((QIODevice_hook *)handle)->hook_bytesWritten(hook);
}

void QIODevice_hook_hook_aboutToClose(QIODevice_hookH handle, QHookH hook)
{
	((QIODevice_hook *)handle)->hook_aboutToClose(hook);
}

void QIODevice_hook_hook_readChannelFinished(QIODevice_hookH handle, QHookH hook)
{
	((QIODevice_hook *)handle)->hook_readChannelFinished(hook);
}

