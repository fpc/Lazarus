//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qobject_hook_c.h"

QObject_hookH QObject_hook_Create(QObjectH handle)
{
	return (QObject_hookH) new QObject_hook((QObject*)handle);
}

void QObject_hook_Destroy(QObject_hookH handle)
{
	delete (QObject_hook *)handle;
}

void QObject_hook_hook_events(QObject_hookH handle, QHookH hook)
{
	((QObject_hook *)handle)->hook_events(hook);
}

void QObject_hook_hook_destroyed(QObject_hookH handle, QHookH hook)
{
	((QObject_hook *)handle)->hook_destroyed(hook);
}

