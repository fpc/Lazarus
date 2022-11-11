//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qthread_hook_c.h"

QThread_hookH QThread_hook_Create(QObjectH handle)
{
	return (QThread_hookH) new QThread_hook((QObject*)handle);
}

void QThread_hook_Destroy(QThread_hookH handle)
{
	delete (QThread_hook *)handle;
}

void QThread_hook_hook_started(QThread_hookH handle, QHookH hook)
{
	((QThread_hook *)handle)->hook_started(hook);
}

void QThread_hook_hook_finished(QThread_hookH handle, QHookH hook)
{
	((QThread_hook *)handle)->hook_finished(hook);
}

