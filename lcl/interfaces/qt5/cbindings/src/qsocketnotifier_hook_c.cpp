//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qsocketnotifier_hook_c.h"

QSocketNotifier_hookH QSocketNotifier_hook_Create(QObjectH handle)
{
	return (QSocketNotifier_hookH) new QSocketNotifier_hook((QObject*)handle);
}

void QSocketNotifier_hook_Destroy(QSocketNotifier_hookH handle)
{
	delete (QSocketNotifier_hook *)handle;
}

void QSocketNotifier_hook_hook_activated(QSocketNotifier_hookH handle, QHookH hook)
{
	((QSocketNotifier_hook *)handle)->hook_activated(hook);
}

