//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qmdisubwindow_hook_c.h"

QMdiSubWindow_hookH QMdiSubWindow_hook_Create(QObjectH handle)
{
	return (QMdiSubWindow_hookH) new QMdiSubWindow_hook((QObject*)handle);
}

void QMdiSubWindow_hook_Destroy(QMdiSubWindow_hookH handle)
{
	delete (QMdiSubWindow_hook *)handle;
}

void QMdiSubWindow_hook_hook_windowStateChanged(QMdiSubWindow_hookH handle, QHookH hook)
{
	((QMdiSubWindow_hook *)handle)->hook_windowStateChanged(hook);
}

void QMdiSubWindow_hook_hook_aboutToActivate(QMdiSubWindow_hookH handle, QHookH hook)
{
	((QMdiSubWindow_hook *)handle)->hook_aboutToActivate(hook);
}

