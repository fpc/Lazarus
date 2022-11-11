//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qdesktopwidget_hook_c.h"

QDesktopWidget_hookH QDesktopWidget_hook_Create(QObjectH handle)
{
	return (QDesktopWidget_hookH) new QDesktopWidget_hook((QObject*)handle);
}

void QDesktopWidget_hook_Destroy(QDesktopWidget_hookH handle)
{
	delete (QDesktopWidget_hook *)handle;
}

void QDesktopWidget_hook_hook_resized(QDesktopWidget_hookH handle, QHookH hook)
{
	((QDesktopWidget_hook *)handle)->hook_resized(hook);
}

void QDesktopWidget_hook_hook_workAreaResized(QDesktopWidget_hookH handle, QHookH hook)
{
	((QDesktopWidget_hook *)handle)->hook_workAreaResized(hook);
}

void QDesktopWidget_hook_hook_screenCountChanged(QDesktopWidget_hookH handle, QHookH hook)
{
	((QDesktopWidget_hook *)handle)->hook_screenCountChanged(hook);
}

