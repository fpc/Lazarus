//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qmainwindow_hook_c.h"

QMainWindow_hookH QMainWindow_hook_Create(QObjectH handle)
{
	return (QMainWindow_hookH) new QMainWindow_hook((QObject*)handle);
}

void QMainWindow_hook_Destroy(QMainWindow_hookH handle)
{
	delete (QMainWindow_hook *)handle;
}

void QMainWindow_hook_hook_iconSizeChanged(QMainWindow_hookH handle, QHookH hook)
{
	((QMainWindow_hook *)handle)->hook_iconSizeChanged(hook);
}

void QMainWindow_hook_hook_toolButtonStyleChanged(QMainWindow_hookH handle, QHookH hook)
{
	((QMainWindow_hook *)handle)->hook_toolButtonStyleChanged(hook);
}

