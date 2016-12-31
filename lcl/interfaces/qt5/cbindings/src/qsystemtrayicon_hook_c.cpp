//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qsystemtrayicon_hook_c.h"

QSystemTrayIcon_hookH QSystemTrayIcon_hook_Create(QObjectH handle)
{
	return (QSystemTrayIcon_hookH) new QSystemTrayIcon_hook((QObject*)handle);
}

void QSystemTrayIcon_hook_Destroy(QSystemTrayIcon_hookH handle)
{
	delete (QSystemTrayIcon_hook *)handle;
}

void QSystemTrayIcon_hook_hook_activated(QSystemTrayIcon_hookH handle, QHookH hook)
{
	((QSystemTrayIcon_hook *)handle)->hook_activated(hook);
}

void QSystemTrayIcon_hook_hook_messageClicked(QSystemTrayIcon_hookH handle, QHookH hook)
{
	((QSystemTrayIcon_hook *)handle)->hook_messageClicked(hook);
}

