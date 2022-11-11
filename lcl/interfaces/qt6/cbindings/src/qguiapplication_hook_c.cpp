//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qguiapplication_hook_c.h"

QGuiApplication_hookH QGuiApplication_hook_Create(QObjectH handle)
{
	return (QGuiApplication_hookH) new QGuiApplication_hook((QObject*)handle);
}

void QGuiApplication_hook_Destroy(QGuiApplication_hookH handle)
{
	delete (QGuiApplication_hook *)handle;
}

void QGuiApplication_hook_hook_applicationStateChanged(QGuiApplication_hookH handle, QHookH hook)
{
  ((QGuiApplication_hook *)handle)->hook_applicationStateChanged(hook);
}
void QGuiApplication_hook_hook_fontDatabaseChanged(QGuiApplication_hookH handle, QHookH hook)
{
	((QGuiApplication_hook *)handle)->hook_fontDatabaseChanged(hook);
}

void QGuiApplication_hook_hook_primaryScreenChanged(QGuiApplication_hookH handle, QHookH hook)
{
	((QGuiApplication_hook *)handle)->hook_primaryScreenChanged(hook);
}

void QGuiApplication_hook_hook_screenAdded(QGuiApplication_hookH handle, QHookH hook)
{
	((QGuiApplication_hook *)handle)->hook_screenAdded(hook);
}

void QGuiApplication_hook_hook_screenRemoved(QGuiApplication_hookH handle, QHookH hook)
{
	((QGuiApplication_hook *)handle)->hook_screenRemoved(hook);
}

void QGuiApplication_hook_hook_lastWindowClosed(QGuiApplication_hookH handle, QHookH hook)
{
	((QGuiApplication_hook *)handle)->hook_lastWindowClosed(hook);
}

void QGuiApplication_hook_hook_focusObjectChanged(QGuiApplication_hookH handle, QHookH hook)
{
	((QGuiApplication_hook *)handle)->hook_focusObjectChanged(hook);
}

void QGuiApplication_hook_hook_focusWindowChanged(QGuiApplication_hookH handle, QHookH hook)
{
	((QGuiApplication_hook *)handle)->hook_focusWindowChanged(hook);
}

void QGuiApplication_hook_hook_commitDataRequest(QGuiApplication_hookH handle, QHookH hook)
{
	((QGuiApplication_hook *)handle)->hook_commitDataRequest(hook);
}

void QGuiApplication_hook_hook_saveStateRequest(QGuiApplication_hookH handle, QHookH hook)
{
	((QGuiApplication_hook *)handle)->hook_saveStateRequest(hook);
}

