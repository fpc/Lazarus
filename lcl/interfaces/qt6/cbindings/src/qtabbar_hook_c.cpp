//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qtabbar_hook_c.h"

QTabBar_hookH QTabBar_hook_Create(QObjectH handle)
{
	return (QTabBar_hookH) new QTabBar_hook((QObject*)handle);
}

void QTabBar_hook_Destroy(QTabBar_hookH handle)
{
	delete (QTabBar_hook *)handle;
}

void QTabBar_hook_hook_currentChanged(QTabBar_hookH handle, QHookH hook)
{
	((QTabBar_hook *)handle)->hook_currentChanged(hook);
}

void QTabBar_hook_hook_tabCloseRequested(QTabBar_hookH handle, QHookH hook)
{
	((QTabBar_hook *)handle)->hook_tabCloseRequested(hook);
}

void QTabBar_hook_hook_tabMoved(QTabBar_hookH handle, QHookH hook)
{
	((QTabBar_hook *)handle)->hook_tabMoved(hook);
}

