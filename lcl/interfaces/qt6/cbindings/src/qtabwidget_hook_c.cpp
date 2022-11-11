//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qtabwidget_hook_c.h"

QTabWidget_hookH QTabWidget_hook_Create(QObjectH handle)
{
	return (QTabWidget_hookH) new QTabWidget_hook((QObject*)handle);
}

void QTabWidget_hook_Destroy(QTabWidget_hookH handle)
{
	delete (QTabWidget_hook *)handle;
}

void QTabWidget_hook_hook_currentChanged(QTabWidget_hookH handle, QHookH hook)
{
	((QTabWidget_hook *)handle)->hook_currentChanged(hook);
}

void QTabWidget_hook_hook_tabCloseRequested(QTabWidget_hookH handle, QHookH hook)
{
	((QTabWidget_hook *)handle)->hook_tabCloseRequested(hook);
}

