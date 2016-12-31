//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qaction_hook_c.h"

QAction_hookH QAction_hook_Create(QObjectH handle)
{
	return (QAction_hookH) new QAction_hook((QObject*)handle);
}

void QAction_hook_Destroy(QAction_hookH handle)
{
	delete (QAction_hook *)handle;
}

void QAction_hook_hook_changed(QAction_hookH handle, QHookH hook)
{
	((QAction_hook *)handle)->hook_changed(hook);
}

void QAction_hook_hook_triggered(QAction_hookH handle, QHookH hook)
{
	((QAction_hook *)handle)->hook_triggered(hook);
}

void QAction_hook_hook_triggered2(QAction_hookH handle, QHookH hook)
{
	((QAction_hook *)handle)->hook_triggered2(hook);
}

void QAction_hook_hook_hovered(QAction_hookH handle, QHookH hook)
{
	((QAction_hook *)handle)->hook_hovered(hook);
}

void QAction_hook_hook_toggled(QAction_hookH handle, QHookH hook)
{
	((QAction_hook *)handle)->hook_toggled(hook);
}

