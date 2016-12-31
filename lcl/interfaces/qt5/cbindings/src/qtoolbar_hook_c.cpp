//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qtoolbar_hook_c.h"

QToolBar_hookH QToolBar_hook_Create(QObjectH handle)
{
	return (QToolBar_hookH) new QToolBar_hook((QObject*)handle);
}

void QToolBar_hook_Destroy(QToolBar_hookH handle)
{
	delete (QToolBar_hook *)handle;
}

void QToolBar_hook_hook_actionTriggered(QToolBar_hookH handle, QHookH hook)
{
	((QToolBar_hook *)handle)->hook_actionTriggered(hook);
}

void QToolBar_hook_hook_movableChanged(QToolBar_hookH handle, QHookH hook)
{
	((QToolBar_hook *)handle)->hook_movableChanged(hook);
}

void QToolBar_hook_hook_allowedAreasChanged(QToolBar_hookH handle, QHookH hook)
{
	((QToolBar_hook *)handle)->hook_allowedAreasChanged(hook);
}

void QToolBar_hook_hook_orientationChanged(QToolBar_hookH handle, QHookH hook)
{
	((QToolBar_hook *)handle)->hook_orientationChanged(hook);
}

void QToolBar_hook_hook_iconSizeChanged(QToolBar_hookH handle, QHookH hook)
{
	((QToolBar_hook *)handle)->hook_iconSizeChanged(hook);
}

void QToolBar_hook_hook_toolButtonStyleChanged(QToolBar_hookH handle, QHookH hook)
{
	((QToolBar_hook *)handle)->hook_toolButtonStyleChanged(hook);
}

void QToolBar_hook_hook_topLevelChanged(QToolBar_hookH handle, QHookH hook)
{
	((QToolBar_hook *)handle)->hook_topLevelChanged(hook);
}

void QToolBar_hook_hook_visibilityChanged(QToolBar_hookH handle, QHookH hook)
{
	((QToolBar_hook *)handle)->hook_visibilityChanged(hook);
}

