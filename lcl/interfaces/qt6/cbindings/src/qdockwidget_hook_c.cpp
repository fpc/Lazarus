//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qdockwidget_hook_c.h"

QDockWidget_hookH QDockWidget_hook_Create(QObjectH handle)
{
	return (QDockWidget_hookH) new QDockWidget_hook((QObject*)handle);
}

void QDockWidget_hook_Destroy(QDockWidget_hookH handle)
{
	delete (QDockWidget_hook *)handle;
}

void QDockWidget_hook_hook_featuresChanged(QDockWidget_hookH handle, QHookH hook)
{
	((QDockWidget_hook *)handle)->hook_featuresChanged(hook);
}

void QDockWidget_hook_hook_topLevelChanged(QDockWidget_hookH handle, QHookH hook)
{
	((QDockWidget_hook *)handle)->hook_topLevelChanged(hook);
}

void QDockWidget_hook_hook_allowedAreasChanged(QDockWidget_hookH handle, QHookH hook)
{
	((QDockWidget_hook *)handle)->hook_allowedAreasChanged(hook);
}

void QDockWidget_hook_hook_visibilityChanged(QDockWidget_hookH handle, QHookH hook)
{
	((QDockWidget_hook *)handle)->hook_visibilityChanged(hook);
}

void QDockWidget_hook_hook_dockLocationChanged(QDockWidget_hookH handle, QHookH hook)
{
	((QDockWidget_hook *)handle)->hook_dockLocationChanged(hook);
}

