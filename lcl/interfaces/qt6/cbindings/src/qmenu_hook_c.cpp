//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qmenu_hook_c.h"

QMenu_hookH QMenu_hook_Create(QObjectH handle)
{
	return (QMenu_hookH) new QMenu_hook((QObject*)handle);
}

void QMenu_hook_Destroy(QMenu_hookH handle)
{
	delete (QMenu_hook *)handle;
}

void QMenu_hook_hook_aboutToShow(QMenu_hookH handle, QHookH hook)
{
	((QMenu_hook *)handle)->hook_aboutToShow(hook);
}

void QMenu_hook_hook_aboutToHide(QMenu_hookH handle, QHookH hook)
{
	((QMenu_hook *)handle)->hook_aboutToHide(hook);
}

void QMenu_hook_hook_triggered(QMenu_hookH handle, QHookH hook)
{
	((QMenu_hook *)handle)->hook_triggered(hook);
}

void QMenu_hook_hook_hovered(QMenu_hookH handle, QHookH hook)
{
	((QMenu_hook *)handle)->hook_hovered(hook);
}

