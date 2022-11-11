//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qactiongroup_hook_c.h"

QActionGroup_hookH QActionGroup_hook_Create(QObjectH handle)
{
	return (QActionGroup_hookH) new QActionGroup_hook((QObject*)handle);
}

void QActionGroup_hook_Destroy(QActionGroup_hookH handle)
{
	delete (QActionGroup_hook *)handle;
}

void QActionGroup_hook_hook_triggered(QActionGroup_hookH handle, QHookH hook)
{
	((QActionGroup_hook *)handle)->hook_triggered(hook);
}

void QActionGroup_hook_hook_hovered(QActionGroup_hookH handle, QHookH hook)
{
	((QActionGroup_hook *)handle)->hook_hovered(hook);
}

