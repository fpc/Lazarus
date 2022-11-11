//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qtoolbutton_hook_c.h"

QToolButton_hookH QToolButton_hook_Create(QObjectH handle)
{
	return (QToolButton_hookH) new QToolButton_hook((QObject*)handle);
}

void QToolButton_hook_Destroy(QToolButton_hookH handle)
{
	delete (QToolButton_hook *)handle;
}

void QToolButton_hook_hook_triggered(QToolButton_hookH handle, QHookH hook)
{
	((QToolButton_hook *)handle)->hook_triggered(hook);
}

