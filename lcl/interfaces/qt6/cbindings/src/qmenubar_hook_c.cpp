//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qmenubar_hook_c.h"

QMenuBar_hookH QMenuBar_hook_Create(QObjectH handle)
{
	return (QMenuBar_hookH) new QMenuBar_hook((QObject*)handle);
}

void QMenuBar_hook_Destroy(QMenuBar_hookH handle)
{
	delete (QMenuBar_hook *)handle;
}

void QMenuBar_hook_hook_triggered(QMenuBar_hookH handle, QHookH hook)
{
	((QMenuBar_hook *)handle)->hook_triggered(hook);
}

void QMenuBar_hook_hook_hovered(QMenuBar_hookH handle, QHookH hook)
{
	((QMenuBar_hook *)handle)->hook_hovered(hook);
}

