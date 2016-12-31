//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qtoolbox_hook_c.h"

QToolBox_hookH QToolBox_hook_Create(QObjectH handle)
{
	return (QToolBox_hookH) new QToolBox_hook((QObject*)handle);
}

void QToolBox_hook_Destroy(QToolBox_hookH handle)
{
	delete (QToolBox_hook *)handle;
}

void QToolBox_hook_hook_currentChanged(QToolBox_hookH handle, QHookH hook)
{
	((QToolBox_hook *)handle)->hook_currentChanged(hook);
}

