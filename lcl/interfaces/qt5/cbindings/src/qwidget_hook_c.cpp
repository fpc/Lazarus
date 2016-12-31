//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qwidget_hook_c.h"

QWidget_hookH QWidget_hook_Create(QObjectH handle)
{
	return (QWidget_hookH) new QWidget_hook((QObject*)handle);
}

void QWidget_hook_Destroy(QWidget_hookH handle)
{
	delete (QWidget_hook *)handle;
}

void QWidget_hook_hook_customContextMenuRequested(QWidget_hookH handle, QHookH hook)
{
	((QWidget_hook *)handle)->hook_customContextMenuRequested(hook);
}

