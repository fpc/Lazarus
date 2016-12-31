//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qtimer_hook_c.h"

QTimer_hookH QTimer_hook_Create(QObjectH handle)
{
	return (QTimer_hookH) new QTimer_hook((QObject*)handle);
}

void QTimer_hook_Destroy(QTimer_hookH handle)
{
	delete (QTimer_hook *)handle;
}

void QTimer_hook_hook_timeout(QTimer_hookH handle, QHookH hook)
{
	((QTimer_hook *)handle)->hook_timeout(hook);
}

