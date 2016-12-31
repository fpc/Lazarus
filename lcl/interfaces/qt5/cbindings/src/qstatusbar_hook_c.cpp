//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qstatusbar_hook_c.h"

QStatusBar_hookH QStatusBar_hook_Create(QObjectH handle)
{
	return (QStatusBar_hookH) new QStatusBar_hook((QObject*)handle);
}

void QStatusBar_hook_Destroy(QStatusBar_hookH handle)
{
	delete (QStatusBar_hook *)handle;
}

void QStatusBar_hook_hook_messageChanged(QStatusBar_hookH handle, QHookH hook)
{
	((QStatusBar_hook *)handle)->hook_messageChanged(hook);
}

