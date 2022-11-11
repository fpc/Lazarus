//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qmdiarea_hook_c.h"

QMdiArea_hookH QMdiArea_hook_Create(QObjectH handle)
{
	return (QMdiArea_hookH) new QMdiArea_hook((QObject*)handle);
}

void QMdiArea_hook_Destroy(QMdiArea_hookH handle)
{
	delete (QMdiArea_hook *)handle;
}

void QMdiArea_hook_hook_subWindowActivated(QMdiArea_hookH handle, QHookH hook)
{
	((QMdiArea_hook *)handle)->hook_subWindowActivated(hook);
}

