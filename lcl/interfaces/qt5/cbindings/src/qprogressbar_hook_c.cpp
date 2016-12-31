//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qprogressbar_hook_c.h"

QProgressBar_hookH QProgressBar_hook_Create(QObjectH handle)
{
	return (QProgressBar_hookH) new QProgressBar_hook((QObject*)handle);
}

void QProgressBar_hook_Destroy(QProgressBar_hookH handle)
{
	delete (QProgressBar_hook *)handle;
}

void QProgressBar_hook_hook_valueChanged(QProgressBar_hookH handle, QHookH hook)
{
	((QProgressBar_hook *)handle)->hook_valueChanged(hook);
}

