//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qprogressdialog_hook_c.h"

QProgressDialog_hookH QProgressDialog_hook_Create(QObjectH handle)
{
	return (QProgressDialog_hookH) new QProgressDialog_hook((QObject*)handle);
}

void QProgressDialog_hook_Destroy(QProgressDialog_hookH handle)
{
	delete (QProgressDialog_hook *)handle;
}

void QProgressDialog_hook_hook_canceled(QProgressDialog_hookH handle, QHookH hook)
{
	((QProgressDialog_hook *)handle)->hook_canceled(hook);
}

