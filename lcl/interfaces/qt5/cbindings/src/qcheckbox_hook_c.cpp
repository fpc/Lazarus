//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qcheckbox_hook_c.h"

QCheckBox_hookH QCheckBox_hook_Create(QObjectH handle)
{
	return (QCheckBox_hookH) new QCheckBox_hook((QObject*)handle);
}

void QCheckBox_hook_Destroy(QCheckBox_hookH handle)
{
	delete (QCheckBox_hook *)handle;
}

void QCheckBox_hook_hook_stateChanged(QCheckBox_hookH handle, QHookH hook)
{
	((QCheckBox_hook *)handle)->hook_stateChanged(hook);
}

