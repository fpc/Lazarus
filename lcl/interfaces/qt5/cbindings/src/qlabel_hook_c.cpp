//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qlabel_hook_c.h"

QLabel_hookH QLabel_hook_Create(QObjectH handle)
{
	return (QLabel_hookH) new QLabel_hook((QObject*)handle);
}

void QLabel_hook_Destroy(QLabel_hookH handle)
{
	delete (QLabel_hook *)handle;
}

void QLabel_hook_hook_linkActivated(QLabel_hookH handle, QHookH hook)
{
	((QLabel_hook *)handle)->hook_linkActivated(hook);
}

void QLabel_hook_hook_linkHovered(QLabel_hookH handle, QHookH hook)
{
	((QLabel_hook *)handle)->hook_linkHovered(hook);
}

