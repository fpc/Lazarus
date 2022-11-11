//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qclipboard_hook_c.h"

QClipboard_hookH QClipboard_hook_Create(QObjectH handle)
{
	return (QClipboard_hookH) new QClipboard_hook((QObject*)handle);
}

void QClipboard_hook_Destroy(QClipboard_hookH handle)
{
	delete (QClipboard_hook *)handle;
}

void QClipboard_hook_hook_changed(QClipboard_hookH handle, QHookH hook)
{
	((QClipboard_hook *)handle)->hook_changed(hook);
}

void QClipboard_hook_hook_selectionChanged(QClipboard_hookH handle, QHookH hook)
{
	((QClipboard_hook *)handle)->hook_selectionChanged(hook);
}

void QClipboard_hook_hook_findBufferChanged(QClipboard_hookH handle, QHookH hook)
{
	((QClipboard_hook *)handle)->hook_findBufferChanged(hook);
}

void QClipboard_hook_hook_dataChanged(QClipboard_hookH handle, QHookH hook)
{
	((QClipboard_hook *)handle)->hook_dataChanged(hook);
}

