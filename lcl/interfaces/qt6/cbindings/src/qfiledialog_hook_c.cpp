//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qfiledialog_hook_c.h"

QFileDialog_hookH QFileDialog_hook_Create(QObjectH handle)
{
	return (QFileDialog_hookH) new QFileDialog_hook((QObject*)handle);
}

void QFileDialog_hook_Destroy(QFileDialog_hookH handle)
{
	delete (QFileDialog_hook *)handle;
}

void QFileDialog_hook_hook_fileSelected(QFileDialog_hookH handle, QHookH hook)
{
	((QFileDialog_hook *)handle)->hook_fileSelected(hook);
}

void QFileDialog_hook_hook_filesSelected(QFileDialog_hookH handle, QHookH hook)
{
	((QFileDialog_hook *)handle)->hook_filesSelected(hook);
}

void QFileDialog_hook_hook_currentChanged(QFileDialog_hookH handle, QHookH hook)
{
	((QFileDialog_hook *)handle)->hook_currentChanged(hook);
}

void QFileDialog_hook_hook_directoryEntered(QFileDialog_hookH handle, QHookH hook)
{
	((QFileDialog_hook *)handle)->hook_directoryEntered(hook);
}

void QFileDialog_hook_hook_filterSelected(QFileDialog_hookH handle, QHookH hook)
{
	((QFileDialog_hook *)handle)->hook_filterSelected(hook);
}

