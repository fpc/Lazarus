//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qfilesystemwatcher_hook_c.h"

QFileSystemWatcher_hookH QFileSystemWatcher_hook_Create(QObjectH handle)
{
	return (QFileSystemWatcher_hookH) new QFileSystemWatcher_hook((QObject*)handle);
}

void QFileSystemWatcher_hook_Destroy(QFileSystemWatcher_hookH handle)
{
	delete (QFileSystemWatcher_hook *)handle;
}

void QFileSystemWatcher_hook_hook_fileChanged(QFileSystemWatcher_hookH handle, QHookH hook)
{
	((QFileSystemWatcher_hook *)handle)->hook_fileChanged(hook);
}

void QFileSystemWatcher_hook_hook_directoryChanged(QFileSystemWatcher_hookH handle, QHookH hook)
{
	((QFileSystemWatcher_hook *)handle)->hook_directoryChanged(hook);
}

