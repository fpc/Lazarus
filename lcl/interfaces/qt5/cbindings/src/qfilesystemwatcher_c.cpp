//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qfilesystemwatcher_c.h"

QFileSystemWatcherH QFileSystemWatcher_Create(QObjectH parent)
{
	return (QFileSystemWatcherH) new QFileSystemWatcher((QObject*)parent);
}

void QFileSystemWatcher_Destroy(QFileSystemWatcherH handle)
{
	delete (QFileSystemWatcher *)handle;
}

QFileSystemWatcherH QFileSystemWatcher_Create2(const QStringListH paths, QObjectH parent)
{
	return (QFileSystemWatcherH) new QFileSystemWatcher(*(const QStringList*)paths, (QObject*)parent);
}

bool QFileSystemWatcher_addPath(QFileSystemWatcherH handle, PWideString file)
{
	QString t_file;
	copyPWideStringToQString(file, t_file);
	return (bool) ((QFileSystemWatcher *)handle)->addPath(t_file);
}

void QFileSystemWatcher_addPaths(QFileSystemWatcherH handle, QStringListH retval, const QStringListH files)
{
	*(QStringList *)retval = ((QFileSystemWatcher *)handle)->addPaths(*(const QStringList*)files);
}

bool QFileSystemWatcher_removePath(QFileSystemWatcherH handle, PWideString file)
{
	QString t_file;
	copyPWideStringToQString(file, t_file);
	return (bool) ((QFileSystemWatcher *)handle)->removePath(t_file);
}

void QFileSystemWatcher_removePaths(QFileSystemWatcherH handle, QStringListH retval, const QStringListH files)
{
	*(QStringList *)retval = ((QFileSystemWatcher *)handle)->removePaths(*(const QStringList*)files);
}

void QFileSystemWatcher_files(QFileSystemWatcherH handle, QStringListH retval)
{
	*(QStringList *)retval = ((QFileSystemWatcher *)handle)->files();
}

void QFileSystemWatcher_directories(QFileSystemWatcherH handle, QStringListH retval)
{
	*(QStringList *)retval = ((QFileSystemWatcher *)handle)->directories();
}

