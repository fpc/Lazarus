//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QFILESYSTEMWATCHER_C_H
#define QFILESYSTEMWATCHER_C_H

#include <QtCore>
#include "pascalbind.h"

C_EXPORT QFileSystemWatcherH QFileSystemWatcher_Create(QObjectH parent);
C_EXPORT void QFileSystemWatcher_Destroy(QFileSystemWatcherH handle);
C_EXPORT QFileSystemWatcherH QFileSystemWatcher_Create2(const QStringListH paths, QObjectH parent);
C_EXPORT bool QFileSystemWatcher_addPath(QFileSystemWatcherH handle, PWideString file);
C_EXPORT void QFileSystemWatcher_addPaths(QFileSystemWatcherH handle, QStringListH retval, const QStringListH files);
C_EXPORT bool QFileSystemWatcher_removePath(QFileSystemWatcherH handle, PWideString file);
C_EXPORT void QFileSystemWatcher_removePaths(QFileSystemWatcherH handle, QStringListH retval, const QStringListH files);
C_EXPORT void QFileSystemWatcher_files(QFileSystemWatcherH handle, QStringListH retval);
C_EXPORT void QFileSystemWatcher_directories(QFileSystemWatcherH handle, QStringListH retval);

#endif
