//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QFILESYSTEMWATCHER_HOOK_C_H
#define QFILESYSTEMWATCHER_HOOK_C_H

#include "qfilesystemwatcher_hook.h"

C_EXPORT QFileSystemWatcher_hookH QFileSystemWatcher_hook_Create(QObjectH handle);
C_EXPORT void QFileSystemWatcher_hook_Destroy(QFileSystemWatcher_hookH handle);
C_EXPORT void QFileSystemWatcher_hook_hook_fileChanged(QFileSystemWatcher_hookH handle, QHookH hook);
C_EXPORT void QFileSystemWatcher_hook_hook_directoryChanged(QFileSystemWatcher_hookH handle, QHookH hook);

#endif
