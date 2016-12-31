//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QFILESYSTEMWATCHER_HOOK_H
#define QFILESYSTEMWATCHER_HOOK_H

#include <qfilesystemwatcher.h>

#include "qobject_hook.h"

class QFileSystemWatcher_hook : public QObject_hook {
  Q_OBJECT
  public:
    QFileSystemWatcher_hook(QObject *handle) : QObject_hook(handle) {
      fileChanged_event.func = NULL;
      directoryChanged_event.func = NULL;
    }
    void hook_fileChanged(QHook &hook) { 
      if ( !fileChanged_event.func )
        connect(handle, SIGNAL(fileChanged(const QString&)), this, SLOT(fileChanged_hook(const QString&)));
      fileChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(fileChanged(const QString&)), this, SLOT(fileChanged_hook(const QString&)));
    }
    void hook_directoryChanged(QHook &hook) { 
      if ( !directoryChanged_event.func )
        connect(handle, SIGNAL(directoryChanged(const QString&)), this, SLOT(directoryChanged_hook(const QString&)));
      directoryChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(directoryChanged(const QString&)), this, SLOT(directoryChanged_hook(const QString&)));
    }

  private slots:
    void fileChanged_hook(const QString& path) {
      if ( fileChanged_event.func ) {
        typedef void (*func_type)(void *data, PWideString path);
	PWideString t_path;
	initializePWideString(t_path);
	copyQStringToPWideString(path, t_path);
	(*(func_type)fileChanged_event.func)(fileChanged_event.data, t_path);
	finalizePWideString(t_path);
      }
    }
    void directoryChanged_hook(const QString& path) {
      if ( directoryChanged_event.func ) {
        typedef void (*func_type)(void *data, PWideString path);
	PWideString t_path;
	initializePWideString(t_path);
	copyQStringToPWideString(path, t_path);
	(*(func_type)directoryChanged_event.func)(directoryChanged_event.data, t_path);
	finalizePWideString(t_path);
      }
    }
  private:
    QHook fileChanged_event;
    QHook directoryChanged_event;
};


#endif
