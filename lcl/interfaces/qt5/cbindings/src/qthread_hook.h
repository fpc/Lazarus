//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QTHREAD_HOOK_H
#define QTHREAD_HOOK_H

#include <qthread.h>

#include "qobject_hook.h"

class QThread_hook : public QObject_hook {
  Q_OBJECT
  public:
    QThread_hook(QObject *handle) : QObject_hook(handle) {
      started_event.func = NULL;
      finished_event.func = NULL;
    }
    void hook_started(QHook &hook) { 
      if ( !started_event.func )
        connect(handle, SIGNAL(started()), this, SLOT(started_hook()));
      started_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(started()), this, SLOT(started_hook()));
    }
    void hook_finished(QHook &hook) { 
      if ( !finished_event.func )
        connect(handle, SIGNAL(finished()), this, SLOT(finished_hook()));
      finished_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(finished()), this, SLOT(finished_hook()));
    }

  private slots:
    void started_hook() {
      if ( started_event.func ) {
        typedef void (*func_type)(void *data);
	(*(func_type)started_event.func)(started_event.data);
      }
    }
    void finished_hook() {
      if ( finished_event.func ) {
        typedef void (*func_type)(void *data);
	(*(func_type)finished_event.func)(finished_event.data);
      }
    }
  private:
    QHook started_event;
    QHook finished_event;
};


#endif
