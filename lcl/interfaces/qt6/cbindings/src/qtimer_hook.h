//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QTIMER_HOOK_H
#define QTIMER_HOOK_H

#include <qtimer.h>

#include "qobject_hook.h"

class QTimer_hook : public QObject_hook {
  Q_OBJECT
  public:
    QTimer_hook(QObject *handle) : QObject_hook(handle) {
      timeout_event.func = NULL;
    }
    void hook_timeout(QHook &hook) { 
      if ( !timeout_event.func )
        connect(handle, SIGNAL(timeout()), this, SLOT(timeout_hook()));
      timeout_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(timeout()), this, SLOT(timeout_hook()));
    }

  private slots:
    void timeout_hook() {
      if ( timeout_event.func ) {
        typedef void (*func_type)(void *data);
	(*(func_type)timeout_event.func)(timeout_event.data);
      }
    }
  private:
    QHook timeout_event;
};


#endif
