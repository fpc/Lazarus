//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QSOCKETNOTIFIER_HOOK_H
#define QSOCKETNOTIFIER_HOOK_H

#include <qsocketnotifier.h>

#include "qobject_hook.h"

class QSocketNotifier_hook : public QObject_hook {
  Q_OBJECT
  public:
    QSocketNotifier_hook(QObject *handle) : QObject_hook(handle) {
      activated_event.func = NULL;
    }
    void hook_activated(QHook &hook) { 
      if ( !activated_event.func )
        connect(handle, SIGNAL(activated(int)), this, SLOT(activated_hook(int)));
      activated_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(activated(int)), this, SLOT(activated_hook(int)));
    }

  private slots:
    void activated_hook(int socket) {
      if ( activated_event.func ) {
        typedef void (*func_type)(void *data, int socket);
	(*(func_type)activated_event.func)(activated_event.data, socket);
      }
    }
  private:
    QHook activated_event;
};


#endif
