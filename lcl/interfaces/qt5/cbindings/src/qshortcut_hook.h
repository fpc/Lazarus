//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QSHORTCUT_HOOK_H
#define QSHORTCUT_HOOK_H

#include <qshortcut.h>

#include "qobject_hook.h"

class QShortcut_hook : public QObject_hook {
  Q_OBJECT
  public:
    QShortcut_hook(QObject *handle) : QObject_hook(handle) {
      activated_event.func = NULL;
      activatedAmbiguously_event.func = NULL;
    }
    void hook_activated(QHook &hook) { 
      if ( !activated_event.func )
        connect(handle, SIGNAL(activated()), this, SLOT(activated_hook()));
      activated_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(activated()), this, SLOT(activated_hook()));
    }
    void hook_activatedAmbiguously(QHook &hook) { 
      if ( !activatedAmbiguously_event.func )
        connect(handle, SIGNAL(activatedAmbiguously()), this, SLOT(activatedAmbiguously_hook()));
      activatedAmbiguously_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(activatedAmbiguously()), this, SLOT(activatedAmbiguously_hook()));
    }

  private slots:
    void activated_hook() {
      if ( activated_event.func ) {
        typedef void (*func_type)(void *data);
	(*(func_type)activated_event.func)(activated_event.data);
      }
    }
    void activatedAmbiguously_hook() {
      if ( activatedAmbiguously_event.func ) {
        typedef void (*func_type)(void *data);
	(*(func_type)activatedAmbiguously_event.func)(activatedAmbiguously_event.data);
      }
    }
  private:
    QHook activated_event;
    QHook activatedAmbiguously_event;
};


#endif
