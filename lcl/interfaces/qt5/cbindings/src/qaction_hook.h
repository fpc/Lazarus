//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QACTION_HOOK_H
#define QACTION_HOOK_H

#include <qaction.h>

#include "qobject_hook.h"

class QAction_hook : public QObject_hook {
  Q_OBJECT
  public:
    QAction_hook(QObject *handle) : QObject_hook(handle) {
      changed_event.func = NULL;
      triggered_event.func = NULL;
      triggered2_event.func = NULL;
      hovered_event.func = NULL;
      toggled_event.func = NULL;
    }
    void hook_changed(QHook &hook) { 
      if ( !changed_event.func )
        connect(handle, SIGNAL(changed()), this, SLOT(changed_hook()));
      changed_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(changed()), this, SLOT(changed_hook()));
    }
    void hook_triggered(QHook &hook) { 
      if ( !triggered_event.func )
        connect(handle, SIGNAL(triggered(bool)), this, SLOT(triggered_hook(bool)));
      triggered_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(triggered(bool)), this, SLOT(triggered_hook(bool)));
    }
    void hook_triggered2(QHook &hook) { 
      if ( !triggered2_event.func )
        connect(handle, SIGNAL(triggered()), this, SLOT(triggered2_hook()));
      triggered2_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(triggered()), this, SLOT(triggered2_hook()));
    }
    void hook_hovered(QHook &hook) { 
      if ( !hovered_event.func )
        connect(handle, SIGNAL(hovered()), this, SLOT(hovered_hook()));
      hovered_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(hovered()), this, SLOT(hovered_hook()));
    }
    void hook_toggled(QHook &hook) { 
      if ( !toggled_event.func )
        connect(handle, SIGNAL(toggled(bool)), this, SLOT(toggled_hook(bool)));
      toggled_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(toggled(bool)), this, SLOT(toggled_hook(bool)));
    }

  private slots:
    void changed_hook() {
      if ( changed_event.func ) {
        typedef void (*func_type)(void *data);
	(*(func_type)changed_event.func)(changed_event.data);
      }
    }
    void triggered_hook(bool checked) {
      if ( triggered_event.func ) {
        typedef void (*func_type)(void *data, bool checked);
	(*(func_type)triggered_event.func)(triggered_event.data, checked);
      }
    }
    void triggered2_hook() {
      if ( triggered2_event.func ) {
        typedef void (*func_type)(void *data);
	(*(func_type)triggered2_event.func)(triggered2_event.data);
      }
    }
    void hovered_hook() {
      if ( hovered_event.func ) {
        typedef void (*func_type)(void *data);
	(*(func_type)hovered_event.func)(hovered_event.data);
      }
    }
    void toggled_hook(bool AnonParam1) {
      if ( toggled_event.func ) {
        typedef void (*func_type)(void *data, bool AnonParam1);
	(*(func_type)toggled_event.func)(toggled_event.data, AnonParam1);
      }
    }
  private:
    QHook changed_event;
    QHook triggered_event;
    QHook triggered2_event;
    QHook hovered_event;
    QHook toggled_event;
};


#endif
