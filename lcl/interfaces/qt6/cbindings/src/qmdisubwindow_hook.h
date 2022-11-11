//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QMDISUBWINDOW_HOOK_H
#define QMDISUBWINDOW_HOOK_H

#include <qmdisubwindow.h>

#include "qwidget_hook.h"

class QMdiSubWindow_hook : public QWidget_hook {
  Q_OBJECT
  public:
    QMdiSubWindow_hook(QObject *handle) : QWidget_hook(handle) {
      windowStateChanged_event.func = NULL;
      aboutToActivate_event.func = NULL;
    }
    void hook_windowStateChanged(QHook &hook) { 
      if ( !windowStateChanged_event.func )
        connect(handle, SIGNAL(windowStateChanged(Qt::WindowStates, Qt::WindowStates)), this, SLOT(windowStateChanged_hook(Qt::WindowStates, Qt::WindowStates)));
      windowStateChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(windowStateChanged(Qt::WindowStates, Qt::WindowStates)), this, SLOT(windowStateChanged_hook(Qt::WindowStates, Qt::WindowStates)));
    }
    void hook_aboutToActivate(QHook &hook) { 
      if ( !aboutToActivate_event.func )
        connect(handle, SIGNAL(aboutToActivate()), this, SLOT(aboutToActivate_hook()));
      aboutToActivate_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(aboutToActivate()), this, SLOT(aboutToActivate_hook()));
    }

  private slots:
    void windowStateChanged_hook(Qt::WindowStates oldState, Qt::WindowStates newState) {
      if ( windowStateChanged_event.func ) {
        typedef void (*func_type)(void *data, unsigned int oldState, unsigned int newState);
	(*(func_type)windowStateChanged_event.func)(windowStateChanged_event.data, (unsigned int)oldState, (unsigned int)newState);
      }
    }
    void aboutToActivate_hook() {
      if ( aboutToActivate_event.func ) {
        typedef void (*func_type)(void *data);
	(*(func_type)aboutToActivate_event.func)(aboutToActivate_event.data);
      }
    }
  private:
    QHook windowStateChanged_event;
    QHook aboutToActivate_event;
};


#endif
