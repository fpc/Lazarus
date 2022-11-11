//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QTABBAR_HOOK_H
#define QTABBAR_HOOK_H

#include <qtabbar.h>

#include "qwidget_hook.h"

class QTabBar_hook : public QWidget_hook {
  Q_OBJECT
  public:
    QTabBar_hook(QObject *handle) : QWidget_hook(handle) {
      currentChanged_event.func = NULL;
      tabCloseRequested_event.func = NULL;
      tabMoved_event.func = NULL;
    }
    void hook_currentChanged(QHook &hook) { 
      if ( !currentChanged_event.func )
        connect(handle, SIGNAL(currentChanged(int)), this, SLOT(currentChanged_hook(int)));
      currentChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(currentChanged(int)), this, SLOT(currentChanged_hook(int)));
    }
    void hook_tabCloseRequested(QHook &hook) { 
      if ( !tabCloseRequested_event.func )
        connect(handle, SIGNAL(tabCloseRequested(int)), this, SLOT(tabCloseRequested_hook(int)));
      tabCloseRequested_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(tabCloseRequested(int)), this, SLOT(tabCloseRequested_hook(int)));
    }
    void hook_tabMoved(QHook &hook) { 
      if ( !tabMoved_event.func )
        connect(handle, SIGNAL(tabMoved(int, int)), this, SLOT(tabMoved_hook(int, int)));
      tabMoved_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(tabMoved(int, int)), this, SLOT(tabMoved_hook(int, int)));
    }

  private slots:
    void currentChanged_hook(int index) {
      if ( currentChanged_event.func ) {
        typedef void (*func_type)(void *data, int index);
	(*(func_type)currentChanged_event.func)(currentChanged_event.data, index);
      }
    }
    void tabCloseRequested_hook(int index) {
      if ( tabCloseRequested_event.func ) {
        typedef void (*func_type)(void *data, int index);
	(*(func_type)tabCloseRequested_event.func)(tabCloseRequested_event.data, index);
      }
    }
    void tabMoved_hook(int from, int to) {
      if ( tabMoved_event.func ) {
        typedef void (*func_type)(void *data, int from, int to);
	(*(func_type)tabMoved_event.func)(tabMoved_event.data, from, to);
      }
    }
  private:
    QHook currentChanged_event;
    QHook tabCloseRequested_event;
    QHook tabMoved_event;
};


#endif
