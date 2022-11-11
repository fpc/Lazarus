//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QTABWIDGET_HOOK_H
#define QTABWIDGET_HOOK_H

#include <qtabwidget.h>

#include "qwidget_hook.h"

class QTabWidget_hook : public QWidget_hook {
  Q_OBJECT
  public:
    QTabWidget_hook(QObject *handle) : QWidget_hook(handle) {
      currentChanged_event.func = NULL;
      tabCloseRequested_event.func = NULL;
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
  private:
    QHook currentChanged_event;
    QHook tabCloseRequested_event;
};


#endif
