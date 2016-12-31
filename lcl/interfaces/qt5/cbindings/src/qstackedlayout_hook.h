//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QSTACKEDLAYOUT_HOOK_H
#define QSTACKEDLAYOUT_HOOK_H

#include <qstackedlayout.h>

#include "qlayout_hook.h"

class QStackedLayout_hook : public QLayout_hook {
  Q_OBJECT
  public:
    QStackedLayout_hook(QObject *handle) : QLayout_hook(handle) {
      widgetRemoved_event.func = NULL;
      currentChanged_event.func = NULL;
    }
    void hook_widgetRemoved(QHook &hook) { 
      if ( !widgetRemoved_event.func )
        connect(handle, SIGNAL(widgetRemoved(int)), this, SLOT(widgetRemoved_hook(int)));
      widgetRemoved_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(widgetRemoved(int)), this, SLOT(widgetRemoved_hook(int)));
    }
    void hook_currentChanged(QHook &hook) { 
      if ( !currentChanged_event.func )
        connect(handle, SIGNAL(currentChanged(int)), this, SLOT(currentChanged_hook(int)));
      currentChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(currentChanged(int)), this, SLOT(currentChanged_hook(int)));
    }

  private slots:
    void widgetRemoved_hook(int index) {
      if ( widgetRemoved_event.func ) {
        typedef void (*func_type)(void *data, int index);
	(*(func_type)widgetRemoved_event.func)(widgetRemoved_event.data, index);
      }
    }
    void currentChanged_hook(int index) {
      if ( currentChanged_event.func ) {
        typedef void (*func_type)(void *data, int index);
	(*(func_type)currentChanged_event.func)(currentChanged_event.data, index);
      }
    }
  private:
    QHook widgetRemoved_event;
    QHook currentChanged_event;
};


#endif
