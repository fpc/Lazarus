//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QSTACKEDWIDGET_HOOK_H
#define QSTACKEDWIDGET_HOOK_H

#include <qstackedwidget.h>

#include "qframe_hook.h"

class QStackedWidget_hook : public QFrame_hook {
  Q_OBJECT
  public:
    QStackedWidget_hook(QObject *handle) : QFrame_hook(handle) {
      currentChanged_event.func = NULL;
      widgetRemoved_event.func = NULL;
    }
    void hook_currentChanged(QHook &hook) { 
      if ( !currentChanged_event.func )
        connect(handle, SIGNAL(currentChanged(int)), this, SLOT(currentChanged_hook(int)));
      currentChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(currentChanged(int)), this, SLOT(currentChanged_hook(int)));
    }
    void hook_widgetRemoved(QHook &hook) { 
      if ( !widgetRemoved_event.func )
        connect(handle, SIGNAL(widgetRemoved(int)), this, SLOT(widgetRemoved_hook(int)));
      widgetRemoved_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(widgetRemoved(int)), this, SLOT(widgetRemoved_hook(int)));
    }

  private slots:
    void currentChanged_hook(int AnonParam1) {
      if ( currentChanged_event.func ) {
        typedef void (*func_type)(void *data, int AnonParam1);
	(*(func_type)currentChanged_event.func)(currentChanged_event.data, AnonParam1);
      }
    }
    void widgetRemoved_hook(int index) {
      if ( widgetRemoved_event.func ) {
        typedef void (*func_type)(void *data, int index);
	(*(func_type)widgetRemoved_event.func)(widgetRemoved_event.data, index);
      }
    }
  private:
    QHook currentChanged_event;
    QHook widgetRemoved_event;
};


#endif
