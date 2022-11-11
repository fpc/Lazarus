//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QDESKTOPWIDGET_HOOK_H
#define QDESKTOPWIDGET_HOOK_H

#include <qdesktopwidget.h>

#include "qwidget_hook.h"

class QDesktopWidget_hook : public QWidget_hook {
  Q_OBJECT
  public:
    QDesktopWidget_hook(QObject *handle) : QWidget_hook(handle) {
      resized_event.func = NULL;
      workAreaResized_event.func = NULL;
      screenCountChanged_event.func = NULL;
    }
    void hook_resized(QHook &hook) { 
      if ( !resized_event.func )
        connect(handle, SIGNAL(resized(int)), this, SLOT(resized_hook(int)));
      resized_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(resized(int)), this, SLOT(resized_hook(int)));
    }
    void hook_workAreaResized(QHook &hook) { 
      if ( !workAreaResized_event.func )
        connect(handle, SIGNAL(workAreaResized(int)), this, SLOT(workAreaResized_hook(int)));
      workAreaResized_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(workAreaResized(int)), this, SLOT(workAreaResized_hook(int)));
    }
    void hook_screenCountChanged(QHook &hook) { 
      if ( !screenCountChanged_event.func )
        connect(handle, SIGNAL(screenCountChanged(int)), this, SLOT(screenCountChanged_hook(int)));
      screenCountChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(screenCountChanged(int)), this, SLOT(screenCountChanged_hook(int)));
    }

  private slots:
    void resized_hook(int AnonParam1) {
      if ( resized_event.func ) {
        typedef void (*func_type)(void *data, int AnonParam1);
	(*(func_type)resized_event.func)(resized_event.data, AnonParam1);
      }
    }
    void workAreaResized_hook(int AnonParam1) {
      if ( workAreaResized_event.func ) {
        typedef void (*func_type)(void *data, int AnonParam1);
	(*(func_type)workAreaResized_event.func)(workAreaResized_event.data, AnonParam1);
      }
    }
    void screenCountChanged_hook(int AnonParam1) {
      if ( screenCountChanged_event.func ) {
        typedef void (*func_type)(void *data, int AnonParam1);
	(*(func_type)screenCountChanged_event.func)(screenCountChanged_event.data, AnonParam1);
      }
    }
  private:
    QHook resized_event;
    QHook workAreaResized_event;
    QHook screenCountChanged_event;
};


#endif
