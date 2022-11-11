//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QTOOLBUTTON_HOOK_H
#define QTOOLBUTTON_HOOK_H

#include <qtoolbutton.h>

#include "qabstractbutton_hook.h"

class QToolButton_hook : public QAbstractButton_hook {
  Q_OBJECT
  public:
    QToolButton_hook(QObject *handle) : QAbstractButton_hook(handle) {
      triggered_event.func = NULL;
    }
    void hook_triggered(QHook &hook) { 
      if ( !triggered_event.func )
        connect(handle, SIGNAL(triggered(QAction*)), this, SLOT(triggered_hook(QAction*)));
      triggered_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(triggered(QAction*)), this, SLOT(triggered_hook(QAction*)));
    }

  private slots:
    void triggered_hook(QAction* AnonParam1) {
      if ( triggered_event.func ) {
        typedef void (*func_type)(void *data, QActionH AnonParam1);
	(*(func_type)triggered_event.func)(triggered_event.data, (QActionH)AnonParam1);
      }
    }
  private:
    QHook triggered_event;
};


#endif
