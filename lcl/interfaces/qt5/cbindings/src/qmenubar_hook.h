//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QMENUBAR_HOOK_H
#define QMENUBAR_HOOK_H

#include <qmenubar.h>

#include "qwidget_hook.h"

class QMenuBar_hook : public QWidget_hook {
  Q_OBJECT
  public:
    QMenuBar_hook(QObject *handle) : QWidget_hook(handle) {
      triggered_event.func = NULL;
      hovered_event.func = NULL;
    }
    void hook_triggered(QHook &hook) { 
      if ( !triggered_event.func )
        connect(handle, SIGNAL(triggered(QAction*)), this, SLOT(triggered_hook(QAction*)));
      triggered_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(triggered(QAction*)), this, SLOT(triggered_hook(QAction*)));
    }
    void hook_hovered(QHook &hook) { 
      if ( !hovered_event.func )
        connect(handle, SIGNAL(hovered(QAction*)), this, SLOT(hovered_hook(QAction*)));
      hovered_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(hovered(QAction*)), this, SLOT(hovered_hook(QAction*)));
    }

  private slots:
    void triggered_hook(QAction* action) {
      if ( triggered_event.func ) {
        typedef void (*func_type)(void *data, QActionH action);
	(*(func_type)triggered_event.func)(triggered_event.data, (QActionH)action);
      }
    }
    void hovered_hook(QAction* action) {
      if ( hovered_event.func ) {
        typedef void (*func_type)(void *data, QActionH action);
	(*(func_type)hovered_event.func)(hovered_event.data, (QActionH)action);
      }
    }
  private:
    QHook triggered_event;
    QHook hovered_event;
};


#endif
