//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QDIALOG_HOOK_H
#define QDIALOG_HOOK_H

#include <qdialog.h>

#include "qwidget_hook.h"

class QDialog_hook : public QWidget_hook {
  Q_OBJECT
  public:
    QDialog_hook(QObject *handle) : QWidget_hook(handle) {
      finished_event.func = NULL;
      accepted_event.func = NULL;
      rejected_event.func = NULL;
    }
    void hook_finished(QHook &hook) { 
      if ( !finished_event.func )
        connect(handle, SIGNAL(finished(int)), this, SLOT(finished_hook(int)));
      finished_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(finished(int)), this, SLOT(finished_hook(int)));
    }
    void hook_accepted(QHook &hook) { 
      if ( !accepted_event.func )
        connect(handle, SIGNAL(accepted()), this, SLOT(accepted_hook()));
      accepted_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(accepted()), this, SLOT(accepted_hook()));
    }
    void hook_rejected(QHook &hook) { 
      if ( !rejected_event.func )
        connect(handle, SIGNAL(rejected()), this, SLOT(rejected_hook()));
      rejected_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(rejected()), this, SLOT(rejected_hook()));
    }

  private slots:
    void finished_hook(int result) {
      if ( finished_event.func ) {
        typedef void (*func_type)(void *data, int result);
	(*(func_type)finished_event.func)(finished_event.data, result);
      }
    }
    void accepted_hook() {
      if ( accepted_event.func ) {
        typedef void (*func_type)(void *data);
	(*(func_type)accepted_event.func)(accepted_event.data);
      }
    }
    void rejected_hook() {
      if ( rejected_event.func ) {
        typedef void (*func_type)(void *data);
	(*(func_type)rejected_event.func)(rejected_event.data);
      }
    }
  private:
    QHook finished_event;
    QHook accepted_event;
    QHook rejected_event;
};


#endif
