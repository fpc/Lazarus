//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QPROGRESSDIALOG_HOOK_H
#define QPROGRESSDIALOG_HOOK_H

#include <qprogressdialog.h>

#include "qdialog_hook.h"

class QProgressDialog_hook : public QDialog_hook {
  Q_OBJECT
  public:
    QProgressDialog_hook(QObject *handle) : QDialog_hook(handle) {
      canceled_event.func = NULL;
    }
    void hook_canceled(QHook &hook) { 
      if ( !canceled_event.func )
        connect(handle, SIGNAL(canceled()), this, SLOT(canceled_hook()));
      canceled_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(canceled()), this, SLOT(canceled_hook()));
    }

  private slots:
    void canceled_hook() {
      if ( canceled_event.func ) {
        typedef void (*func_type)(void *data);
	(*(func_type)canceled_event.func)(canceled_event.data);
      }
    }
  private:
    QHook canceled_event;
};


#endif
