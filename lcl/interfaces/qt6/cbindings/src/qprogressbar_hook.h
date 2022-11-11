//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QPROGRESSBAR_HOOK_H
#define QPROGRESSBAR_HOOK_H

#include <qprogressbar.h>

#include "qwidget_hook.h"

class QProgressBar_hook : public QWidget_hook {
  Q_OBJECT
  public:
    QProgressBar_hook(QObject *handle) : QWidget_hook(handle) {
      valueChanged_event.func = NULL;
    }
    void hook_valueChanged(QHook &hook) { 
      if ( !valueChanged_event.func )
        connect(handle, SIGNAL(valueChanged(int)), this, SLOT(valueChanged_hook(int)));
      valueChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(valueChanged(int)), this, SLOT(valueChanged_hook(int)));
    }

  private slots:
    void valueChanged_hook(int value) {
      if ( valueChanged_event.func ) {
        typedef void (*func_type)(void *data, int value);
	(*(func_type)valueChanged_event.func)(valueChanged_event.data, value);
      }
    }
  private:
    QHook valueChanged_event;
};


#endif
