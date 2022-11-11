//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QCHECKBOX_HOOK_H
#define QCHECKBOX_HOOK_H

#include <qcheckbox.h>

#include "qabstractbutton_hook.h"

class QCheckBox_hook : public QAbstractButton_hook {
  Q_OBJECT
  public:
    QCheckBox_hook(QObject *handle) : QAbstractButton_hook(handle) {
      stateChanged_event.func = NULL;
    }
    void hook_stateChanged(QHook &hook) { 
      if ( !stateChanged_event.func )
        connect(handle, SIGNAL(stateChanged(int)), this, SLOT(stateChanged_hook(int)));
      stateChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(stateChanged(int)), this, SLOT(stateChanged_hook(int)));
    }

  private slots:
    void stateChanged_hook(int AnonParam1) {
      if ( stateChanged_event.func ) {
        typedef void (*func_type)(void *data, int AnonParam1);
	(*(func_type)stateChanged_event.func)(stateChanged_event.data, AnonParam1);
      }
    }
  private:
    QHook stateChanged_event;
};


#endif
