//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QLCDNUMBER_HOOK_H
#define QLCDNUMBER_HOOK_H

#include <qlcdnumber.h>

#include "qframe_hook.h"

class QLCDNumber_hook : public QFrame_hook {
  Q_OBJECT
  public:
    QLCDNumber_hook(QObject *handle) : QFrame_hook(handle) {
      overflow_event.func = NULL;
    }
    void hook_overflow(QHook &hook) { 
      if ( !overflow_event.func )
        connect(handle, SIGNAL(overflow()), this, SLOT(overflow_hook()));
      overflow_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(overflow()), this, SLOT(overflow_hook()));
    }

  private slots:
    void overflow_hook() {
      if ( overflow_event.func ) {
        typedef void (*func_type)(void *data);
	(*(func_type)overflow_event.func)(overflow_event.data);
      }
    }
  private:
    QHook overflow_event;
};


#endif
