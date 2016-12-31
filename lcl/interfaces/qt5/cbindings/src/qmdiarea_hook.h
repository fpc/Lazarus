//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QMDIAREA_HOOK_H
#define QMDIAREA_HOOK_H

#include <qmdiarea.h>

#include "qabstractscrollarea_hook.h"

class QMdiArea_hook : public QAbstractScrollArea_hook {
  Q_OBJECT
  public:
    QMdiArea_hook(QObject *handle) : QAbstractScrollArea_hook(handle) {
      subWindowActivated_event.func = NULL;
    }
    void hook_subWindowActivated(QHook &hook) { 
      if ( !subWindowActivated_event.func )
        connect(handle, SIGNAL(subWindowActivated(QMdiSubWindow*)), this, SLOT(subWindowActivated_hook(QMdiSubWindow*)));
      subWindowActivated_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(subWindowActivated(QMdiSubWindow*)), this, SLOT(subWindowActivated_hook(QMdiSubWindow*)));
    }

  private slots:
    void subWindowActivated_hook(QMdiSubWindow* AnonParam1) {
      if ( subWindowActivated_event.func ) {
        typedef void (*func_type)(void *data, QMdiSubWindowH AnonParam1);
	(*(func_type)subWindowActivated_event.func)(subWindowActivated_event.data, (QMdiSubWindowH)AnonParam1);
      }
    }
  private:
    QHook subWindowActivated_event;
};


#endif
