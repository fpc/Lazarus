//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QWIDGET_HOOK_H
#define QWIDGET_HOOK_H

#include <qwidget.h>

#include "qobject_hook.h"

class QWidget_hook : public QObject_hook {
  Q_OBJECT
  public:
    QWidget_hook(QObject *handle) : QObject_hook(handle) {
      customContextMenuRequested_event.func = NULL;
    }
    void hook_customContextMenuRequested(QHook &hook) { 
      if ( !customContextMenuRequested_event.func )
        connect(handle, SIGNAL(customContextMenuRequested(const QPoint&)), this, SLOT(customContextMenuRequested_hook(const QPoint&)));
      customContextMenuRequested_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(customContextMenuRequested(const QPoint&)), this, SLOT(customContextMenuRequested_hook(const QPoint&)));
    }

  private slots:
    void customContextMenuRequested_hook(const QPoint& pos) {
      if ( customContextMenuRequested_event.func ) {
        typedef void (*func_type)(void *data, const QPointH pos);
	(*(func_type)customContextMenuRequested_event.func)(customContextMenuRequested_event.data, (const QPointH)&pos);
      }
    }
  private:
    QHook customContextMenuRequested_event;
};


#endif
