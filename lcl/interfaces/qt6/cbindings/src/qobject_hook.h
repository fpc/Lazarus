//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************




#ifndef QOBJECT_HOOK_H
#define QOBJECT_HOOK_H

#include <qobject.h>
#include "pascalbind.h"


class QObject_hook : public QObject {
  Q_OBJECT

  public:

  QObject_hook(QObject *handle) : QObject(NULL) {
    this->handle = handle;
    this->events.func = NULL;
    this->destroyed_event.func = NULL;
    connect(handle, SIGNAL(destroyed()), this, SLOT(destroyed_hook()));
  }

  virtual ~QObject_hook() {
    if (handle) {
      handle->removeEventFilter(this);
      handle->disconnect(this);
      handle = NULL;
    } 
  }

  void hook_events(QHook &hook) {
    if (handle) {
      if (!events.func) {
        handle->installEventFilter(this);
        events = hook;
      }
      if (!hook.func) handle->removeEventFilter(this);
      events = hook;
    }
  }

  void hook_destroyed(QHook &hook) {
    destroyed_event = hook;
  }

  protected:

    QObject *handle;

    virtual bool eventFilter(QObject *sender, QEvent *event) {
      if (events.func) {
        typedef bool (*func_type)(void *data, QObject *sender, QEvent *event);
        return (*(func_type)events.func)(events.data, sender, event);
      }
      else return false;
    }
  
  private slots:

    void destroyed_hook() {
      if ( destroyed_event.func ) {
        typedef void (*func_type)(void *data);
        (*(func_type)destroyed_event.func)(destroyed_event.data);
      }
      handle = NULL;
    }
  
  private:
    QHook events;
    QHook destroyed_event;
};

#endif
