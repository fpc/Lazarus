//******************************************************************************
//  Copyright (c) 2005-2022 by Matteo Salvi
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************

#ifndef Q_NATIVEEVENTFILTER_HOOK_H
#define Q_NATIVEEVENTFILTER_HOOK_H

#include <qcoreapplication.h>
#include <qabstractnativeeventfilter.h>
#include "pascalbind.h"

#if QT_VERSION >= QT_VERSION_CHECK(6, 2, 0)
	#define _NATIVE_EVENT_RESULT qintptr
#else
	#define _NATIVE_EVENT_RESULT long
#endif


class Q_NativeEventFilter_hook : public QAbstractNativeEventFilter {
  //Q_OBJECT

  public:  
	bool nativeEventFilter(const QByteArray &eventType, void *message, _NATIVE_EVENT_RESULT *result) override;

  Q_NativeEventFilter_hook(QCoreApplication *handle) : QAbstractNativeEventFilter() {
    this->handle = handle;
    this->events.func = NULL;
    this->destroyed_event.func = NULL;
    //connect(handle, SIGNAL(destroyed()), this, SLOT(destroyed_hook()));
  }

  virtual ~Q_NativeEventFilter_hook() {
    if (handle) {
      handle->removeNativeEventFilter(this);
      handle = NULL;
    }
  }

  void hook_installfilter(QHook &hook) {
    if (handle) {
      if (!events.func) {
        handle->installNativeEventFilter(this);
        events = hook;
      }
      if (!hook.func)
        handle->removeNativeEventFilter(this);
      events = hook;
    }
  }
  void hook_removefilter() {
    if (handle) {
      handle->removeNativeEventFilter(this);   
      events.func = NULL;
    }
  }

  void hook_destroyed(QHook &hook) {
    destroyed_event = hook;
  }

  protected:

    QCoreApplication *handle;

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


bool Q_NativeEventFilter_hook::nativeEventFilter(const QByteArray &eventType, void *message, _NATIVE_EVENT_RESULT *result) {
  if (events.func) {
    Q_NativeEventFilter_hook* sender = this;
    typedef bool (*func_type)(void *data, Q_NativeEventFilter_hook* sender, const QByteArray &eventType, void *message);
    return (*(func_type)events.func)(events.data, sender, eventType, message);
  }
  return false;
}

#endif
