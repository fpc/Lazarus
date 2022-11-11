//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QCLIPBOARD_HOOK_H
#define QCLIPBOARD_HOOK_H

#include <qclipboard.h>

#include "qobject_hook.h"

class QClipboard_hook : public QObject_hook {
  Q_OBJECT
  public:
    QClipboard_hook(QObject *handle) : QObject_hook(handle) {
      changed_event.func = NULL;
      selectionChanged_event.func = NULL;
      findBufferChanged_event.func = NULL;
      dataChanged_event.func = NULL;
    }
    void hook_changed(QHook &hook) { 
      if ( !changed_event.func )
        connect(handle, SIGNAL(changed(QClipboard::Mode)), this, SLOT(changed_hook(QClipboard::Mode)));
      changed_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(changed(QClipboard::Mode)), this, SLOT(changed_hook(QClipboard::Mode)));
    }
    void hook_selectionChanged(QHook &hook) { 
      if ( !selectionChanged_event.func )
        connect(handle, SIGNAL(selectionChanged()), this, SLOT(selectionChanged_hook()));
      selectionChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(selectionChanged()), this, SLOT(selectionChanged_hook()));
    }
    void hook_findBufferChanged(QHook &hook) { 
      if ( !findBufferChanged_event.func )
        connect(handle, SIGNAL(findBufferChanged()), this, SLOT(findBufferChanged_hook()));
      findBufferChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(findBufferChanged()), this, SLOT(findBufferChanged_hook()));
    }
    void hook_dataChanged(QHook &hook) { 
      if ( !dataChanged_event.func )
        connect(handle, SIGNAL(dataChanged()), this, SLOT(dataChanged_hook()));
      dataChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(dataChanged()), this, SLOT(dataChanged_hook()));
    }

  private slots:
    void changed_hook(QClipboard::Mode mode) {
      if ( changed_event.func ) {
        typedef void (*func_type)(void *data, QClipboard::Mode mode);
	(*(func_type)changed_event.func)(changed_event.data, mode);
      }
    }
    void selectionChanged_hook() {
      if ( selectionChanged_event.func ) {
        typedef void (*func_type)(void *data);
	(*(func_type)selectionChanged_event.func)(selectionChanged_event.data);
      }
    }
    void findBufferChanged_hook() {
      if ( findBufferChanged_event.func ) {
        typedef void (*func_type)(void *data);
	(*(func_type)findBufferChanged_event.func)(findBufferChanged_event.data);
      }
    }
    void dataChanged_hook() {
      if ( dataChanged_event.func ) {
        typedef void (*func_type)(void *data);
	(*(func_type)dataChanged_event.func)(dataChanged_event.data);
      }
    }
  private:
    QHook changed_event;
    QHook selectionChanged_event;
    QHook findBufferChanged_event;
    QHook dataChanged_event;
};


#endif
