//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QIODEVICE_HOOK_H
#define QIODEVICE_HOOK_H

#include <qiodevice.h>

#include "qobject_hook.h"

class QIODevice_hook : public QObject_hook {
  Q_OBJECT
  public:
    QIODevice_hook(QObject *handle) : QObject_hook(handle) {
      readyRead_event.func = NULL;
      bytesWritten_event.func = NULL;
      aboutToClose_event.func = NULL;
      readChannelFinished_event.func = NULL;
    }
    void hook_readyRead(QHook &hook) { 
      if ( !readyRead_event.func )
        connect(handle, SIGNAL(readyRead()), this, SLOT(readyRead_hook()));
      readyRead_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(readyRead()), this, SLOT(readyRead_hook()));
    }
    void hook_bytesWritten(QHook &hook) { 
      if ( !bytesWritten_event.func )
        connect(handle, SIGNAL(bytesWritten(qint64)), this, SLOT(bytesWritten_hook(qint64)));
      bytesWritten_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(bytesWritten(qint64)), this, SLOT(bytesWritten_hook(qint64)));
    }
    void hook_aboutToClose(QHook &hook) { 
      if ( !aboutToClose_event.func )
        connect(handle, SIGNAL(aboutToClose()), this, SLOT(aboutToClose_hook()));
      aboutToClose_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(aboutToClose()), this, SLOT(aboutToClose_hook()));
    }
    void hook_readChannelFinished(QHook &hook) { 
      if ( !readChannelFinished_event.func )
        connect(handle, SIGNAL(readChannelFinished()), this, SLOT(readChannelFinished_hook()));
      readChannelFinished_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(readChannelFinished()), this, SLOT(readChannelFinished_hook()));
    }

  private slots:
    void readyRead_hook() {
      if ( readyRead_event.func ) {
        typedef void (*func_type)(void *data);
	(*(func_type)readyRead_event.func)(readyRead_event.data);
      }
    }
    void bytesWritten_hook(qint64 bytes) {
      if ( bytesWritten_event.func ) {
        typedef void (*func_type)(void *data, qint64 bytes);
	(*(func_type)bytesWritten_event.func)(bytesWritten_event.data, bytes);
      }
    }
    void aboutToClose_hook() {
      if ( aboutToClose_event.func ) {
        typedef void (*func_type)(void *data);
	(*(func_type)aboutToClose_event.func)(aboutToClose_event.data);
      }
    }
    void readChannelFinished_hook() {
      if ( readChannelFinished_event.func ) {
        typedef void (*func_type)(void *data);
	(*(func_type)readChannelFinished_event.func)(readChannelFinished_event.data);
      }
    }
  private:
    QHook readyRead_event;
    QHook bytesWritten_event;
    QHook aboutToClose_event;
    QHook readChannelFinished_event;
};


#endif
