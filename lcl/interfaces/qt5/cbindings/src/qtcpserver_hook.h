//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QTCPSERVER_HOOK_H
#define QTCPSERVER_HOOK_H

#include <qtcpserver.h>

#include "qobject_hook.h"

class QTcpServer_hook : public QObject_hook {
  Q_OBJECT
  public:
    QTcpServer_hook(QObject *handle) : QObject_hook(handle) {
      newConnection_event.func = NULL;
      acceptError_event.func = NULL;
    }
    void hook_newConnection(QHook &hook) { 
      if ( !newConnection_event.func )
        connect(handle, SIGNAL(newConnection()), this, SLOT(newConnection_hook()));
      newConnection_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(newConnection()), this, SLOT(newConnection_hook()));
    }
    void hook_acceptError(QHook &hook) { 
      if ( !acceptError_event.func )
        connect(handle, SIGNAL(acceptError(QAbstractSocket::SocketError)), this, SLOT(acceptError_hook(QAbstractSocket::SocketError)));
      acceptError_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(acceptError(QAbstractSocket::SocketError)), this, SLOT(acceptError_hook(QAbstractSocket::SocketError)));
    }

  private slots:
    void newConnection_hook() {
      if ( newConnection_event.func ) {
        typedef void (*func_type)(void *data);
	(*(func_type)newConnection_event.func)(newConnection_event.data);
      }
    }
    void acceptError_hook(QAbstractSocket::SocketError socketError) {
      if ( acceptError_event.func ) {
        typedef void (*func_type)(void *data, QAbstractSocket::SocketError socketError);
	(*(func_type)acceptError_event.func)(acceptError_event.data, socketError);
      }
    }
  private:
    QHook newConnection_event;
    QHook acceptError_event;
};


#endif
