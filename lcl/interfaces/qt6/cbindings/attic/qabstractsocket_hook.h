//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QABSTRACTSOCKET_HOOK_H
#define QABSTRACTSOCKET_HOOK_H

#include <qabstractsocket.h>

#include "qiodevice_hook.h"

class QAbstractSocket_hook : public QIODevice_hook {
  Q_OBJECT
  public:
    QAbstractSocket_hook(QObject *handle) : QIODevice_hook(handle) {
      hostFound_event.func = NULL;
      connected_event.func = NULL;
      disconnected_event.func = NULL;
      stateChanged_event.func = NULL;
      error_event.func = NULL;
      proxyAuthenticationRequired_event.func = NULL;
    }
    void hook_hostFound(QHook &hook) { 
      if ( !hostFound_event.func )
        connect(handle, SIGNAL(hostFound()), this, SLOT(hostFound_hook()));
      hostFound_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(hostFound()), this, SLOT(hostFound_hook()));
    }
    void hook_connected(QHook &hook) { 
      if ( !connected_event.func )
        connect(handle, SIGNAL(connected()), this, SLOT(connected_hook()));
      connected_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(connected()), this, SLOT(connected_hook()));
    }
    void hook_disconnected(QHook &hook) { 
      if ( !disconnected_event.func )
        connect(handle, SIGNAL(disconnected()), this, SLOT(disconnected_hook()));
      disconnected_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(disconnected()), this, SLOT(disconnected_hook()));
    }
    void hook_stateChanged(QHook &hook) { 
      if ( !stateChanged_event.func )
        connect(handle, SIGNAL(stateChanged(QAbstractSocket::SocketState)), this, SLOT(stateChanged_hook(QAbstractSocket::SocketState)));
      stateChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(stateChanged(QAbstractSocket::SocketState)), this, SLOT(stateChanged_hook(QAbstractSocket::SocketState)));
    }
    void hook_error(QHook &hook) { 
      if ( !error_event.func )
        connect(handle, SIGNAL(error(QAbstractSocket::SocketError)), this, SLOT(error_hook(QAbstractSocket::SocketError)));
      error_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(error(QAbstractSocket::SocketError)), this, SLOT(error_hook(QAbstractSocket::SocketError)));
    }
    void hook_proxyAuthenticationRequired(QHook &hook) { 
      if ( !proxyAuthenticationRequired_event.func )
        connect(handle, SIGNAL(proxyAuthenticationRequired(const QNetworkProxy&, QAuthenticator*)), this, SLOT(proxyAuthenticationRequired_hook(const QNetworkProxy&, QAuthenticator*)));
      proxyAuthenticationRequired_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(proxyAuthenticationRequired(const QNetworkProxy&, QAuthenticator*)), this, SLOT(proxyAuthenticationRequired_hook(const QNetworkProxy&, QAuthenticator*)));
    }

  private slots:
    void hostFound_hook() {
      if ( hostFound_event.func ) {
        typedef void (*func_type)(void *data);
	(*(func_type)hostFound_event.func)(hostFound_event.data);
      }
    }
    void connected_hook() {
      if ( connected_event.func ) {
        typedef void (*func_type)(void *data);
	(*(func_type)connected_event.func)(connected_event.data);
      }
    }
    void disconnected_hook() {
      if ( disconnected_event.func ) {
        typedef void (*func_type)(void *data);
	(*(func_type)disconnected_event.func)(disconnected_event.data);
      }
    }
    void stateChanged_hook(QAbstractSocket::SocketState AnonParam1) {
      if ( stateChanged_event.func ) {
        typedef void (*func_type)(void *data, QAbstractSocket::SocketState AnonParam1);
	(*(func_type)stateChanged_event.func)(stateChanged_event.data, AnonParam1);
      }
    }
    void error_hook(QAbstractSocket::SocketError AnonParam1) {
      if ( error_event.func ) {
        typedef void (*func_type)(void *data, QAbstractSocket::SocketError AnonParam1);
	(*(func_type)error_event.func)(error_event.data, AnonParam1);
      }
    }
    void proxyAuthenticationRequired_hook(const QNetworkProxy& proxy, QAuthenticator* authenticator) {
      if ( proxyAuthenticationRequired_event.func ) {
        typedef void (*func_type)(void *data, const QNetworkProxyH proxy, QAuthenticatorH authenticator);
	(*(func_type)proxyAuthenticationRequired_event.func)(proxyAuthenticationRequired_event.data, (const QNetworkProxyH)&proxy, (QAuthenticatorH)authenticator);
      }
    }
  private:
    QHook hostFound_event;
    QHook connected_event;
    QHook disconnected_event;
    QHook stateChanged_event;
    QHook error_event;
    QHook proxyAuthenticationRequired_event;
};


#endif
