//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QNETWORKACCESSMANAGER_HOOK_H
#define QNETWORKACCESSMANAGER_HOOK_H

#include <qnetworkaccessmanager.h>

#include "qobject_hook.h"

class QNetworkAccessManager_hook : public QObject_hook {
  Q_OBJECT
  public:
    QNetworkAccessManager_hook(QObject *handle) : QObject_hook(handle) {
      proxyAuthenticationRequired_event.func = NULL;
      authenticationRequired_event.func = NULL;
      finished_event.func = NULL;
      encrypted_event.func = NULL;
      networkSessionConnected_event.func = NULL;
      networkAccessibleChanged_event.func = NULL;
    }
    void hook_proxyAuthenticationRequired(QHook &hook) { 
      if ( !proxyAuthenticationRequired_event.func )
        connect(handle, SIGNAL(proxyAuthenticationRequired(const QNetworkProxy&, QAuthenticator*)), this, SLOT(proxyAuthenticationRequired_hook(const QNetworkProxy&, QAuthenticator*)));
      proxyAuthenticationRequired_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(proxyAuthenticationRequired(const QNetworkProxy&, QAuthenticator*)), this, SLOT(proxyAuthenticationRequired_hook(const QNetworkProxy&, QAuthenticator*)));
    }
    void hook_authenticationRequired(QHook &hook) { 
      if ( !authenticationRequired_event.func )
        connect(handle, SIGNAL(authenticationRequired(QNetworkReply*, QAuthenticator*)), this, SLOT(authenticationRequired_hook(QNetworkReply*, QAuthenticator*)));
      authenticationRequired_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(authenticationRequired(QNetworkReply*, QAuthenticator*)), this, SLOT(authenticationRequired_hook(QNetworkReply*, QAuthenticator*)));
    }
    void hook_finished(QHook &hook) { 
      if ( !finished_event.func )
        connect(handle, SIGNAL(finished(QNetworkReply*)), this, SLOT(finished_hook(QNetworkReply*)));
      finished_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(finished(QNetworkReply*)), this, SLOT(finished_hook(QNetworkReply*)));
    }
    void hook_encrypted(QHook &hook) { 
      if ( !encrypted_event.func )
        connect(handle, SIGNAL(encrypted(QNetworkReply*)), this, SLOT(encrypted_hook(QNetworkReply*)));
      encrypted_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(encrypted(QNetworkReply*)), this, SLOT(encrypted_hook(QNetworkReply*)));
    }
    void hook_networkSessionConnected(QHook &hook) { 
      if ( !networkSessionConnected_event.func )
        connect(handle, SIGNAL(networkSessionConnected()), this, SLOT(networkSessionConnected_hook()));
      networkSessionConnected_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(networkSessionConnected()), this, SLOT(networkSessionConnected_hook()));
    }
    void hook_networkAccessibleChanged(QHook &hook) { 
      if ( !networkAccessibleChanged_event.func )
        connect(handle, SIGNAL(networkAccessibleChanged(QNetworkAccessManager::NetworkAccessibility)), this, SLOT(networkAccessibleChanged_hook(QNetworkAccessManager::NetworkAccessibility)));
      networkAccessibleChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(networkAccessibleChanged(QNetworkAccessManager::NetworkAccessibility)), this, SLOT(networkAccessibleChanged_hook(QNetworkAccessManager::NetworkAccessibility)));
    }

  private slots:
    void proxyAuthenticationRequired_hook(const QNetworkProxy& proxy, QAuthenticator* authenticator) {
      if ( proxyAuthenticationRequired_event.func ) {
        typedef void (*func_type)(void *data, const QNetworkProxyH proxy, QAuthenticatorH authenticator);
	(*(func_type)proxyAuthenticationRequired_event.func)(proxyAuthenticationRequired_event.data, (const QNetworkProxyH)&proxy, (QAuthenticatorH)authenticator);
      }
    }
    void authenticationRequired_hook(QNetworkReply* reply, QAuthenticator* authenticator) {
      if ( authenticationRequired_event.func ) {
        typedef void (*func_type)(void *data, QNetworkReplyH reply, QAuthenticatorH authenticator);
	(*(func_type)authenticationRequired_event.func)(authenticationRequired_event.data, (QNetworkReplyH)reply, (QAuthenticatorH)authenticator);
      }
    }
    void finished_hook(QNetworkReply* reply) {
      if ( finished_event.func ) {
        typedef void (*func_type)(void *data, QNetworkReplyH reply);
	(*(func_type)finished_event.func)(finished_event.data, (QNetworkReplyH)reply);
      }
    }
    void encrypted_hook(QNetworkReply* reply) {
      if ( encrypted_event.func ) {
        typedef void (*func_type)(void *data, QNetworkReplyH reply);
	(*(func_type)encrypted_event.func)(encrypted_event.data, (QNetworkReplyH)reply);
      }
    }
    void networkSessionConnected_hook() {
      if ( networkSessionConnected_event.func ) {
        typedef void (*func_type)(void *data);
	(*(func_type)networkSessionConnected_event.func)(networkSessionConnected_event.data);
      }
    }
    void networkAccessibleChanged_hook(QNetworkAccessManager::NetworkAccessibility accessible) {
      if ( networkAccessibleChanged_event.func ) {
        typedef void (*func_type)(void *data, QNetworkAccessManager::NetworkAccessibility accessible);
	(*(func_type)networkAccessibleChanged_event.func)(networkAccessibleChanged_event.data, accessible);
      }
    }
  private:
    QHook proxyAuthenticationRequired_event;
    QHook authenticationRequired_event;
    QHook finished_event;
    QHook encrypted_event;
    QHook networkSessionConnected_event;
    QHook networkAccessibleChanged_event;
};


#endif
