//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QSYSTEMTRAYICON_HOOK_H
#define QSYSTEMTRAYICON_HOOK_H

#include <qsystemtrayicon.h>

#include "qobject_hook.h"

class QSystemTrayIcon_hook : public QObject_hook {
  Q_OBJECT
  public:
    QSystemTrayIcon_hook(QObject *handle) : QObject_hook(handle) {
      activated_event.func = NULL;
      messageClicked_event.func = NULL;
    }
    void hook_activated(QHook &hook) { 
      if ( !activated_event.func )
        connect(handle, SIGNAL(activated(QSystemTrayIcon::ActivationReason)), this, SLOT(activated_hook(QSystemTrayIcon::ActivationReason)));
      activated_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(activated(QSystemTrayIcon::ActivationReason)), this, SLOT(activated_hook(QSystemTrayIcon::ActivationReason)));
    }
    void hook_messageClicked(QHook &hook) { 
      if ( !messageClicked_event.func )
        connect(handle, SIGNAL(messageClicked()), this, SLOT(messageClicked_hook()));
      messageClicked_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(messageClicked()), this, SLOT(messageClicked_hook()));
    }

  private slots:
    void activated_hook(QSystemTrayIcon::ActivationReason reason) {
      if ( activated_event.func ) {
        typedef void (*func_type)(void *data, QSystemTrayIcon::ActivationReason reason);
	(*(func_type)activated_event.func)(activated_event.data, reason);
      }
    }
    void messageClicked_hook() {
      if ( messageClicked_event.func ) {
        typedef void (*func_type)(void *data);
	(*(func_type)messageClicked_event.func)(messageClicked_event.data);
      }
    }
  private:
    QHook activated_event;
    QHook messageClicked_event;
};


#endif
