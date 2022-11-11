//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QCOREAPPLICATION_HOOK_H
#define QCOREAPPLICATION_HOOK_H

#include <qcoreapplication.h>

#include "qobject_hook.h"

class QCoreApplication_hook : public QObject_hook {
  Q_OBJECT
  public:
    QCoreApplication_hook(QObject *handle) : QObject_hook(handle) {
      aboutToQuit_event.func = NULL;
      organizationNameChanged_event.func = NULL;
      organizationDomainChanged_event.func = NULL;
      applicationNameChanged_event.func = NULL;
      applicationVersionChanged_event.func = NULL;
    }
    void hook_aboutToQuit(QHook &hook) { 
      if ( !aboutToQuit_event.func )
        connect(handle, SIGNAL(aboutToQuit()), this, SLOT(aboutToQuit_hook()));
      aboutToQuit_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(aboutToQuit()), this, SLOT(aboutToQuit_hook()));
    }
    void hook_organizationNameChanged(QHook &hook) { 
      if ( !organizationNameChanged_event.func )
        connect(handle, SIGNAL(organizationNameChanged()), this, SLOT(organizationNameChanged_hook()));
      organizationNameChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(organizationNameChanged()), this, SLOT(organizationNameChanged_hook()));
    }
    void hook_organizationDomainChanged(QHook &hook) { 
      if ( !organizationDomainChanged_event.func )
        connect(handle, SIGNAL(organizationDomainChanged()), this, SLOT(organizationDomainChanged_hook()));
      organizationDomainChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(organizationDomainChanged()), this, SLOT(organizationDomainChanged_hook()));
    }
    void hook_applicationNameChanged(QHook &hook) { 
      if ( !applicationNameChanged_event.func )
        connect(handle, SIGNAL(applicationNameChanged()), this, SLOT(applicationNameChanged_hook()));
      applicationNameChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(applicationNameChanged()), this, SLOT(applicationNameChanged_hook()));
    }
    void hook_applicationVersionChanged(QHook &hook) { 
      if ( !applicationVersionChanged_event.func )
        connect(handle, SIGNAL(applicationVersionChanged()), this, SLOT(applicationVersionChanged_hook()));
      applicationVersionChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(applicationVersionChanged()), this, SLOT(applicationVersionChanged_hook()));
    }

  private slots:
    void aboutToQuit_hook() {
      if ( aboutToQuit_event.func ) {
        typedef void (*func_type)(void *data);
	(*(func_type)aboutToQuit_event.func)(aboutToQuit_event.data);
      }
    }
    void organizationNameChanged_hook() {
      if ( organizationNameChanged_event.func ) {
        typedef void (*func_type)(void *data);
	(*(func_type)organizationNameChanged_event.func)(organizationNameChanged_event.data);
      }
    }
    void organizationDomainChanged_hook() {
      if ( organizationDomainChanged_event.func ) {
        typedef void (*func_type)(void *data);
	(*(func_type)organizationDomainChanged_event.func)(organizationDomainChanged_event.data);
      }
    }
    void applicationNameChanged_hook() {
      if ( applicationNameChanged_event.func ) {
        typedef void (*func_type)(void *data);
	(*(func_type)applicationNameChanged_event.func)(applicationNameChanged_event.data);
      }
    }
    void applicationVersionChanged_hook() {
      if ( applicationVersionChanged_event.func ) {
        typedef void (*func_type)(void *data);
	(*(func_type)applicationVersionChanged_event.func)(applicationVersionChanged_event.data);
      }
    }
  private:
    QHook aboutToQuit_event;
    QHook organizationNameChanged_event;
    QHook organizationDomainChanged_event;
    QHook applicationNameChanged_event;
    QHook applicationVersionChanged_event;
};


#endif
