//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QGUIAPPLICATION_HOOK_H
#define QGUIAPPLICATION_HOOK_H

#include <qguiapplication.h>

#include "qcoreapplication_hook.h"

class QGuiApplication_hook : public QCoreApplication_hook {
  Q_OBJECT
  public:
    QGuiApplication_hook(QObject *handle) : QCoreApplication_hook(handle) {
      applicationStateChanged_event.func = NULL;
      fontDatabaseChanged_event.func = NULL;
      primaryScreenChanged_event.func = NULL;
      screenAdded_event.func = NULL;
      screenRemoved_event.func = NULL;
      lastWindowClosed_event.func = NULL;
      focusObjectChanged_event.func = NULL;
      focusWindowChanged_event.func = NULL;
      commitDataRequest_event.func = NULL;
      saveStateRequest_event.func = NULL;
    }
    void hook_applicationStateChanged(QHook &hook) {
      if ( !applicationStateChanged_event.func )
        connect(handle, SIGNAL(applicationStateChanged(Qt::ApplicationState)), this, SLOT(applicationStateChanged_hook(Qt::ApplicationState)));
      applicationStateChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(applicationStateChanged(Qt::ApplicationState)), this, SLOT(applicationStateChanged_hook(Qt::ApplicationState)));
    }

    void hook_fontDatabaseChanged(QHook &hook) { 
      if ( !fontDatabaseChanged_event.func )
        connect(handle, SIGNAL(fontDatabaseChanged()), this, SLOT(fontDatabaseChanged_hook()));
      fontDatabaseChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(fontDatabaseChanged()), this, SLOT(fontDatabaseChanged_hook()));
    }
    void hook_primaryScreenChanged(QHook &hook) {
      if ( !primaryScreenChanged_event.func )
        connect(handle, SIGNAL(primaryScreenChanged(QScreen*)), this, SLOT(primaryScreenChanged_hook(QScreen*)));
      primaryScreenChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(primaryScreenChanged(QScreen*)), this, SLOT(primaryScreenChanged_hook(QScreen*)));
    }
    void hook_screenAdded(QHook &hook) { 
      if ( !screenAdded_event.func )
        connect(handle, SIGNAL(screenAdded(QScreen*)), this, SLOT(screenAdded_hook(QScreen*)));
      screenAdded_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(screenAdded(QScreen*)), this, SLOT(screenAdded_hook(QScreen*)));
    }
    void hook_screenRemoved(QHook &hook) {
      if ( !screenRemoved_event.func )
        connect(handle, SIGNAL(screenRemoved(QScreen*)), this, SLOT(screenRemoved_hook(QScreen*)));
      screenRemoved_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(screenRemoved(QScreen*)), this, SLOT(screenRemoved_hook(QScreen*)));
    }
    void hook_lastWindowClosed(QHook &hook) { 
      if ( !lastWindowClosed_event.func )
        connect(handle, SIGNAL(lastWindowClosed()), this, SLOT(lastWindowClosed_hook()));
      lastWindowClosed_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(lastWindowClosed()), this, SLOT(lastWindowClosed_hook()));
    }
    void hook_focusObjectChanged(QHook &hook) { 
      if ( !focusObjectChanged_event.func )
        connect(handle, SIGNAL(focusObjectChanged(QObject*)), this, SLOT(focusObjectChanged_hook(QObject*)));
      focusObjectChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(focusObjectChanged(QObject*)), this, SLOT(focusObjectChanged_hook(QObject*)));
    }
    void hook_focusWindowChanged(QHook &hook) { 
      if ( !focusWindowChanged_event.func )
        connect(handle, SIGNAL(focusWindowChanged(QWindow*)), this, SLOT(focusWindowChanged_hook(QWindow*)));
      focusWindowChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(focusWindowChanged(QWindow*)), this, SLOT(focusWindowChanged_hook(QWindow*)));
    }
    void hook_commitDataRequest(QHook &hook) { 
      if ( !commitDataRequest_event.func )
        connect(handle, SIGNAL(commitDataRequest(QSessionManager&)), this, SLOT(commitDataRequest_hook(QSessionManager&)));
      commitDataRequest_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(commitDataRequest(QSessionManager&)), this, SLOT(commitDataRequest_hook(QSessionManager&)));
    }
    void hook_saveStateRequest(QHook &hook) { 
      if ( !saveStateRequest_event.func )
        connect(handle, SIGNAL(saveStateRequest(QSessionManager&)), this, SLOT(saveStateRequest_hook(QSessionManager&)));
      saveStateRequest_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(saveStateRequest(QSessionManager&)), this, SLOT(saveStateRequest_hook(QSessionManager&)));
    }

  private slots:
    void applicationStateChanged_hook(Qt::ApplicationState state) {
      if ( applicationStateChanged_event.func ) {
        typedef void (*func_type)(void *data, Qt::ApplicationState state);
	(*(func_type)applicationStateChanged_event.func)(applicationStateChanged_event.data, (Qt::ApplicationState)state);
      }
    }
    void fontDatabaseChanged_hook() {
      if ( fontDatabaseChanged_event.func ) {
        typedef void (*func_type)(void *data);
	(*(func_type)fontDatabaseChanged_event.func)(fontDatabaseChanged_event.data);
      }
    }
    void primaryScreenChanged_hook(QScreen* screen) {
      if ( primaryScreenChanged_event.func ) {
        typedef void (*func_type)(void *data, QScreenH screen);
	(*(func_type)primaryScreenChanged_event.func)(primaryScreenChanged_event.data, (QScreenH)screen);
      }
    }
    void screenAdded_hook(QScreen* screen) {
      if ( screenAdded_event.func ) {
        typedef void (*func_type)(void *data, QScreenH screen);
	(*(func_type)screenAdded_event.func)(screenAdded_event.data, (QScreenH)screen);
      }
    }
    void screenRemoved_hook(QScreen* screen) {
      if ( screenRemoved_event.func ) {
        typedef void (*func_type)(void *data, QScreenH screen);
	(*(func_type)screenRemoved_event.func)(screenRemoved_event.data, (QScreenH)screen);
      }
    }
    void lastWindowClosed_hook() {
      if ( lastWindowClosed_event.func ) {
        typedef void (*func_type)(void *data);
	(*(func_type)lastWindowClosed_event.func)(lastWindowClosed_event.data);
      }
    }
    void focusObjectChanged_hook(QObject* focusObject) {
      if ( focusObjectChanged_event.func ) {
        typedef void (*func_type)(void *data, QObjectH focusObject);
	(*(func_type)focusObjectChanged_event.func)(focusObjectChanged_event.data, (QObjectH)focusObject);
      }
    }
    void focusWindowChanged_hook(QWindow* focusWindow) {
      if ( focusWindowChanged_event.func ) {
        typedef void (*func_type)(void *data, QWindowH focusWindow);
	(*(func_type)focusWindowChanged_event.func)(focusWindowChanged_event.data, (QWindowH)focusWindow);
      }
    }
    void commitDataRequest_hook(QSessionManager& sessionManager) {
      if ( commitDataRequest_event.func ) {
        typedef void (*func_type)(void *data, QSessionManagerH sessionManager);
	(*(func_type)commitDataRequest_event.func)(commitDataRequest_event.data, (QSessionManagerH)&sessionManager);
      }
    }
    void saveStateRequest_hook(QSessionManager& sessionManager) {
      if ( saveStateRequest_event.func ) {
        typedef void (*func_type)(void *data, QSessionManagerH sessionManager);
	(*(func_type)saveStateRequest_event.func)(saveStateRequest_event.data, (QSessionManagerH)&sessionManager);
      }
    }
  private:
    QHook applicationStateChanged_event;
    QHook fontDatabaseChanged_event;
    QHook primaryScreenChanged_event;
    QHook screenAdded_event;
    QHook screenRemoved_event;
    QHook lastWindowClosed_event;
    QHook focusObjectChanged_event;
    QHook focusWindowChanged_event;
    QHook commitDataRequest_event;
    QHook saveStateRequest_event;
};


#endif
