//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QPROCESS_HOOK_H
#define QPROCESS_HOOK_H

#include <qprocess.h>

#include "qobject_hook.h"

class QProcessEnvironment_hook : public QObject_hook {
  Q_OBJECT
  public:
    QProcessEnvironment_hook(QObject *handle) : QObject_hook(handle) {
    }
};


#include "qiodevice_hook.h"

class QProcess_hook : public QIODevice_hook {
  Q_OBJECT
  public:
    QProcess_hook(QObject *handle) : QIODevice_hook(handle) {
      started_event.func = NULL;
      finished_event.func = NULL;
      finished2_event.func = NULL;
      error_event.func = NULL;
      stateChanged_event.func = NULL;
      readyReadStandardOutput_event.func = NULL;
      readyReadStandardError_event.func = NULL;
    }
    void hook_started(QHook &hook) { 
      if ( !started_event.func )
        connect(handle, SIGNAL(started()), this, SLOT(started_hook()));
      started_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(started()), this, SLOT(started_hook()));
    }
    void hook_finished(QHook &hook) { 
      if ( !finished_event.func )
        connect(handle, SIGNAL(finished(int)), this, SLOT(finished_hook(int)));
      finished_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(finished(int)), this, SLOT(finished_hook(int)));
    }
    void hook_finished2(QHook &hook) { 
      if ( !finished2_event.func )
        connect(handle, SIGNAL(finished(int, QProcess::ExitStatus)), this, SLOT(finished2_hook(int, QProcess::ExitStatus)));
      finished2_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(finished(int, QProcess::ExitStatus)), this, SLOT(finished2_hook(int, QProcess::ExitStatus)));
    }
    void hook_error(QHook &hook) { 
      if ( !error_event.func )
        connect(handle, SIGNAL(error(QProcess::ProcessError)), this, SLOT(error_hook(QProcess::ProcessError)));
      error_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(error(QProcess::ProcessError)), this, SLOT(error_hook(QProcess::ProcessError)));
    }
    void hook_stateChanged(QHook &hook) { 
      if ( !stateChanged_event.func )
        connect(handle, SIGNAL(stateChanged(QProcess::ProcessState)), this, SLOT(stateChanged_hook(QProcess::ProcessState)));
      stateChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(stateChanged(QProcess::ProcessState)), this, SLOT(stateChanged_hook(QProcess::ProcessState)));
    }
    void hook_readyReadStandardOutput(QHook &hook) { 
      if ( !readyReadStandardOutput_event.func )
        connect(handle, SIGNAL(readyReadStandardOutput()), this, SLOT(readyReadStandardOutput_hook()));
      readyReadStandardOutput_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(readyReadStandardOutput()), this, SLOT(readyReadStandardOutput_hook()));
    }
    void hook_readyReadStandardError(QHook &hook) { 
      if ( !readyReadStandardError_event.func )
        connect(handle, SIGNAL(readyReadStandardError()), this, SLOT(readyReadStandardError_hook()));
      readyReadStandardError_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(readyReadStandardError()), this, SLOT(readyReadStandardError_hook()));
    }

  private slots:
    void started_hook() {
      if ( started_event.func ) {
        typedef void (*func_type)(void *data);
	(*(func_type)started_event.func)(started_event.data);
      }
    }
    void finished_hook(int exitCode) {
      if ( finished_event.func ) {
        typedef void (*func_type)(void *data, int exitCode);
	(*(func_type)finished_event.func)(finished_event.data, exitCode);
      }
    }
    void finished2_hook(int exitCode, QProcess::ExitStatus exitStatus) {
      if ( finished2_event.func ) {
        typedef void (*func_type)(void *data, int exitCode, QProcess::ExitStatus exitStatus);
	(*(func_type)finished2_event.func)(finished2_event.data, exitCode, exitStatus);
      }
    }
    void error_hook(QProcess::ProcessError error) {
      if ( error_event.func ) {
        typedef void (*func_type)(void *data, QProcess::ProcessError error);
	(*(func_type)error_event.func)(error_event.data, error);
      }
    }
    void stateChanged_hook(QProcess::ProcessState state) {
      if ( stateChanged_event.func ) {
        typedef void (*func_type)(void *data, QProcess::ProcessState state);
	(*(func_type)stateChanged_event.func)(stateChanged_event.data, state);
      }
    }
    void readyReadStandardOutput_hook() {
      if ( readyReadStandardOutput_event.func ) {
        typedef void (*func_type)(void *data);
	(*(func_type)readyReadStandardOutput_event.func)(readyReadStandardOutput_event.data);
      }
    }
    void readyReadStandardError_hook() {
      if ( readyReadStandardError_event.func ) {
        typedef void (*func_type)(void *data);
	(*(func_type)readyReadStandardError_event.func)(readyReadStandardError_event.data);
      }
    }
  private:
    QHook started_event;
    QHook finished_event;
    QHook finished2_event;
    QHook error_event;
    QHook stateChanged_event;
    QHook readyReadStandardOutput_event;
    QHook readyReadStandardError_event;
};


#endif
