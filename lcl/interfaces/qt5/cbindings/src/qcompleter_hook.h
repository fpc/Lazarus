//******************************************************************************
//  Copyright (c) 2022 by Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QCOMPLETER_HOOK_H
#define QCOMPLETER_HOOK_H

#include <qcompleter.h>

#include "qobject_hook.h"

class QCompleter_hook : public QObject_hook {
  Q_OBJECT
  public:
    QCompleter_hook(QObject *handle) : QObject_hook(handle) {
      activated_event.func = NULL;
      activated2_event.func = NULL;
      highlighted_event.func = NULL;
      highlighted2_event.func = NULL;
    }

    void hook_activated(QHook &hook) {
      if ( !activated_event.func )
        connect(handle, SIGNAL(activated(const QModelIndex&)), this, SLOT(activated_hook(const QModelIndex&)));
      activated_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(activated(const QModelIndex&)), this, SLOT(activated_hook(const QModelIndex&)));
    }

    void hook_activated2(QHook &hook) {
      if ( !activated2_event.func )
        connect(handle, SIGNAL(activated(const QString&)), this, SLOT(activated2_hook(const QString&)));
      activated2_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(activated(const QString&)), this, SLOT(activated2_hook(const QString&)));
    }

    void hook_highlighted(QHook &hook) {
      if ( !highlighted_event.func )
        connect(handle, SIGNAL(highlighted(const QModelIndex&)), this, SLOT(highlighted_hook(const QModelIndex&)));
      highlighted_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(highlighted(const QModelIndex&)), this, SLOT(highlighted_hook(const QModelIndex&)));
    }

    void hook_highlighted2(QHook &hook) {
      if ( !highlighted2_event.func )
        connect(handle, SIGNAL(highlighted(const QString&)), this, SLOT(highlighted2_hook(const QString&)));
      highlighted2_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(highlighted(const QString&)), this, SLOT(highlighted2_hook(const QString&)));
    }

  private slots:

    void activated_hook(const QModelIndex& index) {
      if ( activated_event.func ) {
        typedef void (*func_type)(void *data, const QModelIndexH index);
      	(*(func_type)activated_event.func)(activated_event.data, (const QModelIndexH)&index);
      }
    }

    void activated2_hook(const QString& AnonParam1) {
      if ( activated2_event.func ) {
        typedef void (*func_type)(void *data, PWideString AnonParam1);
	      PWideString t_AnonParam1;
	      initializePWideString(t_AnonParam1);
	      copyQStringToPWideString(AnonParam1, t_AnonParam1);
	      (*(func_type)activated2_event.func)(activated2_event.data, t_AnonParam1);
	      finalizePWideString(t_AnonParam1);
      }
    }

    void highlighted_hook(const QModelIndex& index) {
      if ( highlighted_event.func ) {
        typedef void (*func_type)(void *data, const QModelIndexH index);
      	(*(func_type)highlighted_event.func)(highlighted_event.data, (const QModelIndexH)&index);
      }
    }

    void highlighted2_hook(const QString& AnonParam1) {
      if ( highlighted2_event.func ) {
        typedef void (*func_type)(void *data, PWideString AnonParam1);
	      PWideString t_AnonParam1;
	      initializePWideString(t_AnonParam1);
	      copyQStringToPWideString(AnonParam1, t_AnonParam1);
	      (*(func_type)highlighted2_event.func)(highlighted2_event.data, t_AnonParam1);
	      finalizePWideString(t_AnonParam1);
      }
    }
  private:
    QHook activated_event;
    QHook activated2_event;
    QHook highlighted_event;
    QHook highlighted2_event;
};


#endif
