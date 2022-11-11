//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QBUTTONGROUP_HOOK_H
#define QBUTTONGROUP_HOOK_H

#include <qbuttongroup.h>

#include "qobject_hook.h"

class QButtonGroup_hook : public QObject_hook {
  Q_OBJECT
  public:
    QButtonGroup_hook(QObject *handle) : QObject_hook(handle) {
      buttonClicked_event.func = NULL;
      buttonClicked2_event.func = NULL;
      buttonPressed_event.func = NULL;
      buttonPressed2_event.func = NULL;
      buttonReleased_event.func = NULL;
      buttonReleased2_event.func = NULL;
    }
    void hook_buttonClicked(QHook &hook) { 
      if ( !buttonClicked_event.func )
        connect(handle, SIGNAL(buttonClicked(QAbstractButton*)), this, SLOT(buttonClicked_hook(QAbstractButton*)));
      buttonClicked_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(buttonClicked(QAbstractButton*)), this, SLOT(buttonClicked_hook(QAbstractButton*)));
    }
    void hook_buttonClicked2(QHook &hook) { 
      if ( !buttonClicked2_event.func )
        connect(handle, SIGNAL(buttonClicked(int)), this, SLOT(buttonClicked2_hook(int)));
      buttonClicked2_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(buttonClicked(int)), this, SLOT(buttonClicked2_hook(int)));
    }
    void hook_buttonPressed(QHook &hook) { 
      if ( !buttonPressed_event.func )
        connect(handle, SIGNAL(buttonPressed(QAbstractButton*)), this, SLOT(buttonPressed_hook(QAbstractButton*)));
      buttonPressed_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(buttonPressed(QAbstractButton*)), this, SLOT(buttonPressed_hook(QAbstractButton*)));
    }
    void hook_buttonPressed2(QHook &hook) { 
      if ( !buttonPressed2_event.func )
        connect(handle, SIGNAL(buttonPressed(int)), this, SLOT(buttonPressed2_hook(int)));
      buttonPressed2_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(buttonPressed(int)), this, SLOT(buttonPressed2_hook(int)));
    }
    void hook_buttonReleased(QHook &hook) { 
      if ( !buttonReleased_event.func )
        connect(handle, SIGNAL(buttonReleased(QAbstractButton*)), this, SLOT(buttonReleased_hook(QAbstractButton*)));
      buttonReleased_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(buttonReleased(QAbstractButton*)), this, SLOT(buttonReleased_hook(QAbstractButton*)));
    }
    void hook_buttonReleased2(QHook &hook) { 
      if ( !buttonReleased2_event.func )
        connect(handle, SIGNAL(buttonReleased(int)), this, SLOT(buttonReleased2_hook(int)));
      buttonReleased2_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(buttonReleased(int)), this, SLOT(buttonReleased2_hook(int)));
    }

  private slots:
    void buttonClicked_hook(QAbstractButton* AnonParam1) {
      if ( buttonClicked_event.func ) {
        typedef void (*func_type)(void *data, QAbstractButtonH AnonParam1);
	(*(func_type)buttonClicked_event.func)(buttonClicked_event.data, (QAbstractButtonH)AnonParam1);
      }
    }
    void buttonClicked2_hook(int AnonParam1) {
      if ( buttonClicked2_event.func ) {
        typedef void (*func_type)(void *data, int AnonParam1);
	(*(func_type)buttonClicked2_event.func)(buttonClicked2_event.data, AnonParam1);
      }
    }
    void buttonPressed_hook(QAbstractButton* AnonParam1) {
      if ( buttonPressed_event.func ) {
        typedef void (*func_type)(void *data, QAbstractButtonH AnonParam1);
	(*(func_type)buttonPressed_event.func)(buttonPressed_event.data, (QAbstractButtonH)AnonParam1);
      }
    }
    void buttonPressed2_hook(int AnonParam1) {
      if ( buttonPressed2_event.func ) {
        typedef void (*func_type)(void *data, int AnonParam1);
	(*(func_type)buttonPressed2_event.func)(buttonPressed2_event.data, AnonParam1);
      }
    }
    void buttonReleased_hook(QAbstractButton* AnonParam1) {
      if ( buttonReleased_event.func ) {
        typedef void (*func_type)(void *data, QAbstractButtonH AnonParam1);
	(*(func_type)buttonReleased_event.func)(buttonReleased_event.data, (QAbstractButtonH)AnonParam1);
      }
    }
    void buttonReleased2_hook(int AnonParam1) {
      if ( buttonReleased2_event.func ) {
        typedef void (*func_type)(void *data, int AnonParam1);
	(*(func_type)buttonReleased2_event.func)(buttonReleased2_event.data, AnonParam1);
      }
    }
  private:
    QHook buttonClicked_event;
    QHook buttonClicked2_event;
    QHook buttonPressed_event;
    QHook buttonPressed2_event;
    QHook buttonReleased_event;
    QHook buttonReleased2_event;
};


#endif
