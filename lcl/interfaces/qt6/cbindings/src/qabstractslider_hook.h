//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QABSTRACTSLIDER_HOOK_H
#define QABSTRACTSLIDER_HOOK_H

#include <qabstractslider.h>

#include "qwidget_hook.h"

class QAbstractSlider_hook : public QWidget_hook {
  Q_OBJECT
  public:
    QAbstractSlider_hook(QObject *handle) : QWidget_hook(handle) {
      valueChanged_event.func = NULL;
      sliderPressed_event.func = NULL;
      sliderMoved_event.func = NULL;
      sliderReleased_event.func = NULL;
      rangeChanged_event.func = NULL;
      actionTriggered_event.func = NULL;
    }
    void hook_valueChanged(QHook &hook) { 
      if ( !valueChanged_event.func )
        connect(handle, SIGNAL(valueChanged(int)), this, SLOT(valueChanged_hook(int)));
      valueChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(valueChanged(int)), this, SLOT(valueChanged_hook(int)));
    }
    void hook_sliderPressed(QHook &hook) { 
      if ( !sliderPressed_event.func )
        connect(handle, SIGNAL(sliderPressed()), this, SLOT(sliderPressed_hook()));
      sliderPressed_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(sliderPressed()), this, SLOT(sliderPressed_hook()));
    }
    void hook_sliderMoved(QHook &hook) { 
      if ( !sliderMoved_event.func )
        connect(handle, SIGNAL(sliderMoved(int)), this, SLOT(sliderMoved_hook(int)));
      sliderMoved_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(sliderMoved(int)), this, SLOT(sliderMoved_hook(int)));
    }
    void hook_sliderReleased(QHook &hook) { 
      if ( !sliderReleased_event.func )
        connect(handle, SIGNAL(sliderReleased()), this, SLOT(sliderReleased_hook()));
      sliderReleased_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(sliderReleased()), this, SLOT(sliderReleased_hook()));
    }
    void hook_rangeChanged(QHook &hook) { 
      if ( !rangeChanged_event.func )
        connect(handle, SIGNAL(rangeChanged(int, int)), this, SLOT(rangeChanged_hook(int, int)));
      rangeChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(rangeChanged(int, int)), this, SLOT(rangeChanged_hook(int, int)));
    }
    void hook_actionTriggered(QHook &hook) { 
      if ( !actionTriggered_event.func )
        connect(handle, SIGNAL(actionTriggered(int)), this, SLOT(actionTriggered_hook(int)));
      actionTriggered_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(actionTriggered(int)), this, SLOT(actionTriggered_hook(int)));
    }

  private slots:
    void valueChanged_hook(int value) {
      if ( valueChanged_event.func ) {
        typedef void (*func_type)(void *data, int value);
	(*(func_type)valueChanged_event.func)(valueChanged_event.data, value);
      }
    }
    void sliderPressed_hook() {
      if ( sliderPressed_event.func ) {
        typedef void (*func_type)(void *data);
	(*(func_type)sliderPressed_event.func)(sliderPressed_event.data);
      }
    }
    void sliderMoved_hook(int position) {
      if ( sliderMoved_event.func ) {
        typedef void (*func_type)(void *data, int position);
	(*(func_type)sliderMoved_event.func)(sliderMoved_event.data, position);
      }
    }
    void sliderReleased_hook() {
      if ( sliderReleased_event.func ) {
        typedef void (*func_type)(void *data);
	(*(func_type)sliderReleased_event.func)(sliderReleased_event.data);
      }
    }
    void rangeChanged_hook(int min, int max) {
      if ( rangeChanged_event.func ) {
        typedef void (*func_type)(void *data, int min, int max);
	(*(func_type)rangeChanged_event.func)(rangeChanged_event.data, min, max);
      }
    }
    void actionTriggered_hook(int action) {
      if ( actionTriggered_event.func ) {
        typedef void (*func_type)(void *data, int action);
	(*(func_type)actionTriggered_event.func)(actionTriggered_event.data, action);
      }
    }
  private:
    QHook valueChanged_event;
    QHook sliderPressed_event;
    QHook sliderMoved_event;
    QHook sliderReleased_event;
    QHook rangeChanged_event;
    QHook actionTriggered_event;
};


#endif
