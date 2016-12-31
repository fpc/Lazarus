//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QSPINBOX_HOOK_H
#define QSPINBOX_HOOK_H

#include <qspinbox.h>

#include "qabstractspinbox_hook.h"

class QSpinBox_hook : public QAbstractSpinBox_hook {
  Q_OBJECT
  public:
    QSpinBox_hook(QObject *handle) : QAbstractSpinBox_hook(handle) {
      valueChanged_event.func = NULL;
      valueChanged2_event.func = NULL;
    }
    void hook_valueChanged(QHook &hook) { 
      if ( !valueChanged_event.func )
        connect(handle, SIGNAL(valueChanged(int)), this, SLOT(valueChanged_hook(int)));
      valueChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(valueChanged(int)), this, SLOT(valueChanged_hook(int)));
    }
    void hook_valueChanged2(QHook &hook) { 
      if ( !valueChanged2_event.func )
        connect(handle, SIGNAL(valueChanged(const QString&)), this, SLOT(valueChanged2_hook(const QString&)));
      valueChanged2_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(valueChanged(const QString&)), this, SLOT(valueChanged2_hook(const QString&)));
    }

  private slots:
    void valueChanged_hook(int AnonParam1) {
      if ( valueChanged_event.func ) {
        typedef void (*func_type)(void *data, int AnonParam1);
	(*(func_type)valueChanged_event.func)(valueChanged_event.data, AnonParam1);
      }
    }
    void valueChanged2_hook(const QString& AnonParam1) {
      if ( valueChanged2_event.func ) {
        typedef void (*func_type)(void *data, PWideString AnonParam1);
	PWideString t_AnonParam1;
	initializePWideString(t_AnonParam1);
	copyQStringToPWideString(AnonParam1, t_AnonParam1);
	(*(func_type)valueChanged2_event.func)(valueChanged2_event.data, t_AnonParam1);
	finalizePWideString(t_AnonParam1);
      }
    }
  private:
    QHook valueChanged_event;
    QHook valueChanged2_event;
};


#include "qabstractspinbox_hook.h"

class QDoubleSpinBox_hook : public QAbstractSpinBox_hook {
  Q_OBJECT
  public:
    QDoubleSpinBox_hook(QObject *handle) : QAbstractSpinBox_hook(handle) {
      valueChanged_event.func = NULL;
      valueChanged2_event.func = NULL;
    }
    void hook_valueChanged(QHook &hook) { 
      if ( !valueChanged_event.func )
        connect(handle, SIGNAL(valueChanged(double)), this, SLOT(valueChanged_hook(double)));
      valueChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(valueChanged(double)), this, SLOT(valueChanged_hook(double)));
    }
    void hook_valueChanged2(QHook &hook) { 
      if ( !valueChanged2_event.func )
        connect(handle, SIGNAL(valueChanged(const QString&)), this, SLOT(valueChanged2_hook(const QString&)));
      valueChanged2_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(valueChanged(const QString&)), this, SLOT(valueChanged2_hook(const QString&)));
    }

  private slots:
    void valueChanged_hook(double AnonParam1) {
      if ( valueChanged_event.func ) {
        typedef void (*func_type)(void *data, double AnonParam1);
	(*(func_type)valueChanged_event.func)(valueChanged_event.data, AnonParam1);
      }
    }
    void valueChanged2_hook(const QString& AnonParam1) {
      if ( valueChanged2_event.func ) {
        typedef void (*func_type)(void *data, PWideString AnonParam1);
	PWideString t_AnonParam1;
	initializePWideString(t_AnonParam1);
	copyQStringToPWideString(AnonParam1, t_AnonParam1);
	(*(func_type)valueChanged2_event.func)(valueChanged2_event.data, t_AnonParam1);
	finalizePWideString(t_AnonParam1);
      }
    }
  private:
    QHook valueChanged_event;
    QHook valueChanged2_event;
};


#endif
