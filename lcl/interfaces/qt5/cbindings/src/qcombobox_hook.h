//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QCOMBOBOX_HOOK_H
#define QCOMBOBOX_HOOK_H

#include <qcombobox.h>

#include "qwidget_hook.h"

class QComboBox_hook : public QWidget_hook {
  Q_OBJECT
  public:
    QComboBox_hook(QObject *handle) : QWidget_hook(handle) {
      editTextChanged_event.func = NULL;
      activated_event.func = NULL;
      activated2_event.func = NULL;
      highlighted_event.func = NULL;
      highlighted2_event.func = NULL;
      currentIndexChanged_event.func = NULL;
      currentIndexChanged2_event.func = NULL;
      currentTextChanged_event.func = NULL;
    }
    void hook_editTextChanged(QHook &hook) { 
      if ( !editTextChanged_event.func )
        connect(handle, SIGNAL(editTextChanged(const QString&)), this, SLOT(editTextChanged_hook(const QString&)));
      editTextChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(editTextChanged(const QString&)), this, SLOT(editTextChanged_hook(const QString&)));
    }
    void hook_activated(QHook &hook) { 
      if ( !activated_event.func )
        connect(handle, SIGNAL(activated(int)), this, SLOT(activated_hook(int)));
      activated_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(activated(int)), this, SLOT(activated_hook(int)));
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
        connect(handle, SIGNAL(highlighted(int)), this, SLOT(highlighted_hook(int)));
      highlighted_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(highlighted(int)), this, SLOT(highlighted_hook(int)));
    }
    void hook_highlighted2(QHook &hook) { 
      if ( !highlighted2_event.func )
        connect(handle, SIGNAL(highlighted(const QString&)), this, SLOT(highlighted2_hook(const QString&)));
      highlighted2_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(highlighted(const QString&)), this, SLOT(highlighted2_hook(const QString&)));
    }
    void hook_currentIndexChanged(QHook &hook) { 
      if ( !currentIndexChanged_event.func )
        connect(handle, SIGNAL(currentIndexChanged(int)), this, SLOT(currentIndexChanged_hook(int)));
      currentIndexChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(currentIndexChanged(int)), this, SLOT(currentIndexChanged_hook(int)));
    }
    void hook_currentIndexChanged2(QHook &hook) { 
      if ( !currentIndexChanged2_event.func )
        connect(handle, SIGNAL(currentIndexChanged(const QString&)), this, SLOT(currentIndexChanged2_hook(const QString&)));
      currentIndexChanged2_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(currentIndexChanged(const QString&)), this, SLOT(currentIndexChanged2_hook(const QString&)));
    }
    void hook_currentTextChanged(QHook &hook) { 
      if ( !currentTextChanged_event.func )
        connect(handle, SIGNAL(currentTextChanged(const QString&)), this, SLOT(currentTextChanged_hook(const QString&)));
      currentTextChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(currentTextChanged(const QString&)), this, SLOT(currentTextChanged_hook(const QString&)));
    }

  private slots:
    void editTextChanged_hook(const QString& AnonParam1) {
      if ( editTextChanged_event.func ) {
        typedef void (*func_type)(void *data, PWideString AnonParam1);
	PWideString t_AnonParam1;
	initializePWideString(t_AnonParam1);
	copyQStringToPWideString(AnonParam1, t_AnonParam1);
	(*(func_type)editTextChanged_event.func)(editTextChanged_event.data, t_AnonParam1);
	finalizePWideString(t_AnonParam1);
      }
    }
    void activated_hook(int index) {
      if ( activated_event.func ) {
        typedef void (*func_type)(void *data, int index);
	(*(func_type)activated_event.func)(activated_event.data, index);
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
    void highlighted_hook(int index) {
      if ( highlighted_event.func ) {
        typedef void (*func_type)(void *data, int index);
	(*(func_type)highlighted_event.func)(highlighted_event.data, index);
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
    void currentIndexChanged_hook(int index) {
      if ( currentIndexChanged_event.func ) {
        typedef void (*func_type)(void *data, int index);
	(*(func_type)currentIndexChanged_event.func)(currentIndexChanged_event.data, index);
      }
    }
    void currentIndexChanged2_hook(const QString& AnonParam1) {
      if ( currentIndexChanged2_event.func ) {
        typedef void (*func_type)(void *data, PWideString AnonParam1);
	PWideString t_AnonParam1;
	initializePWideString(t_AnonParam1);
	copyQStringToPWideString(AnonParam1, t_AnonParam1);
	(*(func_type)currentIndexChanged2_event.func)(currentIndexChanged2_event.data, t_AnonParam1);
	finalizePWideString(t_AnonParam1);
      }
    }
    void currentTextChanged_hook(const QString& AnonParam1) {
      if ( currentTextChanged_event.func ) {
        typedef void (*func_type)(void *data, PWideString AnonParam1);
	PWideString t_AnonParam1;
	initializePWideString(t_AnonParam1);
	copyQStringToPWideString(AnonParam1, t_AnonParam1);
	(*(func_type)currentTextChanged_event.func)(currentTextChanged_event.data, t_AnonParam1);
	finalizePWideString(t_AnonParam1);
      }
    }
  private:
    QHook editTextChanged_event;
    QHook activated_event;
    QHook activated2_event;
    QHook highlighted_event;
    QHook highlighted2_event;
    QHook currentIndexChanged_event;
    QHook currentIndexChanged2_event;
    QHook currentTextChanged_event;
};


#endif
