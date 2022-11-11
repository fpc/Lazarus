//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QLINEEDIT_HOOK_H
#define QLINEEDIT_HOOK_H

#include <qlineedit.h>

#include "qwidget_hook.h"

class QLineEdit_hook : public QWidget_hook {
  Q_OBJECT
  public:
    QLineEdit_hook(QObject *handle) : QWidget_hook(handle) {
      textChanged_event.func = NULL;
      textEdited_event.func = NULL;
      cursorPositionChanged_event.func = NULL;
      inputRejected_event.func = NULL;
      returnPressed_event.func = NULL;
      editingFinished_event.func = NULL;
      selectionChanged_event.func = NULL;
    }
    void hook_textChanged(QHook &hook) { 
      if ( !textChanged_event.func )
        connect(handle, SIGNAL(textChanged(const QString&)), this, SLOT(textChanged_hook(const QString&)));
      textChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(textChanged(const QString&)), this, SLOT(textChanged_hook(const QString&)));
    }
    void hook_textEdited(QHook &hook) { 
      if ( !textEdited_event.func )
        connect(handle, SIGNAL(textEdited(const QString&)), this, SLOT(textEdited_hook(const QString&)));
      textEdited_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(textEdited(const QString&)), this, SLOT(textEdited_hook(const QString&)));
    }
    void hook_cursorPositionChanged(QHook &hook) { 
      if ( !cursorPositionChanged_event.func )
        connect(handle, SIGNAL(cursorPositionChanged(int, int)), this, SLOT(cursorPositionChanged_hook(int, int)));
      cursorPositionChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(cursorPositionChanged(int, int)), this, SLOT(cursorPositionChanged_hook(int, int)));
    }
    void hook_returnPressed(QHook &hook) { 
      if ( !returnPressed_event.func )
        connect(handle, SIGNAL(returnPressed()), this, SLOT(returnPressed_hook()));
      returnPressed_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(returnPressed()), this, SLOT(returnPressed_hook()));
    }
    void hook_inputRejected(QHook &hook) {
      if ( !inputRejected_event.func )
        connect(handle, SIGNAL(inputRejected()), this, SLOT(inputRejected_hook()));
      inputRejected_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(inputRejected()), this, SLOT(inputRejected_hook()));
    }
    void hook_editingFinished(QHook &hook) { 
      if ( !editingFinished_event.func )
        connect(handle, SIGNAL(editingFinished()), this, SLOT(editingFinished_hook()));
      editingFinished_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(editingFinished()), this, SLOT(editingFinished_hook()));
    }
    void hook_selectionChanged(QHook &hook) { 
      if ( !selectionChanged_event.func )
        connect(handle, SIGNAL(selectionChanged()), this, SLOT(selectionChanged_hook()));
      selectionChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(selectionChanged()), this, SLOT(selectionChanged_hook()));
    }

  private slots:
    void textChanged_hook(const QString& AnonParam1) {
      if ( textChanged_event.func ) {
        typedef void (*func_type)(void *data, PWideString AnonParam1);
	PWideString t_AnonParam1;
	initializePWideString(t_AnonParam1);
	copyQStringToPWideString(AnonParam1, t_AnonParam1);
	(*(func_type)textChanged_event.func)(textChanged_event.data, t_AnonParam1);
	finalizePWideString(t_AnonParam1);
      }
    }
    void textEdited_hook(const QString& AnonParam1) {
      if ( textEdited_event.func ) {
        typedef void (*func_type)(void *data, PWideString AnonParam1);
	PWideString t_AnonParam1;
	initializePWideString(t_AnonParam1);
	copyQStringToPWideString(AnonParam1, t_AnonParam1);
	(*(func_type)textEdited_event.func)(textEdited_event.data, t_AnonParam1);
	finalizePWideString(t_AnonParam1);
      }
    }
    void cursorPositionChanged_hook(int AnonParam1, int AnonParam2) {
      if ( cursorPositionChanged_event.func ) {
        typedef void (*func_type)(void *data, int AnonParam1, int AnonParam2);
	(*(func_type)cursorPositionChanged_event.func)(cursorPositionChanged_event.data, AnonParam1, AnonParam2);
      }
    }
    void returnPressed_hook() {
      if ( returnPressed_event.func ) {
        typedef void (*func_type)(void *data);
	(*(func_type)returnPressed_event.func)(returnPressed_event.data);
      }
    }
    void inputRejected_hook() {
      if ( inputRejected_event.func ) {
        typedef void (*func_type)(void *data);
	(*(func_type)inputRejected_event.func)(inputRejected_event.data);
      }
    }
    void editingFinished_hook() {
      if ( editingFinished_event.func ) {
        typedef void (*func_type)(void *data);
	(*(func_type)editingFinished_event.func)(editingFinished_event.data);
      }
    }
    void selectionChanged_hook() {
      if ( selectionChanged_event.func ) {
        typedef void (*func_type)(void *data);
	(*(func_type)selectionChanged_event.func)(selectionChanged_event.data);
      }
    }
  private:
    QHook textChanged_event;
    QHook textEdited_event;
    QHook cursorPositionChanged_event;
    QHook inputRejected_event;
    QHook returnPressed_event;
    QHook editingFinished_event;
    QHook selectionChanged_event;
};


#endif
