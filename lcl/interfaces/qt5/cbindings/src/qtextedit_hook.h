//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QTEXTEDIT_HOOK_H
#define QTEXTEDIT_HOOK_H

#include <qtextedit.h>

#include "qabstractscrollarea_hook.h"

class QTextEdit_hook : public QAbstractScrollArea_hook {
  Q_OBJECT
  public:
    QTextEdit_hook(QObject *handle) : QAbstractScrollArea_hook(handle) {
      textChanged_event.func = NULL;
      undoAvailable_event.func = NULL;
      redoAvailable_event.func = NULL;
      currentCharFormatChanged_event.func = NULL;
      copyAvailable_event.func = NULL;
      selectionChanged_event.func = NULL;
      cursorPositionChanged_event.func = NULL;
    }
    void hook_textChanged(QHook &hook) { 
      if ( !textChanged_event.func )
        connect(handle, SIGNAL(textChanged()), this, SLOT(textChanged_hook()));
      textChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(textChanged()), this, SLOT(textChanged_hook()));
    }
    void hook_undoAvailable(QHook &hook) { 
      if ( !undoAvailable_event.func )
        connect(handle, SIGNAL(undoAvailable(bool)), this, SLOT(undoAvailable_hook(bool)));
      undoAvailable_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(undoAvailable(bool)), this, SLOT(undoAvailable_hook(bool)));
    }
    void hook_redoAvailable(QHook &hook) { 
      if ( !redoAvailable_event.func )
        connect(handle, SIGNAL(redoAvailable(bool)), this, SLOT(redoAvailable_hook(bool)));
      redoAvailable_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(redoAvailable(bool)), this, SLOT(redoAvailable_hook(bool)));
    }
    void hook_currentCharFormatChanged(QHook &hook) { 
      if ( !currentCharFormatChanged_event.func )
        connect(handle, SIGNAL(currentCharFormatChanged(const QTextCharFormat&)), this, SLOT(currentCharFormatChanged_hook(const QTextCharFormat&)));
      currentCharFormatChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(currentCharFormatChanged(const QTextCharFormat&)), this, SLOT(currentCharFormatChanged_hook(const QTextCharFormat&)));
    }
    void hook_copyAvailable(QHook &hook) { 
      if ( !copyAvailable_event.func )
        connect(handle, SIGNAL(copyAvailable(bool)), this, SLOT(copyAvailable_hook(bool)));
      copyAvailable_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(copyAvailable(bool)), this, SLOT(copyAvailable_hook(bool)));
    }
    void hook_selectionChanged(QHook &hook) { 
      if ( !selectionChanged_event.func )
        connect(handle, SIGNAL(selectionChanged()), this, SLOT(selectionChanged_hook()));
      selectionChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(selectionChanged()), this, SLOT(selectionChanged_hook()));
    }
    void hook_cursorPositionChanged(QHook &hook) { 
      if ( !cursorPositionChanged_event.func )
        connect(handle, SIGNAL(cursorPositionChanged()), this, SLOT(cursorPositionChanged_hook()));
      cursorPositionChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(cursorPositionChanged()), this, SLOT(cursorPositionChanged_hook()));
    }

  private slots:
    void textChanged_hook() {
      if ( textChanged_event.func ) {
        typedef void (*func_type)(void *data);
	(*(func_type)textChanged_event.func)(textChanged_event.data);
      }
    }
    void undoAvailable_hook(bool b) {
      if ( undoAvailable_event.func ) {
        typedef void (*func_type)(void *data, bool b);
	(*(func_type)undoAvailable_event.func)(undoAvailable_event.data, b);
      }
    }
    void redoAvailable_hook(bool b) {
      if ( redoAvailable_event.func ) {
        typedef void (*func_type)(void *data, bool b);
	(*(func_type)redoAvailable_event.func)(redoAvailable_event.data, b);
      }
    }
    void currentCharFormatChanged_hook(const QTextCharFormat& format) {
      if ( currentCharFormatChanged_event.func ) {
        typedef void (*func_type)(void *data, const QTextCharFormatH format);
	(*(func_type)currentCharFormatChanged_event.func)(currentCharFormatChanged_event.data, (const QTextCharFormatH)&format);
      }
    }
    void copyAvailable_hook(bool b) {
      if ( copyAvailable_event.func ) {
        typedef void (*func_type)(void *data, bool b);
	(*(func_type)copyAvailable_event.func)(copyAvailable_event.data, b);
      }
    }
    void selectionChanged_hook() {
      if ( selectionChanged_event.func ) {
        typedef void (*func_type)(void *data);
	(*(func_type)selectionChanged_event.func)(selectionChanged_event.data);
      }
    }
    void cursorPositionChanged_hook() {
      if ( cursorPositionChanged_event.func ) {
        typedef void (*func_type)(void *data);
	(*(func_type)cursorPositionChanged_event.func)(cursorPositionChanged_event.data);
      }
    }
  private:
    QHook textChanged_event;
    QHook undoAvailable_event;
    QHook redoAvailable_event;
    QHook currentCharFormatChanged_event;
    QHook copyAvailable_event;
    QHook selectionChanged_event;
    QHook cursorPositionChanged_event;
};


#endif
