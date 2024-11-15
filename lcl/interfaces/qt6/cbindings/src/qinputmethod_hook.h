//******************************************************************************
//  Copyright (c) 2024 by Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QINPUTMETHOD_HOOK_H
#define QINPUTMETHOD_HOOK_H

#include <qinputmethod.h>

#include "qobject_hook.h"

class QInputMethod_hook : public QObject_hook {
  Q_OBJECT
  public:

    QInputMethod_hook(QObject *handle) : QObject_hook(handle) {
      actionRectangleChanged_event.func = NULL;
      animatingChanged_event.func = NULL;
      cursorRectangleChanged_event.func = NULL;
      inputDirectionChanged_event.func = NULL;
      inputItemClipRectangleChanged_event.func = NULL;
      keyboardRectangleChanged_event.func = NULL;
      localeChanged_event.func = NULL;
      visibleChanged_event.func = NULL;
    }

    void hook_actionRectangleChanged(QHook &hook) {
      if ( !actionRectangleChanged_event.func )
        connect(handle, SIGNAL(actionRectangleChanged()), this, SLOT(actionRectangleChanged_hook()));
      actionRectangleChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(actionRectangleChanged()), this, SLOT(actionRectangleChanged_hook()));
    }

    void hook_animatingChanged(QHook &hook) {
      if ( !animatingChanged_event.func )
        connect(handle, SIGNAL(animatingChanged()), this, SLOT(animatingChanged_hook()));
      animatingChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(animatingChanged()), this, SLOT(animatingChanged_hook()));
    }

    void hook_cursorRectangleChanged(QHook &hook) {
      if ( !cursorRectangleChanged_event.func )
        connect(handle, SIGNAL(cursorRectangleChanged()), this, SLOT(cursorRectangleChanged_hook()));
      cursorRectangleChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(cursorRectangleChanged()), this, SLOT(cursorRectangleChanged_hook()));
    }

    void hook_inputDirectionChanged(QHook &hook) {
      if ( !inputDirectionChanged_event.func )
        connect(handle, SIGNAL(inputDirectionChanged(Qt::LayoutDirection)), this, SLOT(inputDirectionChanged_hook(Qt::LayoutDirection)));
      inputDirectionChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(inputDirectionChanged(Qt::LayoutDirection)), this, SLOT(inputDirectionChanged_hook(Qt::LayoutDirection)));
    }

    void hook_inputItemClipRectangleChanged(QHook &hook) {
      if ( !inputItemClipRectangleChanged_event.func )
        connect(handle, SIGNAL(inputItemClipRectangleChanged()), this, SLOT(inputItemClipRectangleChanged_hook()));
      inputItemClipRectangleChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(inputItemClipRectangleChanged()), this, SLOT(inputItemClipRectangleChanged_hook()));
    }

    void hook_keyboardRectangleChanged(QHook &hook) {
      if ( !keyboardRectangleChanged_event.func )
        connect(handle, SIGNAL(keyboardRectangleChanged()), this, SLOT(keyboardRectangleChanged_hook()));
      keyboardRectangleChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(keyboardRectangleChanged()), this, SLOT(keyboardRectangleChanged_hook()));
    }

    void hook_localeChanged(QHook &hook) {
      if ( !localeChanged_event.func )
        connect(handle, SIGNAL(localeChanged()), this, SLOT(localeChanged_hook()));
      localeChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(localeChanged()), this, SLOT(localeChanged_hook()));
    }

    void hook_visibleChanged(QHook &hook) {
      if ( !visibleChanged_event.func )
        connect(handle, SIGNAL(visibleChanged()), this, SLOT(visibleChanged_hook()));
      visibleChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(visibleChanged()), this, SLOT(visibleChanged_hook()));
    }


  private slots:
    void actionRectangleChanged_hook() {
      if ( actionRectangleChanged_event.func ) {
        typedef void (*func_type)(void *data);
	(*(func_type)actionRectangleChanged_event.func)(actionRectangleChanged_event.data);
      }
    }

    void animatingChanged_hook() {
      if ( animatingChanged_event.func ) {
        typedef void (*func_type)(void *data);
	(*(func_type)animatingChanged_event.func)(animatingChanged_event.data);
      }
    }

    void cursorRectangleChanged_hook() {
      if ( cursorRectangleChanged_event.func ) {
        typedef void (*func_type)(void *data);
	(*(func_type)cursorRectangleChanged_event.func)(cursorRectangleChanged_event.data);
      }
    }

    void inputDirectionChanged_hook(Qt::LayoutDirection direction) {
      if ( inputDirectionChanged_event.func ) {
        typedef void (*func_type)(void *data, Qt::LayoutDirection direction);
	(*(func_type)inputDirectionChanged_event.func)(inputDirectionChanged_event.data, direction);
      }
    }

    void inputItemClipRectangleChanged_hook() {
      if ( inputItemClipRectangleChanged_event.func ) {
        typedef void (*func_type)(void *data);
	(*(func_type)inputItemClipRectangleChanged_event.func)(inputItemClipRectangleChanged_event.data);
      }
    }

    void keyboardRectangleChanged_hook() {
      if ( keyboardRectangleChanged_event.func ) {
        typedef void (*func_type)(void *data);
	(*(func_type)keyboardRectangleChanged_event.func)(keyboardRectangleChanged_event.data);
      }
    }

    void localeChanged_hook() {
      if ( localeChanged_event.func ) {
        typedef void (*func_type)(void *data);
	(*(func_type)localeChanged_event.func)(localeChanged_event.data);
      }
    }

    void visibleChanged_hook() {
      if ( visibleChanged_event.func ) {
        typedef void (*func_type)(void *data);
	(*(func_type)visibleChanged_event.func)(visibleChanged_event.data);
      }
    }

  private:
     QHook actionRectangleChanged_event;
     QHook animatingChanged_event;
     QHook cursorRectangleChanged_event;
     QHook inputDirectionChanged_event;
     QHook inputItemClipRectangleChanged_event;
     QHook keyboardRectangleChanged_event;
     QHook localeChanged_event;
     QHook visibleChanged_event;
};


#endif
