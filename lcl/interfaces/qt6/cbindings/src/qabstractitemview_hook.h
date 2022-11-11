//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QABSTRACTITEMVIEW_HOOK_H
#define QABSTRACTITEMVIEW_HOOK_H

#include <qabstractitemview.h>

#include "qabstractscrollarea_hook.h"

class QAbstractItemView_hook : public QAbstractScrollArea_hook {
  Q_OBJECT
  public:
    QAbstractItemView_hook(QObject *handle) : QAbstractScrollArea_hook(handle) {
      pressed_event.func = NULL;
      clicked_event.func = NULL;
      doubleClicked_event.func = NULL;
      activated_event.func = NULL;
      entered_event.func = NULL;
      viewportEntered_event.func = NULL;
    }
    void hook_pressed(QHook &hook) { 
      if ( !pressed_event.func )
        connect(handle, SIGNAL(pressed(const QModelIndex&)), this, SLOT(pressed_hook(const QModelIndex&)));
      pressed_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(pressed(const QModelIndex&)), this, SLOT(pressed_hook(const QModelIndex&)));
    }
    void hook_clicked(QHook &hook) { 
      if ( !clicked_event.func )
        connect(handle, SIGNAL(clicked(const QModelIndex&)), this, SLOT(clicked_hook(const QModelIndex&)));
      clicked_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(clicked(const QModelIndex&)), this, SLOT(clicked_hook(const QModelIndex&)));
    }
    void hook_doubleClicked(QHook &hook) { 
      if ( !doubleClicked_event.func )
        connect(handle, SIGNAL(doubleClicked(const QModelIndex&)), this, SLOT(doubleClicked_hook(const QModelIndex&)));
      doubleClicked_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(doubleClicked(const QModelIndex&)), this, SLOT(doubleClicked_hook(const QModelIndex&)));
    }
    void hook_activated(QHook &hook) { 
      if ( !activated_event.func )
        connect(handle, SIGNAL(activated(const QModelIndex&)), this, SLOT(activated_hook(const QModelIndex&)));
      activated_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(activated(const QModelIndex&)), this, SLOT(activated_hook(const QModelIndex&)));
    }
    void hook_entered(QHook &hook) { 
      if ( !entered_event.func )
        connect(handle, SIGNAL(entered(const QModelIndex&)), this, SLOT(entered_hook(const QModelIndex&)));
      entered_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(entered(const QModelIndex&)), this, SLOT(entered_hook(const QModelIndex&)));
    }
    void hook_viewportEntered(QHook &hook) { 
      if ( !viewportEntered_event.func )
        connect(handle, SIGNAL(viewportEntered()), this, SLOT(viewportEntered_hook()));
      viewportEntered_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(viewportEntered()), this, SLOT(viewportEntered_hook()));
    }

  private slots:
    void pressed_hook(const QModelIndex& index) {
      if ( pressed_event.func ) {
        typedef void (*func_type)(void *data, const QModelIndexH index);
	(*(func_type)pressed_event.func)(pressed_event.data, (const QModelIndexH)&index);
      }
    }
    void clicked_hook(const QModelIndex& index) {
      if ( clicked_event.func ) {
        typedef void (*func_type)(void *data, const QModelIndexH index);
	(*(func_type)clicked_event.func)(clicked_event.data, (const QModelIndexH)&index);
      }
    }
    void doubleClicked_hook(const QModelIndex& index) {
      if ( doubleClicked_event.func ) {
        typedef void (*func_type)(void *data, const QModelIndexH index);
	(*(func_type)doubleClicked_event.func)(doubleClicked_event.data, (const QModelIndexH)&index);
      }
    }
    void activated_hook(const QModelIndex& index) {
      if ( activated_event.func ) {
        typedef void (*func_type)(void *data, const QModelIndexH index);
	(*(func_type)activated_event.func)(activated_event.data, (const QModelIndexH)&index);
      }
    }
    void entered_hook(const QModelIndex& index) {
      if ( entered_event.func ) {
        typedef void (*func_type)(void *data, const QModelIndexH index);
	(*(func_type)entered_event.func)(entered_event.data, (const QModelIndexH)&index);
      }
    }
    void viewportEntered_hook() {
      if ( viewportEntered_event.func ) {
        typedef void (*func_type)(void *data);
	(*(func_type)viewportEntered_event.func)(viewportEntered_event.data);
      }
    }
  private:
    QHook pressed_event;
    QHook clicked_event;
    QHook doubleClicked_event;
    QHook activated_event;
    QHook entered_event;
    QHook viewportEntered_event;
};


#endif
