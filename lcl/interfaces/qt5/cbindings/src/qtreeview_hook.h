//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QTREEVIEW_HOOK_H
#define QTREEVIEW_HOOK_H

#include <qtreeview.h>

#include "qabstractitemview_hook.h"

class QTreeView_hook : public QAbstractItemView_hook {
  Q_OBJECT
  public:
    QTreeView_hook(QObject *handle) : QAbstractItemView_hook(handle) {
      expanded_event.func = NULL;
      collapsed_event.func = NULL;
    }
    void hook_expanded(QHook &hook) { 
      if ( !expanded_event.func )
        connect(handle, SIGNAL(expanded(const QModelIndex&)), this, SLOT(expanded_hook(const QModelIndex&)));
      expanded_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(expanded(const QModelIndex&)), this, SLOT(expanded_hook(const QModelIndex&)));
    }
    void hook_collapsed(QHook &hook) { 
      if ( !collapsed_event.func )
        connect(handle, SIGNAL(collapsed(const QModelIndex&)), this, SLOT(collapsed_hook(const QModelIndex&)));
      collapsed_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(collapsed(const QModelIndex&)), this, SLOT(collapsed_hook(const QModelIndex&)));
    }

  private slots:
    void expanded_hook(const QModelIndex& index) {
      if ( expanded_event.func ) {
        typedef void (*func_type)(void *data, const QModelIndexH index);
	(*(func_type)expanded_event.func)(expanded_event.data, (const QModelIndexH)&index);
      }
    }
    void collapsed_hook(const QModelIndex& index) {
      if ( collapsed_event.func ) {
        typedef void (*func_type)(void *data, const QModelIndexH index);
	(*(func_type)collapsed_event.func)(collapsed_event.data, (const QModelIndexH)&index);
      }
    }
  private:
    QHook expanded_event;
    QHook collapsed_event;
};


#endif
