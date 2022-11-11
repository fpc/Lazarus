//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QITEMSELECTIONMODEL_HOOK_H
#define QITEMSELECTIONMODEL_HOOK_H

#include <qitemselectionmodel.h>

#include "qobject_hook.h"

class QItemSelectionRange_hook : public QObject_hook {
  Q_OBJECT
  public:
    QItemSelectionRange_hook(QObject *handle) : QObject_hook(handle) {
    }
};


#include "qobject_hook.h"

class QItemSelectionModel_hook : public QObject_hook {
  Q_OBJECT
  public:
    QItemSelectionModel_hook(QObject *handle) : QObject_hook(handle) {
      currentChanged_event.func = NULL;
      currentRowChanged_event.func = NULL;
      currentColumnChanged_event.func = NULL;
    }
    void hook_currentChanged(QHook &hook) { 
      if ( !currentChanged_event.func )
        connect(handle, SIGNAL(currentChanged(const QModelIndex&, const QModelIndex&)), this, SLOT(currentChanged_hook(const QModelIndex&, const QModelIndex&)));
      currentChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(currentChanged(const QModelIndex&, const QModelIndex&)), this, SLOT(currentChanged_hook(const QModelIndex&, const QModelIndex&)));
    }
    void hook_currentRowChanged(QHook &hook) { 
      if ( !currentRowChanged_event.func )
        connect(handle, SIGNAL(currentRowChanged(const QModelIndex&, const QModelIndex&)), this, SLOT(currentRowChanged_hook(const QModelIndex&, const QModelIndex&)));
      currentRowChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(currentRowChanged(const QModelIndex&, const QModelIndex&)), this, SLOT(currentRowChanged_hook(const QModelIndex&, const QModelIndex&)));
    }
    void hook_currentColumnChanged(QHook &hook) { 
      if ( !currentColumnChanged_event.func )
        connect(handle, SIGNAL(currentColumnChanged(const QModelIndex&, const QModelIndex&)), this, SLOT(currentColumnChanged_hook(const QModelIndex&, const QModelIndex&)));
      currentColumnChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(currentColumnChanged(const QModelIndex&, const QModelIndex&)), this, SLOT(currentColumnChanged_hook(const QModelIndex&, const QModelIndex&)));
    }

  private slots:
    void currentChanged_hook(const QModelIndex& current, const QModelIndex& previous) {
      if ( currentChanged_event.func ) {
        typedef void (*func_type)(void *data, const QModelIndexH current, const QModelIndexH previous);
	(*(func_type)currentChanged_event.func)(currentChanged_event.data, (const QModelIndexH)&current, (const QModelIndexH)&previous);
      }
    }
    void currentRowChanged_hook(const QModelIndex& current, const QModelIndex& previous) {
      if ( currentRowChanged_event.func ) {
        typedef void (*func_type)(void *data, const QModelIndexH current, const QModelIndexH previous);
	(*(func_type)currentRowChanged_event.func)(currentRowChanged_event.data, (const QModelIndexH)&current, (const QModelIndexH)&previous);
      }
    }
    void currentColumnChanged_hook(const QModelIndex& current, const QModelIndex& previous) {
      if ( currentColumnChanged_event.func ) {
        typedef void (*func_type)(void *data, const QModelIndexH current, const QModelIndexH previous);
	(*(func_type)currentColumnChanged_event.func)(currentColumnChanged_event.data, (const QModelIndexH)&current, (const QModelIndexH)&previous);
      }
    }
  private:
    QHook currentChanged_event;
    QHook currentRowChanged_event;
    QHook currentColumnChanged_event;
};


#endif
