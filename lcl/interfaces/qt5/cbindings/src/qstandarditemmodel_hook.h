//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QSTANDARDITEMMODEL_HOOK_H
#define QSTANDARDITEMMODEL_HOOK_H

#include <qstandarditemmodel.h>

#include "qobject_hook.h"

class QStandardItem_hook : public QObject_hook {
  Q_OBJECT
  public:
    QStandardItem_hook(QObject *handle) : QObject_hook(handle) {
    }
};


#include "qabstractitemmodel_hook.h"

class QStandardItemModel_hook : public QAbstractItemModel_hook {
  Q_OBJECT
  public:
    QStandardItemModel_hook(QObject *handle) : QAbstractItemModel_hook(handle) {
      itemChanged_event.func = NULL;
    }
    void hook_itemChanged(QHook &hook) { 
      if ( !itemChanged_event.func )
        connect(handle, SIGNAL(itemChanged(QStandardItem*)), this, SLOT(itemChanged_hook(QStandardItem*)));
      itemChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(itemChanged(QStandardItem*)), this, SLOT(itemChanged_hook(QStandardItem*)));
    }

  private slots:
    void itemChanged_hook(QStandardItem* item) {
      if ( itemChanged_event.func ) {
        typedef void (*func_type)(void *data, QStandardItemH item);
	(*(func_type)itemChanged_event.func)(itemChanged_event.data, (QStandardItemH)item);
      }
    }
  private:
    QHook itemChanged_event;
};


#endif
