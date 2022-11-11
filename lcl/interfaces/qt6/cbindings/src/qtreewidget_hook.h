//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QTREEWIDGET_HOOK_H
#define QTREEWIDGET_HOOK_H

#include <qtreewidget.h>

#include "qobject_hook.h"

class QTreeWidgetItem_hook : public QObject_hook {
  Q_OBJECT
  public:
    QTreeWidgetItem_hook(QObject *handle) : QObject_hook(handle) {
    }
};


#include "qtreeview_hook.h"

class QTreeWidget_hook : public QTreeView_hook {
  Q_OBJECT
  public:
    QTreeWidget_hook(QObject *handle) : QTreeView_hook(handle) {
      itemPressed_event.func = NULL;
      itemClicked_event.func = NULL;
      itemDoubleClicked_event.func = NULL;
      itemActivated_event.func = NULL;
      itemEntered_event.func = NULL;
      itemChanged_event.func = NULL;
      itemExpanded_event.func = NULL;
      itemCollapsed_event.func = NULL;
      currentItemChanged_event.func = NULL;
      itemSelectionChanged_event.func = NULL;
    }
    void hook_itemPressed(QHook &hook) { 
      if ( !itemPressed_event.func )
        connect(handle, SIGNAL(itemPressed(QTreeWidgetItem*, int)), this, SLOT(itemPressed_hook(QTreeWidgetItem*, int)));
      itemPressed_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(itemPressed(QTreeWidgetItem*, int)), this, SLOT(itemPressed_hook(QTreeWidgetItem*, int)));
    }
    void hook_itemClicked(QHook &hook) { 
      if ( !itemClicked_event.func )
        connect(handle, SIGNAL(itemClicked(QTreeWidgetItem*, int)), this, SLOT(itemClicked_hook(QTreeWidgetItem*, int)));
      itemClicked_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(itemClicked(QTreeWidgetItem*, int)), this, SLOT(itemClicked_hook(QTreeWidgetItem*, int)));
    }
    void hook_itemDoubleClicked(QHook &hook) { 
      if ( !itemDoubleClicked_event.func )
        connect(handle, SIGNAL(itemDoubleClicked(QTreeWidgetItem*, int)), this, SLOT(itemDoubleClicked_hook(QTreeWidgetItem*, int)));
      itemDoubleClicked_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(itemDoubleClicked(QTreeWidgetItem*, int)), this, SLOT(itemDoubleClicked_hook(QTreeWidgetItem*, int)));
    }
    void hook_itemActivated(QHook &hook) { 
      if ( !itemActivated_event.func )
        connect(handle, SIGNAL(itemActivated(QTreeWidgetItem*, int)), this, SLOT(itemActivated_hook(QTreeWidgetItem*, int)));
      itemActivated_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(itemActivated(QTreeWidgetItem*, int)), this, SLOT(itemActivated_hook(QTreeWidgetItem*, int)));
    }
    void hook_itemEntered(QHook &hook) { 
      if ( !itemEntered_event.func )
        connect(handle, SIGNAL(itemEntered(QTreeWidgetItem*, int)), this, SLOT(itemEntered_hook(QTreeWidgetItem*, int)));
      itemEntered_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(itemEntered(QTreeWidgetItem*, int)), this, SLOT(itemEntered_hook(QTreeWidgetItem*, int)));
    }
    void hook_itemChanged(QHook &hook) { 
      if ( !itemChanged_event.func )
        connect(handle, SIGNAL(itemChanged(QTreeWidgetItem*, int)), this, SLOT(itemChanged_hook(QTreeWidgetItem*, int)));
      itemChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(itemChanged(QTreeWidgetItem*, int)), this, SLOT(itemChanged_hook(QTreeWidgetItem*, int)));
    }
    void hook_itemExpanded(QHook &hook) { 
      if ( !itemExpanded_event.func )
        connect(handle, SIGNAL(itemExpanded(QTreeWidgetItem*)), this, SLOT(itemExpanded_hook(QTreeWidgetItem*)));
      itemExpanded_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(itemExpanded(QTreeWidgetItem*)), this, SLOT(itemExpanded_hook(QTreeWidgetItem*)));
    }
    void hook_itemCollapsed(QHook &hook) { 
      if ( !itemCollapsed_event.func )
        connect(handle, SIGNAL(itemCollapsed(QTreeWidgetItem*)), this, SLOT(itemCollapsed_hook(QTreeWidgetItem*)));
      itemCollapsed_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(itemCollapsed(QTreeWidgetItem*)), this, SLOT(itemCollapsed_hook(QTreeWidgetItem*)));
    }
    void hook_currentItemChanged(QHook &hook) { 
      if ( !currentItemChanged_event.func )
        connect(handle, SIGNAL(currentItemChanged(QTreeWidgetItem*, QTreeWidgetItem*)), this, SLOT(currentItemChanged_hook(QTreeWidgetItem*, QTreeWidgetItem*)));
      currentItemChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(currentItemChanged(QTreeWidgetItem*, QTreeWidgetItem*)), this, SLOT(currentItemChanged_hook(QTreeWidgetItem*, QTreeWidgetItem*)));
    }
    void hook_itemSelectionChanged(QHook &hook) { 
      if ( !itemSelectionChanged_event.func )
        connect(handle, SIGNAL(itemSelectionChanged()), this, SLOT(itemSelectionChanged_hook()));
      itemSelectionChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(itemSelectionChanged()), this, SLOT(itemSelectionChanged_hook()));
    }

  private slots:
    void itemPressed_hook(QTreeWidgetItem* item, int column) {
      if ( itemPressed_event.func ) {
        typedef void (*func_type)(void *data, QTreeWidgetItemH item, int column);
	(*(func_type)itemPressed_event.func)(itemPressed_event.data, (QTreeWidgetItemH)item, column);
      }
    }
    void itemClicked_hook(QTreeWidgetItem* item, int column) {
      if ( itemClicked_event.func ) {
        typedef void (*func_type)(void *data, QTreeWidgetItemH item, int column);
	(*(func_type)itemClicked_event.func)(itemClicked_event.data, (QTreeWidgetItemH)item, column);
      }
    }
    void itemDoubleClicked_hook(QTreeWidgetItem* item, int column) {
      if ( itemDoubleClicked_event.func ) {
        typedef void (*func_type)(void *data, QTreeWidgetItemH item, int column);
	(*(func_type)itemDoubleClicked_event.func)(itemDoubleClicked_event.data, (QTreeWidgetItemH)item, column);
      }
    }
    void itemActivated_hook(QTreeWidgetItem* item, int column) {
      if ( itemActivated_event.func ) {
        typedef void (*func_type)(void *data, QTreeWidgetItemH item, int column);
	(*(func_type)itemActivated_event.func)(itemActivated_event.data, (QTreeWidgetItemH)item, column);
      }
    }
    void itemEntered_hook(QTreeWidgetItem* item, int column) {
      if ( itemEntered_event.func ) {
        typedef void (*func_type)(void *data, QTreeWidgetItemH item, int column);
	(*(func_type)itemEntered_event.func)(itemEntered_event.data, (QTreeWidgetItemH)item, column);
      }
    }
    void itemChanged_hook(QTreeWidgetItem* item, int column) {
      if ( itemChanged_event.func ) {
        typedef void (*func_type)(void *data, QTreeWidgetItemH item, int column);
	(*(func_type)itemChanged_event.func)(itemChanged_event.data, (QTreeWidgetItemH)item, column);
      }
    }
    void itemExpanded_hook(QTreeWidgetItem* item) {
      if ( itemExpanded_event.func ) {
        typedef void (*func_type)(void *data, QTreeWidgetItemH item);
	(*(func_type)itemExpanded_event.func)(itemExpanded_event.data, (QTreeWidgetItemH)item);
      }
    }
    void itemCollapsed_hook(QTreeWidgetItem* item) {
      if ( itemCollapsed_event.func ) {
        typedef void (*func_type)(void *data, QTreeWidgetItemH item);
	(*(func_type)itemCollapsed_event.func)(itemCollapsed_event.data, (QTreeWidgetItemH)item);
      }
    }
    void currentItemChanged_hook(QTreeWidgetItem* current, QTreeWidgetItem* previous) {
      if ( currentItemChanged_event.func ) {
        typedef void (*func_type)(void *data, QTreeWidgetItemH current, QTreeWidgetItemH previous);
	(*(func_type)currentItemChanged_event.func)(currentItemChanged_event.data, (QTreeWidgetItemH)current, (QTreeWidgetItemH)previous);
      }
    }
    void itemSelectionChanged_hook() {
      if ( itemSelectionChanged_event.func ) {
        typedef void (*func_type)(void *data);
	(*(func_type)itemSelectionChanged_event.func)(itemSelectionChanged_event.data);
      }
    }
  private:
    QHook itemPressed_event;
    QHook itemClicked_event;
    QHook itemDoubleClicked_event;
    QHook itemActivated_event;
    QHook itemEntered_event;
    QHook itemChanged_event;
    QHook itemExpanded_event;
    QHook itemCollapsed_event;
    QHook currentItemChanged_event;
    QHook itemSelectionChanged_event;
};


#endif
