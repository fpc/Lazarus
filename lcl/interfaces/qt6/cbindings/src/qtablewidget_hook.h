//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QTABLEWIDGET_HOOK_H
#define QTABLEWIDGET_HOOK_H

#include <qtablewidget.h>

#include "qobject_hook.h"

class QTableWidgetSelectionRange_hook : public QObject_hook {
  Q_OBJECT
  public:
    QTableWidgetSelectionRange_hook(QObject *handle) : QObject_hook(handle) {
    }
};


#include "qobject_hook.h"

class QTableWidgetItem_hook : public QObject_hook {
  Q_OBJECT
  public:
    QTableWidgetItem_hook(QObject *handle) : QObject_hook(handle) {
    }
};


#include "qtableview_hook.h"

class QTableWidget_hook : public QTableView_hook {
  Q_OBJECT
  public:
    QTableWidget_hook(QObject *handle) : QTableView_hook(handle) {
      itemPressed_event.func = NULL;
      itemClicked_event.func = NULL;
      itemDoubleClicked_event.func = NULL;
      itemActivated_event.func = NULL;
      itemEntered_event.func = NULL;
      itemChanged_event.func = NULL;
      currentItemChanged_event.func = NULL;
      itemSelectionChanged_event.func = NULL;
      cellPressed_event.func = NULL;
      cellClicked_event.func = NULL;
      cellDoubleClicked_event.func = NULL;
      cellActivated_event.func = NULL;
      cellEntered_event.func = NULL;
      cellChanged_event.func = NULL;
      currentCellChanged_event.func = NULL;
    }
    void hook_itemPressed(QHook &hook) { 
      if ( !itemPressed_event.func )
        connect(handle, SIGNAL(itemPressed(QTableWidgetItem*)), this, SLOT(itemPressed_hook(QTableWidgetItem*)));
      itemPressed_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(itemPressed(QTableWidgetItem*)), this, SLOT(itemPressed_hook(QTableWidgetItem*)));
    }
    void hook_itemClicked(QHook &hook) { 
      if ( !itemClicked_event.func )
        connect(handle, SIGNAL(itemClicked(QTableWidgetItem*)), this, SLOT(itemClicked_hook(QTableWidgetItem*)));
      itemClicked_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(itemClicked(QTableWidgetItem*)), this, SLOT(itemClicked_hook(QTableWidgetItem*)));
    }
    void hook_itemDoubleClicked(QHook &hook) { 
      if ( !itemDoubleClicked_event.func )
        connect(handle, SIGNAL(itemDoubleClicked(QTableWidgetItem*)), this, SLOT(itemDoubleClicked_hook(QTableWidgetItem*)));
      itemDoubleClicked_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(itemDoubleClicked(QTableWidgetItem*)), this, SLOT(itemDoubleClicked_hook(QTableWidgetItem*)));
    }
    void hook_itemActivated(QHook &hook) { 
      if ( !itemActivated_event.func )
        connect(handle, SIGNAL(itemActivated(QTableWidgetItem*)), this, SLOT(itemActivated_hook(QTableWidgetItem*)));
      itemActivated_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(itemActivated(QTableWidgetItem*)), this, SLOT(itemActivated_hook(QTableWidgetItem*)));
    }
    void hook_itemEntered(QHook &hook) { 
      if ( !itemEntered_event.func )
        connect(handle, SIGNAL(itemEntered(QTableWidgetItem*)), this, SLOT(itemEntered_hook(QTableWidgetItem*)));
      itemEntered_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(itemEntered(QTableWidgetItem*)), this, SLOT(itemEntered_hook(QTableWidgetItem*)));
    }
    void hook_itemChanged(QHook &hook) { 
      if ( !itemChanged_event.func )
        connect(handle, SIGNAL(itemChanged(QTableWidgetItem*)), this, SLOT(itemChanged_hook(QTableWidgetItem*)));
      itemChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(itemChanged(QTableWidgetItem*)), this, SLOT(itemChanged_hook(QTableWidgetItem*)));
    }
    void hook_currentItemChanged(QHook &hook) { 
      if ( !currentItemChanged_event.func )
        connect(handle, SIGNAL(currentItemChanged(QTableWidgetItem*, QTableWidgetItem*)), this, SLOT(currentItemChanged_hook(QTableWidgetItem*, QTableWidgetItem*)));
      currentItemChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(currentItemChanged(QTableWidgetItem*, QTableWidgetItem*)), this, SLOT(currentItemChanged_hook(QTableWidgetItem*, QTableWidgetItem*)));
    }
    void hook_itemSelectionChanged(QHook &hook) { 
      if ( !itemSelectionChanged_event.func )
        connect(handle, SIGNAL(itemSelectionChanged()), this, SLOT(itemSelectionChanged_hook()));
      itemSelectionChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(itemSelectionChanged()), this, SLOT(itemSelectionChanged_hook()));
    }
    void hook_cellPressed(QHook &hook) { 
      if ( !cellPressed_event.func )
        connect(handle, SIGNAL(cellPressed(int, int)), this, SLOT(cellPressed_hook(int, int)));
      cellPressed_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(cellPressed(int, int)), this, SLOT(cellPressed_hook(int, int)));
    }
    void hook_cellClicked(QHook &hook) { 
      if ( !cellClicked_event.func )
        connect(handle, SIGNAL(cellClicked(int, int)), this, SLOT(cellClicked_hook(int, int)));
      cellClicked_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(cellClicked(int, int)), this, SLOT(cellClicked_hook(int, int)));
    }
    void hook_cellDoubleClicked(QHook &hook) { 
      if ( !cellDoubleClicked_event.func )
        connect(handle, SIGNAL(cellDoubleClicked(int, int)), this, SLOT(cellDoubleClicked_hook(int, int)));
      cellDoubleClicked_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(cellDoubleClicked(int, int)), this, SLOT(cellDoubleClicked_hook(int, int)));
    }
    void hook_cellActivated(QHook &hook) { 
      if ( !cellActivated_event.func )
        connect(handle, SIGNAL(cellActivated(int, int)), this, SLOT(cellActivated_hook(int, int)));
      cellActivated_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(cellActivated(int, int)), this, SLOT(cellActivated_hook(int, int)));
    }
    void hook_cellEntered(QHook &hook) { 
      if ( !cellEntered_event.func )
        connect(handle, SIGNAL(cellEntered(int, int)), this, SLOT(cellEntered_hook(int, int)));
      cellEntered_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(cellEntered(int, int)), this, SLOT(cellEntered_hook(int, int)));
    }
    void hook_cellChanged(QHook &hook) { 
      if ( !cellChanged_event.func )
        connect(handle, SIGNAL(cellChanged(int, int)), this, SLOT(cellChanged_hook(int, int)));
      cellChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(cellChanged(int, int)), this, SLOT(cellChanged_hook(int, int)));
    }
    void hook_currentCellChanged(QHook &hook) { 
      if ( !currentCellChanged_event.func )
        connect(handle, SIGNAL(currentCellChanged(int, int, int, int)), this, SLOT(currentCellChanged_hook(int, int, int, int)));
      currentCellChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(currentCellChanged(int, int, int, int)), this, SLOT(currentCellChanged_hook(int, int, int, int)));
    }

  private slots:
    void itemPressed_hook(QTableWidgetItem* item) {
      if ( itemPressed_event.func ) {
        typedef void (*func_type)(void *data, QTableWidgetItemH item);
	(*(func_type)itemPressed_event.func)(itemPressed_event.data, (QTableWidgetItemH)item);
      }
    }
    void itemClicked_hook(QTableWidgetItem* item) {
      if ( itemClicked_event.func ) {
        typedef void (*func_type)(void *data, QTableWidgetItemH item);
	(*(func_type)itemClicked_event.func)(itemClicked_event.data, (QTableWidgetItemH)item);
      }
    }
    void itemDoubleClicked_hook(QTableWidgetItem* item) {
      if ( itemDoubleClicked_event.func ) {
        typedef void (*func_type)(void *data, QTableWidgetItemH item);
	(*(func_type)itemDoubleClicked_event.func)(itemDoubleClicked_event.data, (QTableWidgetItemH)item);
      }
    }
    void itemActivated_hook(QTableWidgetItem* item) {
      if ( itemActivated_event.func ) {
        typedef void (*func_type)(void *data, QTableWidgetItemH item);
	(*(func_type)itemActivated_event.func)(itemActivated_event.data, (QTableWidgetItemH)item);
      }
    }
    void itemEntered_hook(QTableWidgetItem* item) {
      if ( itemEntered_event.func ) {
        typedef void (*func_type)(void *data, QTableWidgetItemH item);
	(*(func_type)itemEntered_event.func)(itemEntered_event.data, (QTableWidgetItemH)item);
      }
    }
    void itemChanged_hook(QTableWidgetItem* item) {
      if ( itemChanged_event.func ) {
        typedef void (*func_type)(void *data, QTableWidgetItemH item);
	(*(func_type)itemChanged_event.func)(itemChanged_event.data, (QTableWidgetItemH)item);
      }
    }
    void currentItemChanged_hook(QTableWidgetItem* current, QTableWidgetItem* previous) {
      if ( currentItemChanged_event.func ) {
        typedef void (*func_type)(void *data, QTableWidgetItemH current, QTableWidgetItemH previous);
	(*(func_type)currentItemChanged_event.func)(currentItemChanged_event.data, (QTableWidgetItemH)current, (QTableWidgetItemH)previous);
      }
    }
    void itemSelectionChanged_hook() {
      if ( itemSelectionChanged_event.func ) {
        typedef void (*func_type)(void *data);
	(*(func_type)itemSelectionChanged_event.func)(itemSelectionChanged_event.data);
      }
    }
    void cellPressed_hook(int row, int column) {
      if ( cellPressed_event.func ) {
        typedef void (*func_type)(void *data, int row, int column);
	(*(func_type)cellPressed_event.func)(cellPressed_event.data, row, column);
      }
    }
    void cellClicked_hook(int row, int column) {
      if ( cellClicked_event.func ) {
        typedef void (*func_type)(void *data, int row, int column);
	(*(func_type)cellClicked_event.func)(cellClicked_event.data, row, column);
      }
    }
    void cellDoubleClicked_hook(int row, int column) {
      if ( cellDoubleClicked_event.func ) {
        typedef void (*func_type)(void *data, int row, int column);
	(*(func_type)cellDoubleClicked_event.func)(cellDoubleClicked_event.data, row, column);
      }
    }
    void cellActivated_hook(int row, int column) {
      if ( cellActivated_event.func ) {
        typedef void (*func_type)(void *data, int row, int column);
	(*(func_type)cellActivated_event.func)(cellActivated_event.data, row, column);
      }
    }
    void cellEntered_hook(int row, int column) {
      if ( cellEntered_event.func ) {
        typedef void (*func_type)(void *data, int row, int column);
	(*(func_type)cellEntered_event.func)(cellEntered_event.data, row, column);
      }
    }
    void cellChanged_hook(int row, int column) {
      if ( cellChanged_event.func ) {
        typedef void (*func_type)(void *data, int row, int column);
	(*(func_type)cellChanged_event.func)(cellChanged_event.data, row, column);
      }
    }
    void currentCellChanged_hook(int currentRow, int currentColumn, int previousRow, int previousColumn) {
      if ( currentCellChanged_event.func ) {
        typedef void (*func_type)(void *data, int currentRow, int currentColumn, int previousRow, int previousColumn);
	(*(func_type)currentCellChanged_event.func)(currentCellChanged_event.data, currentRow, currentColumn, previousRow, previousColumn);
      }
    }
  private:
    QHook itemPressed_event;
    QHook itemClicked_event;
    QHook itemDoubleClicked_event;
    QHook itemActivated_event;
    QHook itemEntered_event;
    QHook itemChanged_event;
    QHook currentItemChanged_event;
    QHook itemSelectionChanged_event;
    QHook cellPressed_event;
    QHook cellClicked_event;
    QHook cellDoubleClicked_event;
    QHook cellActivated_event;
    QHook cellEntered_event;
    QHook cellChanged_event;
    QHook currentCellChanged_event;
};


#endif
