//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QLISTWIDGET_HOOK_H
#define QLISTWIDGET_HOOK_H

#include <qlistwidget.h>

#include "qobject_hook.h"

class QListWidgetItem_hook : public QObject_hook {
  Q_OBJECT
  public:
    QListWidgetItem_hook(QObject *handle) : QObject_hook(handle) {
    }
};


#include "qlistview_hook.h"

class QListWidget_hook : public QListView_hook {
  Q_OBJECT
  public:
    QListWidget_hook(QObject *handle) : QListView_hook(handle) {
      itemPressed_event.func = NULL;
      itemClicked_event.func = NULL;
      itemDoubleClicked_event.func = NULL;
      itemActivated_event.func = NULL;
      itemEntered_event.func = NULL;
      itemChanged_event.func = NULL;
      currentItemChanged_event.func = NULL;
      currentTextChanged_event.func = NULL;
      currentRowChanged_event.func = NULL;
      itemSelectionChanged_event.func = NULL;
    }
    void hook_itemPressed(QHook &hook) { 
      if ( !itemPressed_event.func )
        connect(handle, SIGNAL(itemPressed(QListWidgetItem*)), this, SLOT(itemPressed_hook(QListWidgetItem*)));
      itemPressed_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(itemPressed(QListWidgetItem*)), this, SLOT(itemPressed_hook(QListWidgetItem*)));
    }
    void hook_itemClicked(QHook &hook) { 
      if ( !itemClicked_event.func )
        connect(handle, SIGNAL(itemClicked(QListWidgetItem*)), this, SLOT(itemClicked_hook(QListWidgetItem*)));
      itemClicked_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(itemClicked(QListWidgetItem*)), this, SLOT(itemClicked_hook(QListWidgetItem*)));
    }
    void hook_itemDoubleClicked(QHook &hook) { 
      if ( !itemDoubleClicked_event.func )
        connect(handle, SIGNAL(itemDoubleClicked(QListWidgetItem*)), this, SLOT(itemDoubleClicked_hook(QListWidgetItem*)));
      itemDoubleClicked_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(itemDoubleClicked(QListWidgetItem*)), this, SLOT(itemDoubleClicked_hook(QListWidgetItem*)));
    }
    void hook_itemActivated(QHook &hook) { 
      if ( !itemActivated_event.func )
        connect(handle, SIGNAL(itemActivated(QListWidgetItem*)), this, SLOT(itemActivated_hook(QListWidgetItem*)));
      itemActivated_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(itemActivated(QListWidgetItem*)), this, SLOT(itemActivated_hook(QListWidgetItem*)));
    }
    void hook_itemEntered(QHook &hook) { 
      if ( !itemEntered_event.func )
        connect(handle, SIGNAL(itemEntered(QListWidgetItem*)), this, SLOT(itemEntered_hook(QListWidgetItem*)));
      itemEntered_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(itemEntered(QListWidgetItem*)), this, SLOT(itemEntered_hook(QListWidgetItem*)));
    }
    void hook_itemChanged(QHook &hook) { 
      if ( !itemChanged_event.func )
        connect(handle, SIGNAL(itemChanged(QListWidgetItem*)), this, SLOT(itemChanged_hook(QListWidgetItem*)));
      itemChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(itemChanged(QListWidgetItem*)), this, SLOT(itemChanged_hook(QListWidgetItem*)));
    }
    void hook_currentItemChanged(QHook &hook) { 
      if ( !currentItemChanged_event.func )
        connect(handle, SIGNAL(currentItemChanged(QListWidgetItem*, QListWidgetItem*)), this, SLOT(currentItemChanged_hook(QListWidgetItem*, QListWidgetItem*)));
      currentItemChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(currentItemChanged(QListWidgetItem*, QListWidgetItem*)), this, SLOT(currentItemChanged_hook(QListWidgetItem*, QListWidgetItem*)));
    }
    void hook_currentTextChanged(QHook &hook) { 
      if ( !currentTextChanged_event.func )
        connect(handle, SIGNAL(currentTextChanged(const QString&)), this, SLOT(currentTextChanged_hook(const QString&)));
      currentTextChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(currentTextChanged(const QString&)), this, SLOT(currentTextChanged_hook(const QString&)));
    }
    void hook_currentRowChanged(QHook &hook) { 
      if ( !currentRowChanged_event.func )
        connect(handle, SIGNAL(currentRowChanged(int)), this, SLOT(currentRowChanged_hook(int)));
      currentRowChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(currentRowChanged(int)), this, SLOT(currentRowChanged_hook(int)));
    }
    void hook_itemSelectionChanged(QHook &hook) { 
      if ( !itemSelectionChanged_event.func )
        connect(handle, SIGNAL(itemSelectionChanged()), this, SLOT(itemSelectionChanged_hook()));
      itemSelectionChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(itemSelectionChanged()), this, SLOT(itemSelectionChanged_hook()));
    }

  private slots:
    void itemPressed_hook(QListWidgetItem* item) {
      if ( itemPressed_event.func ) {
        typedef void (*func_type)(void *data, QListWidgetItemH item);
	(*(func_type)itemPressed_event.func)(itemPressed_event.data, (QListWidgetItemH)item);
      }
    }
    void itemClicked_hook(QListWidgetItem* item) {
      if ( itemClicked_event.func ) {
        typedef void (*func_type)(void *data, QListWidgetItemH item);
	(*(func_type)itemClicked_event.func)(itemClicked_event.data, (QListWidgetItemH)item);
      }
    }
    void itemDoubleClicked_hook(QListWidgetItem* item) {
      if ( itemDoubleClicked_event.func ) {
        typedef void (*func_type)(void *data, QListWidgetItemH item);
	(*(func_type)itemDoubleClicked_event.func)(itemDoubleClicked_event.data, (QListWidgetItemH)item);
      }
    }
    void itemActivated_hook(QListWidgetItem* item) {
      if ( itemActivated_event.func ) {
        typedef void (*func_type)(void *data, QListWidgetItemH item);
	(*(func_type)itemActivated_event.func)(itemActivated_event.data, (QListWidgetItemH)item);
      }
    }
    void itemEntered_hook(QListWidgetItem* item) {
      if ( itemEntered_event.func ) {
        typedef void (*func_type)(void *data, QListWidgetItemH item);
	(*(func_type)itemEntered_event.func)(itemEntered_event.data, (QListWidgetItemH)item);
      }
    }
    void itemChanged_hook(QListWidgetItem* item) {
      if ( itemChanged_event.func ) {
        typedef void (*func_type)(void *data, QListWidgetItemH item);
	(*(func_type)itemChanged_event.func)(itemChanged_event.data, (QListWidgetItemH)item);
      }
    }
    void currentItemChanged_hook(QListWidgetItem* current, QListWidgetItem* previous) {
      if ( currentItemChanged_event.func ) {
        typedef void (*func_type)(void *data, QListWidgetItemH current, QListWidgetItemH previous);
	(*(func_type)currentItemChanged_event.func)(currentItemChanged_event.data, (QListWidgetItemH)current, (QListWidgetItemH)previous);
      }
    }
    void currentTextChanged_hook(const QString& currentText) {
      if ( currentTextChanged_event.func ) {
        typedef void (*func_type)(void *data, PWideString currentText);
	PWideString t_currentText;
	initializePWideString(t_currentText);
	copyQStringToPWideString(currentText, t_currentText);
	(*(func_type)currentTextChanged_event.func)(currentTextChanged_event.data, t_currentText);
	finalizePWideString(t_currentText);
      }
    }
    void currentRowChanged_hook(int currentRow) {
      if ( currentRowChanged_event.func ) {
        typedef void (*func_type)(void *data, int currentRow);
	(*(func_type)currentRowChanged_event.func)(currentRowChanged_event.data, currentRow);
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
    QHook currentItemChanged_event;
    QHook currentTextChanged_event;
    QHook currentRowChanged_event;
    QHook itemSelectionChanged_event;
};


#endif
