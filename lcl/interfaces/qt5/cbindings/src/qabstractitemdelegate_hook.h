//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QABSTRACTITEMDELEGATE_HOOK_H
#define QABSTRACTITEMDELEGATE_HOOK_H

#include <qabstractitemdelegate.h>

#include "qobject_hook.h"

class QAbstractItemDelegate_hook : public QObject_hook {
  Q_OBJECT
  public:
    QAbstractItemDelegate_hook(QObject *handle) : QObject_hook(handle) {
      commitData_event.func = NULL;
      closeEditor_event.func = NULL;
      closeEditor2_event.func = NULL;
      sizeHintChanged_event.func = NULL;
    }
    void hook_commitData(QHook &hook) { 
      if ( !commitData_event.func )
        connect(handle, SIGNAL(commitData(QWidget*)), this, SLOT(commitData_hook(QWidget*)));
      commitData_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(commitData(QWidget*)), this, SLOT(commitData_hook(QWidget*)));
    }
    void hook_closeEditor(QHook &hook) { 
      if ( !closeEditor_event.func )
        connect(handle, SIGNAL(closeEditor(QWidget*, QAbstractItemDelegate::EndEditHint)), this, SLOT(closeEditor_hook(QWidget*, QAbstractItemDelegate::EndEditHint)));
      closeEditor_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(closeEditor(QWidget*, QAbstractItemDelegate::EndEditHint)), this, SLOT(closeEditor_hook(QWidget*, QAbstractItemDelegate::EndEditHint)));
    }
    void hook_closeEditor2(QHook &hook) { 
      if ( !closeEditor2_event.func )
        connect(handle, SIGNAL(closeEditor(QWidget*)), this, SLOT(closeEditor2_hook(QWidget*)));
      closeEditor2_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(closeEditor(QWidget*)), this, SLOT(closeEditor2_hook(QWidget*)));
    }
    void hook_sizeHintChanged(QHook &hook) { 
      if ( !sizeHintChanged_event.func )
        connect(handle, SIGNAL(sizeHintChanged(const QModelIndex&)), this, SLOT(sizeHintChanged_hook(const QModelIndex&)));
      sizeHintChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(sizeHintChanged(const QModelIndex&)), this, SLOT(sizeHintChanged_hook(const QModelIndex&)));
    }

  private slots:
    void commitData_hook(QWidget* editor) {
      if ( commitData_event.func ) {
        typedef void (*func_type)(void *data, QWidgetH editor);
	(*(func_type)commitData_event.func)(commitData_event.data, (QWidgetH)editor);
      }
    }
    void closeEditor_hook(QWidget* editor, QAbstractItemDelegate::EndEditHint hint) {
      if ( closeEditor_event.func ) {
        typedef void (*func_type)(void *data, QWidgetH editor, QAbstractItemDelegate::EndEditHint hint);
	(*(func_type)closeEditor_event.func)(closeEditor_event.data, (QWidgetH)editor, hint);
      }
    }
    void closeEditor2_hook(QWidget* editor) {
      if ( closeEditor2_event.func ) {
        typedef void (*func_type)(void *data, QWidgetH editor);
	(*(func_type)closeEditor2_event.func)(closeEditor2_event.data, (QWidgetH)editor);
      }
    }
    void sizeHintChanged_hook(const QModelIndex& AnonParam1) {
      if ( sizeHintChanged_event.func ) {
        typedef void (*func_type)(void *data, const QModelIndexH AnonParam1);
	(*(func_type)sizeHintChanged_event.func)(sizeHintChanged_event.data, (const QModelIndexH)&AnonParam1);
      }
    }
  private:
    QHook commitData_event;
    QHook closeEditor_event;
    QHook closeEditor2_event;
    QHook sizeHintChanged_event;
};


#endif
