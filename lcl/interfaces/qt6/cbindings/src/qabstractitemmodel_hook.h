//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QABSTRACTITEMMODEL_HOOK_H
#define QABSTRACTITEMMODEL_HOOK_H

#include <qabstractitemmodel.h>

#include "qobject_hook.h"

class QModelIndex_hook : public QObject_hook {
  Q_OBJECT
  public:
    QModelIndex_hook(QObject *handle) : QObject_hook(handle) {
    }
};


#include "qobject_hook.h"

class QPersistentModelIndex_hook : public QObject_hook {
  Q_OBJECT
  public:
    QPersistentModelIndex_hook(QObject *handle) : QObject_hook(handle) {
    }
};


#include "qobject_hook.h"

class QAbstractItemModel_hook : public QObject_hook {
  Q_OBJECT
  public:
    QAbstractItemModel_hook(QObject *handle) : QObject_hook(handle) {
      dataChanged_event.func = NULL;
      headerDataChanged_event.func = NULL;
      layoutChanged_event.func = NULL;
      layoutAboutToBeChanged_event.func = NULL;
      rowsAboutToBeInserted_event.func = NULL;
      rowsInserted_event.func = NULL;
      rowsAboutToBeRemoved_event.func = NULL;
      rowsRemoved_event.func = NULL;
      columnsAboutToBeInserted_event.func = NULL;
      columnsInserted_event.func = NULL;
      columnsAboutToBeRemoved_event.func = NULL;
      columnsRemoved_event.func = NULL;
      modelAboutToBeReset_event.func = NULL;
      modelReset_event.func = NULL;
      rowsAboutToBeMoved_event.func = NULL;
      rowsMoved_event.func = NULL;
      columnsAboutToBeMoved_event.func = NULL;
      columnsMoved_event.func = NULL;
    }
    void hook_dataChanged(QHook &hook) { 
      if ( !dataChanged_event.func )
        connect(handle, SIGNAL(dataChanged(const QModelIndex&, const QModelIndex&)), this, SLOT(dataChanged_hook(const QModelIndex&, const QModelIndex&)));
      dataChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(dataChanged(const QModelIndex&, const QModelIndex&)), this, SLOT(dataChanged_hook(const QModelIndex&, const QModelIndex&)));
    }
    void hook_headerDataChanged(QHook &hook) { 
      if ( !headerDataChanged_event.func )
        connect(handle, SIGNAL(headerDataChanged(Qt::Orientation, int, int)), this, SLOT(headerDataChanged_hook(Qt::Orientation, int, int)));
      headerDataChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(headerDataChanged(Qt::Orientation, int, int)), this, SLOT(headerDataChanged_hook(Qt::Orientation, int, int)));
    }
    void hook_layoutChanged(QHook &hook) { 
      if ( !layoutChanged_event.func )
        connect(handle, SIGNAL(layoutChanged()), this, SLOT(layoutChanged_hook()));
      layoutChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(layoutChanged()), this, SLOT(layoutChanged_hook()));
    }
    void hook_layoutAboutToBeChanged(QHook &hook) { 
      if ( !layoutAboutToBeChanged_event.func )
        connect(handle, SIGNAL(layoutAboutToBeChanged()), this, SLOT(layoutAboutToBeChanged_hook()));
      layoutAboutToBeChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(layoutAboutToBeChanged()), this, SLOT(layoutAboutToBeChanged_hook()));
    }
    void hook_rowsAboutToBeInserted(QHook &hook) { 
      if ( !rowsAboutToBeInserted_event.func )
        connect(handle, SIGNAL(rowsAboutToBeInserted(const QModelIndex&, int, int)), this, SLOT(rowsAboutToBeInserted_hook(const QModelIndex&, int, int)));
      rowsAboutToBeInserted_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(rowsAboutToBeInserted(const QModelIndex&, int, int)), this, SLOT(rowsAboutToBeInserted_hook(const QModelIndex&, int, int)));
    }
    void hook_rowsInserted(QHook &hook) { 
      if ( !rowsInserted_event.func )
        connect(handle, SIGNAL(rowsInserted(const QModelIndex&, int, int)), this, SLOT(rowsInserted_hook(const QModelIndex&, int, int)));
      rowsInserted_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(rowsInserted(const QModelIndex&, int, int)), this, SLOT(rowsInserted_hook(const QModelIndex&, int, int)));
    }
    void hook_rowsAboutToBeRemoved(QHook &hook) { 
      if ( !rowsAboutToBeRemoved_event.func )
        connect(handle, SIGNAL(rowsAboutToBeRemoved(const QModelIndex&, int, int)), this, SLOT(rowsAboutToBeRemoved_hook(const QModelIndex&, int, int)));
      rowsAboutToBeRemoved_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(rowsAboutToBeRemoved(const QModelIndex&, int, int)), this, SLOT(rowsAboutToBeRemoved_hook(const QModelIndex&, int, int)));
    }
    void hook_rowsRemoved(QHook &hook) { 
      if ( !rowsRemoved_event.func )
        connect(handle, SIGNAL(rowsRemoved(const QModelIndex&, int, int)), this, SLOT(rowsRemoved_hook(const QModelIndex&, int, int)));
      rowsRemoved_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(rowsRemoved(const QModelIndex&, int, int)), this, SLOT(rowsRemoved_hook(const QModelIndex&, int, int)));
    }
    void hook_columnsAboutToBeInserted(QHook &hook) { 
      if ( !columnsAboutToBeInserted_event.func )
        connect(handle, SIGNAL(columnsAboutToBeInserted(const QModelIndex&, int, int)), this, SLOT(columnsAboutToBeInserted_hook(const QModelIndex&, int, int)));
      columnsAboutToBeInserted_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(columnsAboutToBeInserted(const QModelIndex&, int, int)), this, SLOT(columnsAboutToBeInserted_hook(const QModelIndex&, int, int)));
    }
    void hook_columnsInserted(QHook &hook) { 
      if ( !columnsInserted_event.func )
        connect(handle, SIGNAL(columnsInserted(const QModelIndex&, int, int)), this, SLOT(columnsInserted_hook(const QModelIndex&, int, int)));
      columnsInserted_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(columnsInserted(const QModelIndex&, int, int)), this, SLOT(columnsInserted_hook(const QModelIndex&, int, int)));
    }
    void hook_columnsAboutToBeRemoved(QHook &hook) { 
      if ( !columnsAboutToBeRemoved_event.func )
        connect(handle, SIGNAL(columnsAboutToBeRemoved(const QModelIndex&, int, int)), this, SLOT(columnsAboutToBeRemoved_hook(const QModelIndex&, int, int)));
      columnsAboutToBeRemoved_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(columnsAboutToBeRemoved(const QModelIndex&, int, int)), this, SLOT(columnsAboutToBeRemoved_hook(const QModelIndex&, int, int)));
    }
    void hook_columnsRemoved(QHook &hook) { 
      if ( !columnsRemoved_event.func )
        connect(handle, SIGNAL(columnsRemoved(const QModelIndex&, int, int)), this, SLOT(columnsRemoved_hook(const QModelIndex&, int, int)));
      columnsRemoved_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(columnsRemoved(const QModelIndex&, int, int)), this, SLOT(columnsRemoved_hook(const QModelIndex&, int, int)));
    }
    void hook_modelAboutToBeReset(QHook &hook) { 
      if ( !modelAboutToBeReset_event.func )
        connect(handle, SIGNAL(modelAboutToBeReset()), this, SLOT(modelAboutToBeReset_hook()));
      modelAboutToBeReset_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(modelAboutToBeReset()), this, SLOT(modelAboutToBeReset_hook()));
    }
    void hook_modelReset(QHook &hook) { 
      if ( !modelReset_event.func )
        connect(handle, SIGNAL(modelReset()), this, SLOT(modelReset_hook()));
      modelReset_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(modelReset()), this, SLOT(modelReset_hook()));
    }
    void hook_rowsAboutToBeMoved(QHook &hook) { 
      if ( !rowsAboutToBeMoved_event.func )
        connect(handle, SIGNAL(rowsAboutToBeMoved(const QModelIndex&, int, int, const QModelIndex&, int)), this, SLOT(rowsAboutToBeMoved_hook(const QModelIndex&, int, int, const QModelIndex&, int)));
      rowsAboutToBeMoved_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(rowsAboutToBeMoved(const QModelIndex&, int, int, const QModelIndex&, int)), this, SLOT(rowsAboutToBeMoved_hook(const QModelIndex&, int, int, const QModelIndex&, int)));
    }
    void hook_rowsMoved(QHook &hook) { 
      if ( !rowsMoved_event.func )
        connect(handle, SIGNAL(rowsMoved(const QModelIndex&, int, int, const QModelIndex&, int)), this, SLOT(rowsMoved_hook(const QModelIndex&, int, int, const QModelIndex&, int)));
      rowsMoved_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(rowsMoved(const QModelIndex&, int, int, const QModelIndex&, int)), this, SLOT(rowsMoved_hook(const QModelIndex&, int, int, const QModelIndex&, int)));
    }
    void hook_columnsAboutToBeMoved(QHook &hook) { 
      if ( !columnsAboutToBeMoved_event.func )
        connect(handle, SIGNAL(columnsAboutToBeMoved(const QModelIndex&, int, int, const QModelIndex&, int)), this, SLOT(columnsAboutToBeMoved_hook(const QModelIndex&, int, int, const QModelIndex&, int)));
      columnsAboutToBeMoved_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(columnsAboutToBeMoved(const QModelIndex&, int, int, const QModelIndex&, int)), this, SLOT(columnsAboutToBeMoved_hook(const QModelIndex&, int, int, const QModelIndex&, int)));
    }
    void hook_columnsMoved(QHook &hook) { 
      if ( !columnsMoved_event.func )
        connect(handle, SIGNAL(columnsMoved(const QModelIndex&, int, int, const QModelIndex&, int)), this, SLOT(columnsMoved_hook(const QModelIndex&, int, int, const QModelIndex&, int)));
      columnsMoved_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(columnsMoved(const QModelIndex&, int, int, const QModelIndex&, int)), this, SLOT(columnsMoved_hook(const QModelIndex&, int, int, const QModelIndex&, int)));
    }

  private slots:
    void dataChanged_hook(const QModelIndex& topLeft, const QModelIndex& bottomRight) {
      if ( dataChanged_event.func ) {
        typedef void (*func_type)(void *data, const QModelIndexH topLeft, const QModelIndexH bottomRight);
	(*(func_type)dataChanged_event.func)(dataChanged_event.data, (const QModelIndexH)&topLeft, (const QModelIndexH)&bottomRight);
      }
    }
    void headerDataChanged_hook(Qt::Orientation orientation, int first, int last) {
      if ( headerDataChanged_event.func ) {
        typedef void (*func_type)(void *data, Qt::Orientation orientation, int first, int last);
	(*(func_type)headerDataChanged_event.func)(headerDataChanged_event.data, orientation, first, last);
      }
    }
    void layoutChanged_hook() {
      if ( layoutChanged_event.func ) {
        typedef void (*func_type)(void *data);
	(*(func_type)layoutChanged_event.func)(layoutChanged_event.data);
      }
    }
    void layoutAboutToBeChanged_hook() {
      if ( layoutAboutToBeChanged_event.func ) {
        typedef void (*func_type)(void *data);
	(*(func_type)layoutAboutToBeChanged_event.func)(layoutAboutToBeChanged_event.data);
      }
    }
    void rowsAboutToBeInserted_hook(const QModelIndex& parent, int first, int last) {
      if ( rowsAboutToBeInserted_event.func ) {
        typedef void (*func_type)(void *data, const QModelIndexH parent, int first, int last);
	(*(func_type)rowsAboutToBeInserted_event.func)(rowsAboutToBeInserted_event.data, (const QModelIndexH)&parent, first, last);
      }
    }
    void rowsInserted_hook(const QModelIndex& parent, int first, int last) {
      if ( rowsInserted_event.func ) {
        typedef void (*func_type)(void *data, const QModelIndexH parent, int first, int last);
	(*(func_type)rowsInserted_event.func)(rowsInserted_event.data, (const QModelIndexH)&parent, first, last);
      }
    }
    void rowsAboutToBeRemoved_hook(const QModelIndex& parent, int first, int last) {
      if ( rowsAboutToBeRemoved_event.func ) {
        typedef void (*func_type)(void *data, const QModelIndexH parent, int first, int last);
	(*(func_type)rowsAboutToBeRemoved_event.func)(rowsAboutToBeRemoved_event.data, (const QModelIndexH)&parent, first, last);
      }
    }
    void rowsRemoved_hook(const QModelIndex& parent, int first, int last) {
      if ( rowsRemoved_event.func ) {
        typedef void (*func_type)(void *data, const QModelIndexH parent, int first, int last);
	(*(func_type)rowsRemoved_event.func)(rowsRemoved_event.data, (const QModelIndexH)&parent, first, last);
      }
    }
    void columnsAboutToBeInserted_hook(const QModelIndex& parent, int first, int last) {
      if ( columnsAboutToBeInserted_event.func ) {
        typedef void (*func_type)(void *data, const QModelIndexH parent, int first, int last);
	(*(func_type)columnsAboutToBeInserted_event.func)(columnsAboutToBeInserted_event.data, (const QModelIndexH)&parent, first, last);
      }
    }
    void columnsInserted_hook(const QModelIndex& parent, int first, int last) {
      if ( columnsInserted_event.func ) {
        typedef void (*func_type)(void *data, const QModelIndexH parent, int first, int last);
	(*(func_type)columnsInserted_event.func)(columnsInserted_event.data, (const QModelIndexH)&parent, first, last);
      }
    }
    void columnsAboutToBeRemoved_hook(const QModelIndex& parent, int first, int last) {
      if ( columnsAboutToBeRemoved_event.func ) {
        typedef void (*func_type)(void *data, const QModelIndexH parent, int first, int last);
	(*(func_type)columnsAboutToBeRemoved_event.func)(columnsAboutToBeRemoved_event.data, (const QModelIndexH)&parent, first, last);
      }
    }
    void columnsRemoved_hook(const QModelIndex& parent, int first, int last) {
      if ( columnsRemoved_event.func ) {
        typedef void (*func_type)(void *data, const QModelIndexH parent, int first, int last);
	(*(func_type)columnsRemoved_event.func)(columnsRemoved_event.data, (const QModelIndexH)&parent, first, last);
      }
    }
    void modelAboutToBeReset_hook() {
      if ( modelAboutToBeReset_event.func ) {
        typedef void (*func_type)(void *data);
	(*(func_type)modelAboutToBeReset_event.func)(modelAboutToBeReset_event.data);
      }
    }
    void modelReset_hook() {
      if ( modelReset_event.func ) {
        typedef void (*func_type)(void *data);
	(*(func_type)modelReset_event.func)(modelReset_event.data);
      }
    }
    void rowsAboutToBeMoved_hook(const QModelIndex& sourceParent, int sourceStart, int sourceEnd, const QModelIndex& destinationParent, int destinationRow) {
      if ( rowsAboutToBeMoved_event.func ) {
        typedef void (*func_type)(void *data, const QModelIndexH sourceParent, int sourceStart, int sourceEnd, const QModelIndexH destinationParent, int destinationRow);
	(*(func_type)rowsAboutToBeMoved_event.func)(rowsAboutToBeMoved_event.data, (const QModelIndexH)&sourceParent, sourceStart, sourceEnd, (const QModelIndexH)&destinationParent, destinationRow);
      }
    }
    void rowsMoved_hook(const QModelIndex& parent, int start, int end, const QModelIndex& destination, int row) {
      if ( rowsMoved_event.func ) {
        typedef void (*func_type)(void *data, const QModelIndexH parent, int start, int end, const QModelIndexH destination, int row);
	(*(func_type)rowsMoved_event.func)(rowsMoved_event.data, (const QModelIndexH)&parent, start, end, (const QModelIndexH)&destination, row);
      }
    }
    void columnsAboutToBeMoved_hook(const QModelIndex& sourceParent, int sourceStart, int sourceEnd, const QModelIndex& destinationParent, int destinationColumn) {
      if ( columnsAboutToBeMoved_event.func ) {
        typedef void (*func_type)(void *data, const QModelIndexH sourceParent, int sourceStart, int sourceEnd, const QModelIndexH destinationParent, int destinationColumn);
	(*(func_type)columnsAboutToBeMoved_event.func)(columnsAboutToBeMoved_event.data, (const QModelIndexH)&sourceParent, sourceStart, sourceEnd, (const QModelIndexH)&destinationParent, destinationColumn);
      }
    }
    void columnsMoved_hook(const QModelIndex& parent, int start, int end, const QModelIndex& destination, int column) {
      if ( columnsMoved_event.func ) {
        typedef void (*func_type)(void *data, const QModelIndexH parent, int start, int end, const QModelIndexH destination, int column);
	(*(func_type)columnsMoved_event.func)(columnsMoved_event.data, (const QModelIndexH)&parent, start, end, (const QModelIndexH)&destination, column);
      }
    }
  private:
    QHook dataChanged_event;
    QHook headerDataChanged_event;
    QHook layoutChanged_event;
    QHook layoutAboutToBeChanged_event;
    QHook rowsAboutToBeInserted_event;
    QHook rowsInserted_event;
    QHook rowsAboutToBeRemoved_event;
    QHook rowsRemoved_event;
    QHook columnsAboutToBeInserted_event;
    QHook columnsInserted_event;
    QHook columnsAboutToBeRemoved_event;
    QHook columnsRemoved_event;
    QHook modelAboutToBeReset_event;
    QHook modelReset_event;
    QHook rowsAboutToBeMoved_event;
    QHook rowsMoved_event;
    QHook columnsAboutToBeMoved_event;
    QHook columnsMoved_event;
};


#include "qabstractitemmodel_hook.h"

class QAbstractTableModel_hook : public QAbstractItemModel_hook {
  Q_OBJECT
  public:
    QAbstractTableModel_hook(QObject *handle) : QAbstractItemModel_hook(handle) {
    }
};


#include "qabstractitemmodel_hook.h"

class QAbstractListModel_hook : public QAbstractItemModel_hook {
  Q_OBJECT
  public:
    QAbstractListModel_hook(QObject *handle) : QAbstractItemModel_hook(handle) {
    }
};


#endif
