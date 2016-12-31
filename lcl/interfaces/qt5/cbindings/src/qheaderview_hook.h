//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QHEADERVIEW_HOOK_H
#define QHEADERVIEW_HOOK_H

#include <qheaderview.h>

#include "qabstractitemview_hook.h"

class QHeaderView_hook : public QAbstractItemView_hook {
  Q_OBJECT
  public:
    QHeaderView_hook(QObject *handle) : QAbstractItemView_hook(handle) {
      sectionMoved_event.func = NULL;
      sectionResized_event.func = NULL;
      sectionPressed_event.func = NULL;
      sectionClicked_event.func = NULL;
      sectionEntered_event.func = NULL;
      sectionDoubleClicked_event.func = NULL;
      sectionCountChanged_event.func = NULL;
      sectionHandleDoubleClicked_event.func = NULL;
      geometriesChanged_event.func = NULL;
      sortIndicatorChanged_event.func = NULL;
    }
    void hook_sectionMoved(QHook &hook) { 
      if ( !sectionMoved_event.func )
        connect(handle, SIGNAL(sectionMoved(int, int, int)), this, SLOT(sectionMoved_hook(int, int, int)));
      sectionMoved_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(sectionMoved(int, int, int)), this, SLOT(sectionMoved_hook(int, int, int)));
    }
    void hook_sectionResized(QHook &hook) { 
      if ( !sectionResized_event.func )
        connect(handle, SIGNAL(sectionResized(int, int, int)), this, SLOT(sectionResized_hook(int, int, int)));
      sectionResized_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(sectionResized(int, int, int)), this, SLOT(sectionResized_hook(int, int, int)));
    }
    void hook_sectionPressed(QHook &hook) { 
      if ( !sectionPressed_event.func )
        connect(handle, SIGNAL(sectionPressed(int)), this, SLOT(sectionPressed_hook(int)));
      sectionPressed_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(sectionPressed(int)), this, SLOT(sectionPressed_hook(int)));
    }
    void hook_sectionClicked(QHook &hook) { 
      if ( !sectionClicked_event.func )
        connect(handle, SIGNAL(sectionClicked(int)), this, SLOT(sectionClicked_hook(int)));
      sectionClicked_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(sectionClicked(int)), this, SLOT(sectionClicked_hook(int)));
    }
    void hook_sectionEntered(QHook &hook) { 
      if ( !sectionEntered_event.func )
        connect(handle, SIGNAL(sectionEntered(int)), this, SLOT(sectionEntered_hook(int)));
      sectionEntered_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(sectionEntered(int)), this, SLOT(sectionEntered_hook(int)));
    }
    void hook_sectionDoubleClicked(QHook &hook) { 
      if ( !sectionDoubleClicked_event.func )
        connect(handle, SIGNAL(sectionDoubleClicked(int)), this, SLOT(sectionDoubleClicked_hook(int)));
      sectionDoubleClicked_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(sectionDoubleClicked(int)), this, SLOT(sectionDoubleClicked_hook(int)));
    }
    void hook_sectionCountChanged(QHook &hook) { 
      if ( !sectionCountChanged_event.func )
        connect(handle, SIGNAL(sectionCountChanged(int, int)), this, SLOT(sectionCountChanged_hook(int, int)));
      sectionCountChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(sectionCountChanged(int, int)), this, SLOT(sectionCountChanged_hook(int, int)));
    }
    void hook_sectionHandleDoubleClicked(QHook &hook) { 
      if ( !sectionHandleDoubleClicked_event.func )
        connect(handle, SIGNAL(sectionHandleDoubleClicked(int)), this, SLOT(sectionHandleDoubleClicked_hook(int)));
      sectionHandleDoubleClicked_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(sectionHandleDoubleClicked(int)), this, SLOT(sectionHandleDoubleClicked_hook(int)));
    }
    void hook_geometriesChanged(QHook &hook) { 
      if ( !geometriesChanged_event.func )
        connect(handle, SIGNAL(geometriesChanged()), this, SLOT(geometriesChanged_hook()));
      geometriesChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(geometriesChanged()), this, SLOT(geometriesChanged_hook()));
    }
    void hook_sortIndicatorChanged(QHook &hook) { 
      if ( !sortIndicatorChanged_event.func )
        connect(handle, SIGNAL(sortIndicatorChanged(int, Qt::SortOrder)), this, SLOT(sortIndicatorChanged_hook(int, Qt::SortOrder)));
      sortIndicatorChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(sortIndicatorChanged(int, Qt::SortOrder)), this, SLOT(sortIndicatorChanged_hook(int, Qt::SortOrder)));
    }

  private slots:
    void sectionMoved_hook(int logicalIndex, int oldVisualIndex, int newVisualIndex) {
      if ( sectionMoved_event.func ) {
        typedef void (*func_type)(void *data, int logicalIndex, int oldVisualIndex, int newVisualIndex);
	(*(func_type)sectionMoved_event.func)(sectionMoved_event.data, logicalIndex, oldVisualIndex, newVisualIndex);
      }
    }
    void sectionResized_hook(int logicalIndex, int oldSize, int newSize) {
      if ( sectionResized_event.func ) {
        typedef void (*func_type)(void *data, int logicalIndex, int oldSize, int newSize);
	(*(func_type)sectionResized_event.func)(sectionResized_event.data, logicalIndex, oldSize, newSize);
      }
    }
    void sectionPressed_hook(int logicalIndex) {
      if ( sectionPressed_event.func ) {
        typedef void (*func_type)(void *data, int logicalIndex);
	(*(func_type)sectionPressed_event.func)(sectionPressed_event.data, logicalIndex);
      }
    }
    void sectionClicked_hook(int logicalIndex) {
      if ( sectionClicked_event.func ) {
        typedef void (*func_type)(void *data, int logicalIndex);
	(*(func_type)sectionClicked_event.func)(sectionClicked_event.data, logicalIndex);
      }
    }
    void sectionEntered_hook(int logicalIndex) {
      if ( sectionEntered_event.func ) {
        typedef void (*func_type)(void *data, int logicalIndex);
	(*(func_type)sectionEntered_event.func)(sectionEntered_event.data, logicalIndex);
      }
    }
    void sectionDoubleClicked_hook(int logicalIndex) {
      if ( sectionDoubleClicked_event.func ) {
        typedef void (*func_type)(void *data, int logicalIndex);
	(*(func_type)sectionDoubleClicked_event.func)(sectionDoubleClicked_event.data, logicalIndex);
      }
    }
    void sectionCountChanged_hook(int oldCount, int newCount) {
      if ( sectionCountChanged_event.func ) {
        typedef void (*func_type)(void *data, int oldCount, int newCount);
	(*(func_type)sectionCountChanged_event.func)(sectionCountChanged_event.data, oldCount, newCount);
      }
    }
    void sectionHandleDoubleClicked_hook(int logicalIndex) {
      if ( sectionHandleDoubleClicked_event.func ) {
        typedef void (*func_type)(void *data, int logicalIndex);
	(*(func_type)sectionHandleDoubleClicked_event.func)(sectionHandleDoubleClicked_event.data, logicalIndex);
      }
    }
    void geometriesChanged_hook() {
      if ( geometriesChanged_event.func ) {
        typedef void (*func_type)(void *data);
	(*(func_type)geometriesChanged_event.func)(geometriesChanged_event.data);
      }
    }
    void sortIndicatorChanged_hook(int logicalIndex, Qt::SortOrder order) {
      if ( sortIndicatorChanged_event.func ) {
        typedef void (*func_type)(void *data, int logicalIndex, Qt::SortOrder order);
	(*(func_type)sortIndicatorChanged_event.func)(sortIndicatorChanged_event.data, logicalIndex, order);
      }
    }
  private:
    QHook sectionMoved_event;
    QHook sectionResized_event;
    QHook sectionPressed_event;
    QHook sectionClicked_event;
    QHook sectionEntered_event;
    QHook sectionDoubleClicked_event;
    QHook sectionCountChanged_event;
    QHook sectionHandleDoubleClicked_event;
    QHook geometriesChanged_event;
    QHook sortIndicatorChanged_event;
};


#endif
