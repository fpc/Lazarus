//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QCALENDARWIDGET_HOOK_H
#define QCALENDARWIDGET_HOOK_H

#include <qcalendarwidget.h>

#include "qwidget_hook.h"

class QCalendarWidget_hook : public QWidget_hook {
  Q_OBJECT
  public:
    QCalendarWidget_hook(QObject *handle) : QWidget_hook(handle) {
      selectionChanged_event.func = NULL;
      clicked_event.func = NULL;
      activated_event.func = NULL;
      currentPageChanged_event.func = NULL;
    }
    void hook_selectionChanged(QHook &hook) { 
      if ( !selectionChanged_event.func )
        connect(handle, SIGNAL(selectionChanged()), this, SLOT(selectionChanged_hook()));
      selectionChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(selectionChanged()), this, SLOT(selectionChanged_hook()));
    }
    void hook_clicked(QHook &hook) { 
      if ( !clicked_event.func )
        connect(handle, SIGNAL(clicked(const QDate&)), this, SLOT(clicked_hook(const QDate&)));
      clicked_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(clicked(const QDate&)), this, SLOT(clicked_hook(const QDate&)));
    }
    void hook_activated(QHook &hook) { 
      if ( !activated_event.func )
        connect(handle, SIGNAL(activated(const QDate&)), this, SLOT(activated_hook(const QDate&)));
      activated_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(activated(const QDate&)), this, SLOT(activated_hook(const QDate&)));
    }
    void hook_currentPageChanged(QHook &hook) { 
      if ( !currentPageChanged_event.func )
        connect(handle, SIGNAL(currentPageChanged(int, int)), this, SLOT(currentPageChanged_hook(int, int)));
      currentPageChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(currentPageChanged(int, int)), this, SLOT(currentPageChanged_hook(int, int)));
    }

  private slots:
    void selectionChanged_hook() {
      if ( selectionChanged_event.func ) {
        typedef void (*func_type)(void *data);
	(*(func_type)selectionChanged_event.func)(selectionChanged_event.data);
      }
    }
    void clicked_hook(const QDate& date) {
      if ( clicked_event.func ) {
        typedef void (*func_type)(void *data, const QDateH date);
	(*(func_type)clicked_event.func)(clicked_event.data, (const QDateH)&date);
      }
    }
    void activated_hook(const QDate& date) {
      if ( activated_event.func ) {
        typedef void (*func_type)(void *data, const QDateH date);
	(*(func_type)activated_event.func)(activated_event.data, (const QDateH)&date);
      }
    }
    void currentPageChanged_hook(int year, int month) {
      if ( currentPageChanged_event.func ) {
        typedef void (*func_type)(void *data, int year, int month);
	(*(func_type)currentPageChanged_event.func)(currentPageChanged_event.data, year, month);
      }
    }
  private:
    QHook selectionChanged_event;
    QHook clicked_event;
    QHook activated_event;
    QHook currentPageChanged_event;
};


#endif
