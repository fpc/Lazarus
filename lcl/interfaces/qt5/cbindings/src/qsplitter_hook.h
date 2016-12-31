//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QSPLITTER_HOOK_H
#define QSPLITTER_HOOK_H

#include <qsplitter.h>

#include "qframe_hook.h"

class QSplitter_hook : public QFrame_hook {
  Q_OBJECT
  public:
    QSplitter_hook(QObject *handle) : QFrame_hook(handle) {
      splitterMoved_event.func = NULL;
    }
    void hook_splitterMoved(QHook &hook) { 
      if ( !splitterMoved_event.func )
        connect(handle, SIGNAL(splitterMoved(int, int)), this, SLOT(splitterMoved_hook(int, int)));
      splitterMoved_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(splitterMoved(int, int)), this, SLOT(splitterMoved_hook(int, int)));
    }

  private slots:
    void splitterMoved_hook(int pos, int index) {
      if ( splitterMoved_event.func ) {
        typedef void (*func_type)(void *data, int pos, int index);
	(*(func_type)splitterMoved_event.func)(splitterMoved_event.data, pos, index);
      }
    }
  private:
    QHook splitterMoved_event;
};


#include "qwidget_hook.h"

class QSplitterHandle_hook : public QWidget_hook {
  Q_OBJECT
  public:
    QSplitterHandle_hook(QObject *handle) : QWidget_hook(handle) {
    }
};


#endif
