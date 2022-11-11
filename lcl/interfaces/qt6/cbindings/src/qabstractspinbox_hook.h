//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QABSTRACTSPINBOX_HOOK_H
#define QABSTRACTSPINBOX_HOOK_H

#include <qabstractspinbox.h>

#include "qwidget_hook.h"

class QAbstractSpinBox_hook : public QWidget_hook {
  Q_OBJECT
  public:
    QAbstractSpinBox_hook(QObject *handle) : QWidget_hook(handle) {
      editingFinished_event.func = NULL;
    }
    void hook_editingFinished(QHook &hook) { 
      if ( !editingFinished_event.func )
        connect(handle, SIGNAL(editingFinished()), this, SLOT(editingFinished_hook()));
      editingFinished_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(editingFinished()), this, SLOT(editingFinished_hook()));
    }

  private slots:
    void editingFinished_hook() {
      if ( editingFinished_event.func ) {
        typedef void (*func_type)(void *data);
	(*(func_type)editingFinished_event.func)(editingFinished_event.data);
      }
    }
  private:
    QHook editingFinished_event;
};


#endif
