//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QAPPLICATION_HOOK_H
#define QAPPLICATION_HOOK_H

#include <qapplication.h>

#include "qguiapplication_hook.h"

class QApplication_hook : public QGuiApplication_hook {
  Q_OBJECT
  public:
    QApplication_hook(QObject *handle) : QGuiApplication_hook(handle) {
      focusChanged_event.func = NULL;
    }
    void hook_focusChanged(QHook &hook) { 
      if ( !focusChanged_event.func )
        connect(handle, SIGNAL(focusChanged(QWidget*, QWidget*)), this, SLOT(focusChanged_hook(QWidget*, QWidget*)));
      focusChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(focusChanged(QWidget*, QWidget*)), this, SLOT(focusChanged_hook(QWidget*, QWidget*)));
    }

  private slots:
    void focusChanged_hook(QWidget* old, QWidget* now) {
      if ( focusChanged_event.func ) {
        typedef void (*func_type)(void *data, QWidgetH old, QWidgetH now);
	(*(func_type)focusChanged_event.func)(focusChanged_event.data, (QWidgetH)old, (QWidgetH)now);
      }
    }
  private:
    QHook focusChanged_event;
};


#endif
