//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QSTATUSBAR_HOOK_H
#define QSTATUSBAR_HOOK_H

#include <qstatusbar.h>

#include "qwidget_hook.h"

class QStatusBar_hook : public QWidget_hook {
  Q_OBJECT
  public:
    QStatusBar_hook(QObject *handle) : QWidget_hook(handle) {
      messageChanged_event.func = NULL;
    }
    void hook_messageChanged(QHook &hook) { 
      if ( !messageChanged_event.func )
        connect(handle, SIGNAL(messageChanged(const QString&)), this, SLOT(messageChanged_hook(const QString&)));
      messageChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(messageChanged(const QString&)), this, SLOT(messageChanged_hook(const QString&)));
    }

  private slots:
    void messageChanged_hook(const QString& text) {
      if ( messageChanged_event.func ) {
        typedef void (*func_type)(void *data, PWideString text);
	PWideString t_text;
	initializePWideString(t_text);
	copyQStringToPWideString(text, t_text);
	(*(func_type)messageChanged_event.func)(messageChanged_event.data, t_text);
	finalizePWideString(t_text);
      }
    }
  private:
    QHook messageChanged_event;
};


#endif
