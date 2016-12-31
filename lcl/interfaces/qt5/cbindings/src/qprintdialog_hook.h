//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QPRINTDIALOG_HOOK_H
#define QPRINTDIALOG_HOOK_H

#include <qprintdialog.h>

#include "qabstractprintdialog_hook.h"

class QPrintDialog_hook : public QAbstractPrintDialog_hook {
  Q_OBJECT
  public:
    QPrintDialog_hook(QObject *handle) : QAbstractPrintDialog_hook(handle) {
      accepted_event.func = NULL;
    }
    void hook_accepted(QHook &hook) { 
      if ( !accepted_event.func )
        connect(handle, SIGNAL(accepted(QPrinter*)), this, SLOT(accepted_hook(QPrinter*)));
      accepted_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(accepted(QPrinter*)), this, SLOT(accepted_hook(QPrinter*)));
    }

  private slots:
    void accepted_hook(QPrinter* printer) {
      if ( accepted_event.func ) {
        typedef void (*func_type)(void *data, QPrinterH printer);
	(*(func_type)accepted_event.func)(accepted_event.data, (QPrinterH)printer);
      }
    }
  private:
    QHook accepted_event;
};


#endif
