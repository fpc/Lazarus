//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QPRINTPREVIEWDIALOG_HOOK_H
#define QPRINTPREVIEWDIALOG_HOOK_H

#include <qprintpreviewdialog.h>

#include "qdialog_hook.h"

class QPrintPreviewDialog_hook : public QDialog_hook {
  Q_OBJECT
  public:
    QPrintPreviewDialog_hook(QObject *handle) : QDialog_hook(handle) {
      paintRequested_event.func = NULL;
    }
    void hook_paintRequested(QHook &hook) { 
      if ( !paintRequested_event.func )
        connect(handle, SIGNAL(paintRequested(QPrinter*)), this, SLOT(paintRequested_hook(QPrinter*)));
      paintRequested_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(paintRequested(QPrinter*)), this, SLOT(paintRequested_hook(QPrinter*)));
    }

  private slots:
    void paintRequested_hook(QPrinter* printer) {
      if ( paintRequested_event.func ) {
        typedef void (*func_type)(void *data, QPrinterH printer);
	(*(func_type)paintRequested_event.func)(paintRequested_event.data, (QPrinterH)printer);
      }
    }
  private:
    QHook paintRequested_event;
};


#endif
