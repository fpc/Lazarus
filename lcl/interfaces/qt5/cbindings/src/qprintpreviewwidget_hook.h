//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QPRINTPREVIEWWIDGET_HOOK_H
#define QPRINTPREVIEWWIDGET_HOOK_H

#include <qprintpreviewwidget.h>

#include "qwidget_hook.h"

class QPrintPreviewWidget_hook : public QWidget_hook {
  Q_OBJECT
  public:
    QPrintPreviewWidget_hook(QObject *handle) : QWidget_hook(handle) {
      paintRequested_event.func = NULL;
      previewChanged_event.func = NULL;
    }
    void hook_paintRequested(QHook &hook) { 
      if ( !paintRequested_event.func )
        connect(handle, SIGNAL(paintRequested(QPrinter*)), this, SLOT(paintRequested_hook(QPrinter*)));
      paintRequested_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(paintRequested(QPrinter*)), this, SLOT(paintRequested_hook(QPrinter*)));
    }
    void hook_previewChanged(QHook &hook) { 
      if ( !previewChanged_event.func )
        connect(handle, SIGNAL(previewChanged()), this, SLOT(previewChanged_hook()));
      previewChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(previewChanged()), this, SLOT(previewChanged_hook()));
    }

  private slots:
    void paintRequested_hook(QPrinter* printer) {
      if ( paintRequested_event.func ) {
        typedef void (*func_type)(void *data, QPrinterH printer);
	(*(func_type)paintRequested_event.func)(paintRequested_event.data, (QPrinterH)printer);
      }
    }
    void previewChanged_hook() {
      if ( previewChanged_event.func ) {
        typedef void (*func_type)(void *data);
	(*(func_type)previewChanged_event.func)(previewChanged_event.data);
      }
    }
  private:
    QHook paintRequested_event;
    QHook previewChanged_event;
};


#endif
