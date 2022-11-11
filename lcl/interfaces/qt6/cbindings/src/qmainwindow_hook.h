//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QMAINWINDOW_HOOK_H
#define QMAINWINDOW_HOOK_H

#include <qmainwindow.h>

#include "qwidget_hook.h"

class QMainWindow_hook : public QWidget_hook {
  Q_OBJECT
  public:
    QMainWindow_hook(QObject *handle) : QWidget_hook(handle) {
      iconSizeChanged_event.func = NULL;
      toolButtonStyleChanged_event.func = NULL;
    }
    void hook_iconSizeChanged(QHook &hook) { 
      if ( !iconSizeChanged_event.func )
        connect(handle, SIGNAL(iconSizeChanged(const QSize&)), this, SLOT(iconSizeChanged_hook(const QSize&)));
      iconSizeChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(iconSizeChanged(const QSize&)), this, SLOT(iconSizeChanged_hook(const QSize&)));
    }
    void hook_toolButtonStyleChanged(QHook &hook) { 
      if ( !toolButtonStyleChanged_event.func )
        connect(handle, SIGNAL(toolButtonStyleChanged(Qt::ToolButtonStyle)), this, SLOT(toolButtonStyleChanged_hook(Qt::ToolButtonStyle)));
      toolButtonStyleChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(toolButtonStyleChanged(Qt::ToolButtonStyle)), this, SLOT(toolButtonStyleChanged_hook(Qt::ToolButtonStyle)));
    }

  private slots:
    void iconSizeChanged_hook(const QSize& iconSize) {
      if ( iconSizeChanged_event.func ) {
        typedef void (*func_type)(void *data, const QSizeH iconSize);
	(*(func_type)iconSizeChanged_event.func)(iconSizeChanged_event.data, (const QSizeH)&iconSize);
      }
    }
    void toolButtonStyleChanged_hook(Qt::ToolButtonStyle toolButtonStyle) {
      if ( toolButtonStyleChanged_event.func ) {
        typedef void (*func_type)(void *data, Qt::ToolButtonStyle toolButtonStyle);
	(*(func_type)toolButtonStyleChanged_event.func)(toolButtonStyleChanged_event.data, toolButtonStyle);
      }
    }
  private:
    QHook iconSizeChanged_event;
    QHook toolButtonStyleChanged_event;
};


#endif
