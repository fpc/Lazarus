//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QDOCKWIDGET_HOOK_H
#define QDOCKWIDGET_HOOK_H

#include <qdockwidget.h>

#include "qwidget_hook.h"

class QDockWidget_hook : public QWidget_hook {
  Q_OBJECT
  public:
    QDockWidget_hook(QObject *handle) : QWidget_hook(handle) {
      featuresChanged_event.func = NULL;
      topLevelChanged_event.func = NULL;
      allowedAreasChanged_event.func = NULL;
      visibilityChanged_event.func = NULL;
      dockLocationChanged_event.func = NULL;
    }
    void hook_featuresChanged(QHook &hook) { 
      if ( !featuresChanged_event.func )
        connect(handle, SIGNAL(featuresChanged(QDockWidget::DockWidgetFeatures)), this, SLOT(featuresChanged_hook(QDockWidget::DockWidgetFeatures)));
      featuresChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(featuresChanged(QDockWidget::DockWidgetFeatures)), this, SLOT(featuresChanged_hook(QDockWidget::DockWidgetFeatures)));
    }
    void hook_topLevelChanged(QHook &hook) { 
      if ( !topLevelChanged_event.func )
        connect(handle, SIGNAL(topLevelChanged(bool)), this, SLOT(topLevelChanged_hook(bool)));
      topLevelChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(topLevelChanged(bool)), this, SLOT(topLevelChanged_hook(bool)));
    }
    void hook_allowedAreasChanged(QHook &hook) { 
      if ( !allowedAreasChanged_event.func )
        connect(handle, SIGNAL(allowedAreasChanged(Qt::DockWidgetAreas)), this, SLOT(allowedAreasChanged_hook(Qt::DockWidgetAreas)));
      allowedAreasChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(allowedAreasChanged(Qt::DockWidgetAreas)), this, SLOT(allowedAreasChanged_hook(Qt::DockWidgetAreas)));
    }
    void hook_visibilityChanged(QHook &hook) { 
      if ( !visibilityChanged_event.func )
        connect(handle, SIGNAL(visibilityChanged(bool)), this, SLOT(visibilityChanged_hook(bool)));
      visibilityChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(visibilityChanged(bool)), this, SLOT(visibilityChanged_hook(bool)));
    }
    void hook_dockLocationChanged(QHook &hook) { 
      if ( !dockLocationChanged_event.func )
        connect(handle, SIGNAL(dockLocationChanged(Qt::DockWidgetArea)), this, SLOT(dockLocationChanged_hook(Qt::DockWidgetArea)));
      dockLocationChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(dockLocationChanged(Qt::DockWidgetArea)), this, SLOT(dockLocationChanged_hook(Qt::DockWidgetArea)));
    }

  private slots:
    void featuresChanged_hook(QDockWidget::DockWidgetFeatures features) {
      if ( featuresChanged_event.func ) {
        typedef void (*func_type)(void *data, unsigned int features);
	(*(func_type)featuresChanged_event.func)(featuresChanged_event.data, (unsigned int)features);
      }
    }
    void topLevelChanged_hook(bool topLevel) {
      if ( topLevelChanged_event.func ) {
        typedef void (*func_type)(void *data, bool topLevel);
	(*(func_type)topLevelChanged_event.func)(topLevelChanged_event.data, topLevel);
      }
    }
    void allowedAreasChanged_hook(Qt::DockWidgetAreas allowedAreas) {
      if ( allowedAreasChanged_event.func ) {
        typedef void (*func_type)(void *data, unsigned int allowedAreas);
	(*(func_type)allowedAreasChanged_event.func)(allowedAreasChanged_event.data, (unsigned int)allowedAreas);
      }
    }
    void visibilityChanged_hook(bool visible) {
      if ( visibilityChanged_event.func ) {
        typedef void (*func_type)(void *data, bool visible);
	(*(func_type)visibilityChanged_event.func)(visibilityChanged_event.data, visible);
      }
    }
    void dockLocationChanged_hook(Qt::DockWidgetArea area) {
      if ( dockLocationChanged_event.func ) {
        typedef void (*func_type)(void *data, Qt::DockWidgetArea area);
	(*(func_type)dockLocationChanged_event.func)(dockLocationChanged_event.data, area);
      }
    }
  private:
    QHook featuresChanged_event;
    QHook topLevelChanged_event;
    QHook allowedAreasChanged_event;
    QHook visibilityChanged_event;
    QHook dockLocationChanged_event;
};


#endif
