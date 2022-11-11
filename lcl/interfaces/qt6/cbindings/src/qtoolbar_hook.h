//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QTOOLBAR_HOOK_H
#define QTOOLBAR_HOOK_H

#include <qtoolbar.h>

#include "qwidget_hook.h"

class QToolBar_hook : public QWidget_hook {
  Q_OBJECT
  public:
    QToolBar_hook(QObject *handle) : QWidget_hook(handle) {
      actionTriggered_event.func = NULL;
      movableChanged_event.func = NULL;
      allowedAreasChanged_event.func = NULL;
      orientationChanged_event.func = NULL;
      iconSizeChanged_event.func = NULL;
      toolButtonStyleChanged_event.func = NULL;
      topLevelChanged_event.func = NULL;
      visibilityChanged_event.func = NULL;
    }
    void hook_actionTriggered(QHook &hook) { 
      if ( !actionTriggered_event.func )
        connect(handle, SIGNAL(actionTriggered(QAction*)), this, SLOT(actionTriggered_hook(QAction*)));
      actionTriggered_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(actionTriggered(QAction*)), this, SLOT(actionTriggered_hook(QAction*)));
    }
    void hook_movableChanged(QHook &hook) { 
      if ( !movableChanged_event.func )
        connect(handle, SIGNAL(movableChanged(bool)), this, SLOT(movableChanged_hook(bool)));
      movableChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(movableChanged(bool)), this, SLOT(movableChanged_hook(bool)));
    }
    void hook_allowedAreasChanged(QHook &hook) { 
      if ( !allowedAreasChanged_event.func )
        connect(handle, SIGNAL(allowedAreasChanged(Qt::ToolBarAreas)), this, SLOT(allowedAreasChanged_hook(Qt::ToolBarAreas)));
      allowedAreasChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(allowedAreasChanged(Qt::ToolBarAreas)), this, SLOT(allowedAreasChanged_hook(Qt::ToolBarAreas)));
    }
    void hook_orientationChanged(QHook &hook) { 
      if ( !orientationChanged_event.func )
        connect(handle, SIGNAL(orientationChanged(Qt::Orientation)), this, SLOT(orientationChanged_hook(Qt::Orientation)));
      orientationChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(orientationChanged(Qt::Orientation)), this, SLOT(orientationChanged_hook(Qt::Orientation)));
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
    void hook_topLevelChanged(QHook &hook) { 
      if ( !topLevelChanged_event.func )
        connect(handle, SIGNAL(topLevelChanged(bool)), this, SLOT(topLevelChanged_hook(bool)));
      topLevelChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(topLevelChanged(bool)), this, SLOT(topLevelChanged_hook(bool)));
    }
    void hook_visibilityChanged(QHook &hook) { 
      if ( !visibilityChanged_event.func )
        connect(handle, SIGNAL(visibilityChanged(bool)), this, SLOT(visibilityChanged_hook(bool)));
      visibilityChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(visibilityChanged(bool)), this, SLOT(visibilityChanged_hook(bool)));
    }

  private slots:
    void actionTriggered_hook(QAction* action) {
      if ( actionTriggered_event.func ) {
        typedef void (*func_type)(void *data, QActionH action);
	(*(func_type)actionTriggered_event.func)(actionTriggered_event.data, (QActionH)action);
      }
    }
    void movableChanged_hook(bool movable) {
      if ( movableChanged_event.func ) {
        typedef void (*func_type)(void *data, bool movable);
	(*(func_type)movableChanged_event.func)(movableChanged_event.data, movable);
      }
    }
    void allowedAreasChanged_hook(Qt::ToolBarAreas allowedAreas) {
      if ( allowedAreasChanged_event.func ) {
        typedef void (*func_type)(void *data, unsigned int allowedAreas);
	(*(func_type)allowedAreasChanged_event.func)(allowedAreasChanged_event.data, (unsigned int)allowedAreas);
      }
    }
    void orientationChanged_hook(Qt::Orientation orientation) {
      if ( orientationChanged_event.func ) {
        typedef void (*func_type)(void *data, Qt::Orientation orientation);
	(*(func_type)orientationChanged_event.func)(orientationChanged_event.data, orientation);
      }
    }
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
    void topLevelChanged_hook(bool topLevel) {
      if ( topLevelChanged_event.func ) {
        typedef void (*func_type)(void *data, bool topLevel);
	(*(func_type)topLevelChanged_event.func)(topLevelChanged_event.data, topLevel);
      }
    }
    void visibilityChanged_hook(bool visible) {
      if ( visibilityChanged_event.func ) {
        typedef void (*func_type)(void *data, bool visible);
	(*(func_type)visibilityChanged_event.func)(visibilityChanged_event.data, visible);
      }
    }
  private:
    QHook actionTriggered_event;
    QHook movableChanged_event;
    QHook allowedAreasChanged_event;
    QHook orientationChanged_event;
    QHook iconSizeChanged_event;
    QHook toolButtonStyleChanged_event;
    QHook topLevelChanged_event;
    QHook visibilityChanged_event;
};


#endif
