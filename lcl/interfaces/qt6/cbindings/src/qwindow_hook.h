//******************************************************************************
//  Copyright (c) 2017 by Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QWINDOW_HOOK_H
#define QWINDOW_HOOK_H

#include <qwindow.h>

#include "qobject_hook.h"

class QWindow_hook : public QObject_hook {
  Q_OBJECT
  public:
    QWindow_hook(QObject *handle) : QObject_hook(handle) {
      screenChanged_event.func = NULL;;
      modalityChanged_event.func = NULL;;
      windowStateChanged_event.func = NULL;;
      windowTitleChanged_event.func = NULL;;
      xChanged_event.func = NULL;;
      yChanged_event.func = NULL;;
      widthChanged_event.func = NULL;;
      heightChanged_event.func = NULL;;
      minimumWidthChanged_event.func = NULL;;
      minimumHeightChanged_event.func = NULL;;
      maximumWidthChanged_event.func = NULL;;
      maximumHeightChanged_event.func = NULL;;
      visibleChanged_event.func = NULL;;
      visibilityChanged_event.func = NULL;;
      activeChanged_event.func = NULL;;
      contentOrientationChanged_event.func = NULL;;
      focusObjectChanged_event.func = NULL;;
      opacityChanged_event.func = NULL;;

    }

    void hook_screenChanged(QHook &hook) {
      if ( !screenChanged_event.func )
        connect(handle, SIGNAL(screenChanged(QScreen*)), this, SLOT(screenChanged_hook(QScreen*)));
      screenChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(screenChanged(QScreen*)), this, SLOT(screenChanged_hook(QScreen*)));
    }

    void hook_modalityChanged(QHook &hook) {
      if ( !modalityChanged_event.func )
        connect(handle, SIGNAL(modalityChanged(Qt::WindowModality)), this, SLOT(modalityChanged_hook(Qt::WindowModality)));
      modalityChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(modalityChanged(Qt::WindowModality)), this, SLOT(modalityChanged_hook(Qt::WindowModality)));
    }

    void hook_windowStateChanged(QHook &hook) {
      if ( !windowStateChanged_event.func )
        connect(handle, SIGNAL(windowStateChanged(Qt::WindowState)), this, SLOT(windowStateChanged_hook(Qt::WindowState)));
      windowStateChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(windowStateChanged(Qt::WindowState)), this, SLOT(windowStateChanged_hook(Qt::WindowState)));
    }

    void hook_windowTitleChanged(QHook &hook) {
      if ( !windowTitleChanged_event.func )
        connect(handle, SIGNAL(windowTitleChanged(const QString&)), this, SLOT(windowTitleChanged_hook(const QString&)));
      windowTitleChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(windowTitleChanged(const QString&)), this, SLOT(windowTitleChanged_hook(const QString&)));
    }

    void hook_xChanged(QHook &hook) {
      if ( !xChanged_event.func )
        connect(handle, SIGNAL(xChanged(int)), this, SLOT(xChanged_hook(int)));
      xChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(xChanged(int)), this, SLOT(xChanged_hook(int)));
    }

    void hook_yChanged(QHook &hook) {
      if ( !yChanged_event.func )
        connect(handle, SIGNAL(yChanged(int)), this, SLOT(yChanged_hook(int)));
      yChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(yChanged(int)), this, SLOT(yChanged_hook(int)));
    }

    void hook_widthChanged(QHook &hook) {
      if ( !widthChanged_event.func )
        connect(handle, SIGNAL(widthChanged(int)), this, SLOT(widthChanged_hook(int)));
      widthChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(widthChanged(int)), this, SLOT(widthChanged_hook(int)));
    }

    void hook_heightChanged(QHook &hook) {
      if ( !heightChanged_event.func )
        connect(handle, SIGNAL(heightChanged(int)), this, SLOT(heightChanged_hook(int)));
      heightChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(heightChanged(int)), this, SLOT(heightChanged_hook(int)));
    }

    void hook_minimumWidthChanged(QHook &hook) {
      if ( !minimumWidthChanged_event.func )
        connect(handle, SIGNAL(minimumWidthChanged(int)), this, SLOT(minimumWidthChanged_hook(int)));
      minimumWidthChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(minimumWidthChanged(int)), this, SLOT(minimumWidthChanged_hook(int)));
    }

    void hook_minimumHeightChanged(QHook &hook) {
      if ( !minimumHeightChanged_event.func )
        connect(handle, SIGNAL(minimumHeightChanged(int)), this, SLOT(minimumHeightChanged_hook(int)));
      minimumHeightChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(minimumHeightChanged(int)), this, SLOT(minimumHeightChanged_hook(int)));
    }

    void hook_maximumWidthChanged(QHook &hook) {
      if ( !maximumWidthChanged_event.func )
        connect(handle, SIGNAL(maximumWidthChanged(int)), this, SLOT(maximumWidthChanged_hook(int)));
      maximumWidthChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(maximumWidthChanged(int)), this, SLOT(maximumWidthChanged_hook(int)));
    }

    void hook_maximumHeightChanged(QHook &hook) {
      if ( !maximumHeightChanged_event.func )
        connect(handle, SIGNAL(maximumHeightChanged(int)), this, SLOT(maximumHeightChanged_hook(int)));
      maximumHeightChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(maximumHeightChanged(int)), this, SLOT(maximumHeightChanged_hook(int)));
    }

    void hook_visibleChanged(QHook &hook) {
      if ( !visibleChanged_event.func )
        connect(handle, SIGNAL(visibleChanged(bool)), this, SLOT(visibleChanged_hook(bool)));
      visibleChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(visibleChanged(bool)), this, SLOT(visibleChanged_hook(bool)));
    }

    void hook_visibilityChanged(QHook &hook) {
      if ( !visibilityChanged_event.func )
        connect(handle, SIGNAL(visibilityChanged(QWindow::Visibility)), this, SLOT(visibilityChanged_hook(QWindow::Visibility)));
      visibilityChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(visibilityChanged(QWindow::Visibility)), this, SLOT(visibilityChanged_hook(QWindow::Visibility)));
    }

    void hook_activeChanged(QHook &hook) {
      if ( !activeChanged_event.func )
        connect(handle, SIGNAL(activeChanged()), this, SLOT(activeChanged_hook()));
      activeChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(activeChanged()), this, SLOT(activeChanged_hook()));
    }

    void hook_contentOrientationChanged(QHook &hook) {
      if ( !contentOrientationChanged_event.func )
        connect(handle, SIGNAL(contentOrientationChanged(Qt::ScreenOrientation)), this, SLOT(contentOrientationChanged_hook(Qt::ScreenOrientation)));
      contentOrientationChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(contentOrientationChanged(Qt::ScreenOrientation)), this, SLOT(contentOrientationChanged_hook(Qt::ScreenOrientation)));
    }

    void hook_focusObjectChanged(QHook &hook) {
      if ( !focusObjectChanged_event.func )
        connect(handle, SIGNAL(focusObjectChanged(QObject*)), this, SLOT(focusObjectChanged_hook(QObject*)));
      focusObjectChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(focusObjectChanged(QObject*)), this, SLOT(focusObjectChanged_hook(QObject*)));
    }

    void hook_opacityChanged(QHook &hook) {
      if ( !opacityChanged_event.func )
        connect(handle, SIGNAL(opacityChanged(qreal)), this, SLOT(opacityChanged_hook(qreal)));
      opacityChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(opacityChanged(qreal)), this, SLOT(opacityChanged_hook(qreal)));
    }

  private slots:
    void screenChanged_hook(QScreen* screen) {
      if ( screenChanged_event.func ) {
        typedef void (*func_type)(void *data, QScreenH screen);
	(*(func_type)screenChanged_event.func)(screenChanged_event.data, (QScreenH)screen);
      }
    }

    void modalityChanged_hook(Qt::WindowModality modality) {
      if ( modalityChanged_event.func ) {
        typedef void (*func_type)(void *data, Qt::WindowModality modality);
	(*(func_type)modalityChanged_event.func)(modalityChanged_event.data, modality);
      }
    }

    void windowStateChanged_hook(Qt::WindowState windowState) {
      if ( windowStateChanged_event.func ) {
        typedef void (*func_type)(void *data, Qt::WindowState windowState);
	(*(func_type)windowStateChanged_event.func)(windowStateChanged_event.data, windowState);
      }
    }

    void windowTitleChanged_hook(const QString& title) {
      if ( windowTitleChanged_event.func ) {
        typedef void (*func_type)(void *data, PWideString title);
	PWideString t_title;
	initializePWideString(t_title);
	copyQStringToPWideString(title, t_title);
	(*(func_type)windowTitleChanged_event.func)(windowTitleChanged_event.data, t_title);
	finalizePWideString(t_title);
      }
    }

    void xChanged_hook(int arg) {
      if ( xChanged_event.func ) {
        typedef void (*func_type)(void *data, int arg);
	(*(func_type)xChanged_event.func)(xChanged_event.data, arg);
      }
    }

    void yChanged_hook(int arg) {
      if ( yChanged_event.func ) {
        typedef void (*func_type)(void *data, int arg);
	(*(func_type)yChanged_event.func)(yChanged_event.data, arg);
      }
    }

    void widthChanged_hook(int arg) {
      if ( widthChanged_event.func ) {
        typedef void (*func_type)(void *data, int arg);
	(*(func_type)widthChanged_event.func)(widthChanged_event.data, arg);
      }
    }

    void heightChanged_hook(int arg) {
      if ( heightChanged_event.func ) {
        typedef void (*func_type)(void *data, int arg);
	(*(func_type)heightChanged_event.func)(heightChanged_event.data, arg);
      }
    }

    void minimumWidthChanged_hook(int arg) {
      if ( minimumWidthChanged_event.func ) {
        typedef void (*func_type)(void *data, int arg);
	(*(func_type)minimumWidthChanged_event.func)(minimumWidthChanged_event.data, arg);
      }
    }

    void minimumHeightChanged_hook(int arg) {
      if ( minimumHeightChanged_event.func ) {
        typedef void (*func_type)(void *data, int arg);
	(*(func_type)minimumHeightChanged_event.func)(minimumHeightChanged_event.data, arg);
      }
    }

    void maximumWidthChanged_hook(int arg) {
      if ( maximumWidthChanged_event.func ) {
        typedef void (*func_type)(void *data, int arg);
	(*(func_type)maximumWidthChanged_event.func)(maximumWidthChanged_event.data, arg);
      }
    }

    void maximumHeightChanged_hook(int arg) {
      if ( maximumHeightChanged_event.func ) {
        typedef void (*func_type)(void *data, int arg);
	(*(func_type)maximumHeightChanged_event.func)(maximumHeightChanged_event.data, arg);
      }
    }

    void visibleChanged_hook(bool arg) {
      if ( visibleChanged_event.func ) {
        typedef void (*func_type)(void *data, bool arg);
	(*(func_type)visibleChanged_event.func)(visibleChanged_event.data, arg);
      }
    }

    void visibilityChanged_hook(QWindow::Visibility visibility) {
      if ( visibilityChanged_event.func ) {
        typedef void (*func_type)(void *data, QWindow::Visibility visibility);
	(*(func_type)visibilityChanged_event.func)(visibilityChanged_event.data, visibility);
      }
    }

    void activeChanged_hook() {
      if ( activeChanged_event.func ) {
        typedef void (*func_type)(void *data);
	(*(func_type)activeChanged_event.func)(activeChanged_event.data);
      }
    }

    void contentOrientationChanged_hook(Qt::ScreenOrientation orientation) {
      if ( contentOrientationChanged_event.func ) {
        typedef void (*func_type)(void *data, Qt::ScreenOrientation orientation);
	(*(func_type)contentOrientationChanged_event.func)(contentOrientationChanged_event.data, orientation);
      }
    }

    void focusObjectChanged_hook(QObject* object) {
      if ( focusObjectChanged_event.func ) {
        typedef void (*func_type)(void *data, QObjectH object);
	(*(func_type)focusObjectChanged_event.func)(focusObjectChanged_event.data, (QObjectH)object);
      }
    }

    void opacityChanged_hook(qreal opacity) {
      if ( opacityChanged_event.func ) {
        typedef void (*func_type)(void *data, qreal opacity);
	(*(func_type)opacityChanged_event.func)(opacityChanged_event.data, opacity);
      }
    }

  private:
    QHook screenChanged_event;
    QHook modalityChanged_event;
    QHook windowStateChanged_event;
    QHook windowTitleChanged_event;
    QHook xChanged_event;
    QHook yChanged_event;
    QHook widthChanged_event;
    QHook heightChanged_event;
    QHook minimumWidthChanged_event;
    QHook minimumHeightChanged_event;
    QHook maximumWidthChanged_event;
    QHook maximumHeightChanged_event;
    QHook visibleChanged_event;
    QHook visibilityChanged_event;
    QHook activeChanged_event;
    QHook contentOrientationChanged_event;
    QHook focusObjectChanged_event;
    QHook opacityChanged_event;
};


#endif
