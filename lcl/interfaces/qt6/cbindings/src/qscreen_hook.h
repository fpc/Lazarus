//******************************************************************************
//  Copyright (c) 2017-2022 by Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QSCREEN_HOOK_H
#define QSCREEN_HOOK_H

#include <qscreen.h>

#include "qobject_hook.h"

class QScreen_hook : public QObject_hook {
  Q_OBJECT
  public:
    QScreen_hook(QObject *handle) : QObject_hook(handle) {
      geometryChanged_event.func = NULL;
      availableGeometryChanged_event.func = NULL;
      physicalSizeChanged_event.func = NULL;
      physicalDotsPerInchChanged_event.func = NULL;
      logicalDotsPerInchChanged_event.func = NULL;
      virtualGeometryChanged_event.func = NULL;
      primaryOrientationChanged_event.func = NULL;
      orientationChanged_event.func = NULL;
      refreshRateChanged_event.func = NULL;

    }

    void hook_geometryChanged(QHook &hook) {
      if ( !geometryChanged_event.func )
        connect(handle, SIGNAL(geometryChanged(const QRect&)), this, SLOT(geometryChanged_hook(const QRect&)));
      geometryChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(geometryChanged(const QRect&)), this, SLOT(geometryChanged_hook(const QRect&)));
    }

    void hook_availableGeometryChanged(QHook &hook) {
      if ( !availableGeometryChanged_event.func )
        connect(handle, SIGNAL(availableGeometryChanged(const QRect&)), this, SLOT(availableGeometryChanged_hook(const QRect&)));
      availableGeometryChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(availableGeometryChanged(const QRect&)), this, SLOT(availableGeometryChanged_hook(const QRect&)));
    }

    void hook_physicalSizeChanged(QHook &hook) {
      if ( !physicalSizeChanged_event.func )
        connect(handle, SIGNAL(physicalSizeChanged(const QSizeF&)), this, SLOT(physicalSizeChanged_hook(const QSizeF&)));
      physicalSizeChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(physicalSizeChanged(const QSizeF&)), this, SLOT(physicalSizeChanged_hook(const QSizeF&)));
    }

    void hook_physicalDotsPerInchChanged(QHook &hook) {
      if ( !physicalDotsPerInchChanged_event.func )
        connect(handle, SIGNAL(physicalDotsPerInchChanged(qreal)), this, SLOT(physicalDotsPerInchChanged_hook(qreal)));
      physicalDotsPerInchChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(physicalDotsPerInchChanged(qreal)), this, SLOT(physicalDotsPerInchChanged_hook(qreal)));
    }

    void hook_logicalDotsPerInchChanged(QHook &hook) {
      if ( !logicalDotsPerInchChanged_event.func )
        connect(handle, SIGNAL(logicalDotsPerInchChanged(qreal)), this, SLOT(logicalDotsPerInchChanged_hook(qreal)));
      logicalDotsPerInchChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(logicalDotsPerInchChanged(qreal)), this, SLOT(logicalDotsPerInchChanged_hook(qreal)));
    }

    void hook_virtualGeometryChanged(QHook &hook) {
      if ( !virtualGeometryChanged_event.func )
        connect(handle, SIGNAL(virtualGeometryChanged(const QRect&)), this, SLOT(virtualGeometryChanged_hook(const QRect&)));
      virtualGeometryChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(virtualGeometryChanged(const QRect&)), this, SLOT(virtualGeometryChanged_hook(const QRect&)));
    }

    void hook_primaryOrientationChanged(QHook &hook) {
      if ( !primaryOrientationChanged_event.func )
        connect(handle, SIGNAL(primaryOrientationChanged(Qt::ScreenOrientation)), this, SLOT(primaryOrientationChanged_hook(Qt::ScreenOrientation)));
      primaryOrientationChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(primaryOrientationChanged(Qt::ScreenOrientation)), this, SLOT(primaryOrientationChanged_hook(Qt::ScreenOrientation)));
    }

    void hook_orientationChanged(QHook &hook) {
      if ( !orientationChanged_event.func )
        connect(handle, SIGNAL(orientationChanged(Qt::ScreenOrientation)), this, SLOT(orientationChanged_hook(Qt::ScreenOrientation)));
      orientationChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(orientationChanged(Qt::ScreenOrientation)), this, SLOT(orientationChanged_hook(Qt::ScreenOrientation)));
    }

    void hook_refreshRateChanged(QHook &hook) {
      if ( !refreshRateChanged_event.func )
        connect(handle, SIGNAL(refreshRateChanged(qreal)), this, SLOT(refreshRateChanged_hook(qreal)));
      refreshRateChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(refreshRateChanged(qreal)), this, SLOT(refreshRateChanged_hook(qreal)));
    }

  private slots:
    void geometryChanged_hook(const QRect& geom) {
      if ( geometryChanged_event.func ) {
        typedef void (*func_type)(void *data, PRect geom);
	PRect t_geom;
	copyQRectToPRect(geom, t_geom);
	(*(func_type)geometryChanged_event.func)(geometryChanged_event.data, t_geom);
      }
    }

    void availableGeometryChanged_hook(const QRect& geom) {
      if ( availableGeometryChanged_event.func ) {
        typedef void (*func_type)(void *data, PRect geom);
	PRect t_geom;
	copyQRectToPRect(geom, t_geom);
	(*(func_type)availableGeometryChanged_event.func)(availableGeometryChanged_event.data, t_geom);
      }
    }

    void physicalSizeChanged_hook(const QSizeF& pos) {
      if ( physicalSizeChanged_event.func ) {
        typedef void (*func_type)(void *data, const QSizeFH pos);
	(*(func_type)physicalSizeChanged_event.func)(physicalSizeChanged_event.data, (const QSizeFH)&pos);
      }
    }

    void physicalDotsPerInchChanged_hook(qreal dpi) {
      if ( physicalDotsPerInchChanged_event.func ) {
        typedef void (*func_type)(void *data, qreal dpi);
	(*(func_type)physicalDotsPerInchChanged_event.func)(physicalDotsPerInchChanged_event.data, dpi);
      }
    }

    void logicalDotsPerInchChanged_hook(qreal dpi) {
      if ( logicalDotsPerInchChanged_event.func ) {
        typedef void (*func_type)(void *data, qreal dpi);
	(*(func_type)logicalDotsPerInchChanged_event.func)(logicalDotsPerInchChanged_event.data, dpi);
      }
    }

    void virtualGeometryChanged_hook(const QRect& geom) {
      if ( virtualGeometryChanged_event.func ) {
        typedef void (*func_type)(void *data, PRect geom);
	PRect t_geom;
	copyQRectToPRect(geom, t_geom);
	(*(func_type)virtualGeometryChanged_event.func)(virtualGeometryChanged_event.data, t_geom);
      }
    }

    void primaryOrientationChanged_hook(Qt::ScreenOrientation orientation) {
      if ( primaryOrientationChanged_event.func ) {
        typedef void (*func_type)(void *data, Qt::ScreenOrientation orientation);
	(*(func_type)primaryOrientationChanged_event.func)(primaryOrientationChanged_event.data, orientation);
      }
    }

    void orientationChanged_hook(Qt::ScreenOrientation orientation) {
      if ( orientationChanged_event.func ) {
        typedef void (*func_type)(void *data, Qt::ScreenOrientation orientation);
	(*(func_type)orientationChanged_event.func)(orientationChanged_event.data, orientation);
      }
    }

    void refreshRateChanged_hook(qreal refreshRate) {
      if ( refreshRateChanged_event.func ) {
        typedef void (*func_type)(void *data, qreal refreshRate);
	(*(func_type)refreshRateChanged_event.func)(refreshRateChanged_event.data, refreshRate);
      }
    }

  private:
    QHook geometryChanged_event;
    QHook availableGeometryChanged_event;
    QHook physicalSizeChanged_event;
    QHook physicalDotsPerInchChanged_event;
    QHook logicalDotsPerInchChanged_event;
    QHook virtualGeometryChanged_event;
    QHook primaryOrientationChanged_event;
    QHook orientationChanged_event;
    QHook refreshRateChanged_event;
};

#endif

