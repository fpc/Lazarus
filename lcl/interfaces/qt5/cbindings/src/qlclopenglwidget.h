//******************************************************************************
//  Copyright (c) 2020 by Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************

#ifndef QLCLOPENGLWIDGET_H
#define QLCLOPENGLWIDGET_H

#include <QWidget>
#include "pascalbind.h"

class QLCLOpenGLWidget : public QWidget {

public:

  //====================================================================================
  QLCLOpenGLWidget(QWidget * parent = 0, Qt::WindowFlags flags = Qt::Widget) : QWidget (parent, (Qt::WindowFlags)flags) {
    paintGLOverride.func = NULL;
    setAttribute(Qt::WA_NativeWindow);
    setAttribute(Qt::WA_PaintOnScreen);
    setAttribute(Qt::WA_NoSystemBackground);
  };

  virtual QPaintEngine* paintEngine() const Q_DECL_OVERRIDE
  {
    return nullptr;
  }


  void override_paintGL(const QOverrideHook hook) {
    paintGLOverride = hook;
  }

  void InheritedPaintGLOverride() {
    qInfo("empty inheritedPaintGLOverride called");
  };


private:

  //====================================================================================
  QOverrideHook paintGLOverride;
  //====================================================================================

  void paintGL() {


    if (paintGLOverride.func) {

      typedef void (*func_type)(void *data);
      (*(func_type)paintGLOverride.func)(paintGLOverride.data);
      }
   else InheritedPaintGLOverride();


  };


  void paintEvent(QPaintEvent *e) Q_DECL_OVERRIDE {
    if (paintGLOverride.func) {
      Q_UNUSED(e);
      if (!isVisible())
        return;
      if (updatesEnabled()) {
        paintGL();
      }

    }
    else
      QWidget::paintEvent(e);
  };



};

#endif
