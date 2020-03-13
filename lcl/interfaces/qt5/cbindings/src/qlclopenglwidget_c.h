//******************************************************************************
//  Copyright (c) 2020 by Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QLCLOPENGLWIDGET_C_H
#define QLCLOPENGLWIDGET_C_H

#include "qlclopenglwidget.h"
#include "pascalbind.h"

C_EXPORT QLCLOpenGLWidgetH QLCLOpenGLWidget_Create(QWidgetH parent, unsigned int f);
C_EXPORT void QLCLOpenGLWidget_Destroy(QLCLOpenGLWidgetH handle);
C_EXPORT void QLCLOpenGLWidget_override_paintGL(QLCLOpenGLWidgetH handle, const QOverrideHook hook);
C_EXPORT void QLCLOpenGLWidget_InheritedPaintGL(QLCLOpenGLWidgetH handle);

#endif
