//******************************************************************************
//  Copyright (c) 2020 Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qlclopenglwidget_c.h"

QLCLOpenGLWidgetH QLCLOpenGLWidget_Create(QWidgetH parent, unsigned int f)
{
	return (QLCLOpenGLWidgetH) new QLCLOpenGLWidget((QWidget*)parent, (Qt::WindowFlags)f);
}

void QLCLOpenGLWidget_Destroy(QLCLOpenGLWidgetH handle)
{
	delete (QLCLOpenGLWidget *)handle;
}

void QLCLOpenGLWidget_override_paintGL(QLCLOpenGLWidgetH handle, const QOverrideHook hook)
{
	((QLCLOpenGLWidget *)handle)->override_paintGL(hook);
}

void QLCLOpenGLWidget_InheritedPaintGL(QLCLOpenGLWidgetH handle)
{
	((QLCLOpenGLWidget *)handle)->InheritedPaintGLOverride();
}


