//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qdockwidget_c.h"

QDockWidgetH QDockWidget_Create(PWideString title, QWidgetH parent, unsigned int flags)
{
	QString t_title;
	copyPWideStringToQString(title, t_title);
	return (QDockWidgetH) new QDockWidget(t_title, (QWidget*)parent, (Qt::WindowFlags)flags);
}

void QDockWidget_Destroy(QDockWidgetH handle)
{
	delete (QDockWidget *)handle;
}

QDockWidgetH QDockWidget_Create2(QWidgetH parent, unsigned int flags)
{
	return (QDockWidgetH) new QDockWidget((QWidget*)parent, (Qt::WindowFlags)flags);
}

QWidgetH QDockWidget_widget(QDockWidgetH handle)
{
	return (QWidgetH) ((QDockWidget *)handle)->widget();
}

void QDockWidget_setWidget(QDockWidgetH handle, QWidgetH widget)
{
	((QDockWidget *)handle)->setWidget((QWidget*)widget);
}

void QDockWidget_setFeatures(QDockWidgetH handle, unsigned int features)
{
	((QDockWidget *)handle)->setFeatures((QDockWidget::DockWidgetFeatures)features);
}

unsigned int QDockWidget_features(QDockWidgetH handle)
{
	return (unsigned int) ((QDockWidget *)handle)->features();
}

void QDockWidget_setFloating(QDockWidgetH handle, bool floating)
{
	((QDockWidget *)handle)->setFloating(floating);
}

bool QDockWidget_isFloating(QDockWidgetH handle)
{
	return (bool) ((QDockWidget *)handle)->isFloating();
}

void QDockWidget_setAllowedAreas(QDockWidgetH handle, unsigned int areas)
{
	((QDockWidget *)handle)->setAllowedAreas((Qt::DockWidgetAreas)areas);
}

unsigned int QDockWidget_allowedAreas(QDockWidgetH handle)
{
	return (unsigned int) ((QDockWidget *)handle)->allowedAreas();
}

void QDockWidget_setTitleBarWidget(QDockWidgetH handle, QWidgetH widget)
{
	((QDockWidget *)handle)->setTitleBarWidget((QWidget*)widget);
}

QWidgetH QDockWidget_titleBarWidget(QDockWidgetH handle)
{
	return (QWidgetH) ((QDockWidget *)handle)->titleBarWidget();
}

bool QDockWidget_isAreaAllowed(QDockWidgetH handle, Qt::DockWidgetArea area)
{
	return (bool) ((QDockWidget *)handle)->isAreaAllowed(area);
}

QActionH QDockWidget_toggleViewAction(QDockWidgetH handle)
{
	return (QActionH) ((QDockWidget *)handle)->toggleViewAction();
}

