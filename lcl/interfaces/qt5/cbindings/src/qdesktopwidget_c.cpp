//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qdesktopwidget_c.h"

QDesktopWidgetH QDesktopWidget_Create()
{
	return (QDesktopWidgetH) new QDesktopWidget();
}

void QDesktopWidget_Destroy(QDesktopWidgetH handle)
{
	delete (QDesktopWidget *)handle;
}

bool QDesktopWidget_isVirtualDesktop(QDesktopWidgetH handle)
{
	return (bool) ((QDesktopWidget *)handle)->isVirtualDesktop();
}

int QDesktopWidget_numScreens(QDesktopWidgetH handle)
{
	return (int) ((QDesktopWidget *)handle)->numScreens();
}

int QDesktopWidget_screenCount(QDesktopWidgetH handle)
{
	return (int) ((QDesktopWidget *)handle)->screenCount();
}

int QDesktopWidget_primaryScreen(QDesktopWidgetH handle)
{
	return (int) ((QDesktopWidget *)handle)->primaryScreen();
}

int QDesktopWidget_screenNumber(QDesktopWidgetH handle, const QWidgetH widget)
{
	return (int) ((QDesktopWidget *)handle)->screenNumber((const QWidget*)widget);
}

int QDesktopWidget_screenNumber2(QDesktopWidgetH handle, const QPointH AnonParam1)
{
	return (int) ((QDesktopWidget *)handle)->screenNumber(*(const QPoint*)AnonParam1);
}

QWidgetH QDesktopWidget_screen(QDesktopWidgetH handle, int screen)
{
	return (QWidgetH) ((QDesktopWidget *)handle)->screen(screen);
}

void QDesktopWidget_screenGeometry(QDesktopWidgetH handle, PRect retval, int screen)
{
	QRect t_retval;
	t_retval = ((QDesktopWidget *)handle)->screenGeometry(screen);
	copyQRectToPRect(t_retval, retval);
}

void QDesktopWidget_screenGeometry2(QDesktopWidgetH handle, PRect retval, const QWidgetH widget)
{
	QRect t_retval;
	t_retval = ((QDesktopWidget *)handle)->screenGeometry((const QWidget*)widget);
	copyQRectToPRect(t_retval, retval);
}

void QDesktopWidget_screenGeometry3(QDesktopWidgetH handle, PRect retval, const QPointH point)
{
	QRect t_retval;
	t_retval = ((QDesktopWidget *)handle)->screenGeometry(*(const QPoint*)point);
	copyQRectToPRect(t_retval, retval);
}

void QDesktopWidget_availableGeometry(QDesktopWidgetH handle, PRect retval, int screen)
{
	QRect t_retval;
	t_retval = ((QDesktopWidget *)handle)->availableGeometry(screen);
	copyQRectToPRect(t_retval, retval);
}

void QDesktopWidget_availableGeometry2(QDesktopWidgetH handle, PRect retval, const QWidgetH widget)
{
	QRect t_retval;
	t_retval = ((QDesktopWidget *)handle)->availableGeometry((const QWidget*)widget);
	copyQRectToPRect(t_retval, retval);
}

void QDesktopWidget_availableGeometry3(QDesktopWidgetH handle, PRect retval, const QPointH point)
{
	QRect t_retval;
	t_retval = ((QDesktopWidget *)handle)->availableGeometry(*(const QPoint*)point);
	copyQRectToPRect(t_retval, retval);
}

