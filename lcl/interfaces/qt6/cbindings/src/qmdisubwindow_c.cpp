//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qmdisubwindow_c.h"

QMdiSubWindowH QMdiSubWindow_Create(QWidgetH parent, unsigned int flags)
{
	return (QMdiSubWindowH) new QMdiSubWindow((QWidget*)parent, (Qt::WindowFlags)flags);
}

void QMdiSubWindow_Destroy(QMdiSubWindowH handle)
{
	delete (QMdiSubWindow *)handle;
}

void QMdiSubWindow_sizeHint(QMdiSubWindowH handle, PSize retval)
{
	*(QSize *)retval = ((QMdiSubWindow *)handle)->sizeHint();
}

void QMdiSubWindow_minimumSizeHint(QMdiSubWindowH handle, PSize retval)
{
	*(QSize *)retval = ((QMdiSubWindow *)handle)->minimumSizeHint();
}

void QMdiSubWindow_setWidget(QMdiSubWindowH handle, QWidgetH widget)
{
	((QMdiSubWindow *)handle)->setWidget((QWidget*)widget);
}

QWidgetH QMdiSubWindow_widget(QMdiSubWindowH handle)
{
	return (QWidgetH) ((QMdiSubWindow *)handle)->widget();
}

QWidgetH QMdiSubWindow_maximizedButtonsWidget(QMdiSubWindowH handle)
{
	return (QWidgetH) ((QMdiSubWindow *)handle)->maximizedButtonsWidget();
}

QWidgetH QMdiSubWindow_maximizedSystemMenuIconWidget(QMdiSubWindowH handle)
{
	return (QWidgetH) ((QMdiSubWindow *)handle)->maximizedSystemMenuIconWidget();
}

bool QMdiSubWindow_isShaded(QMdiSubWindowH handle)
{
	return (bool) ((QMdiSubWindow *)handle)->isShaded();
}

void QMdiSubWindow_setOption(QMdiSubWindowH handle, QMdiSubWindow::SubWindowOption option, bool on)
{
	((QMdiSubWindow *)handle)->setOption(option, on);
}

bool QMdiSubWindow_testOption(QMdiSubWindowH handle, QMdiSubWindow::SubWindowOption AnonParam1)
{
	return (bool) ((QMdiSubWindow *)handle)->testOption(AnonParam1);
}

void QMdiSubWindow_setKeyboardSingleStep(QMdiSubWindowH handle, int step)
{
	((QMdiSubWindow *)handle)->setKeyboardSingleStep(step);
}

int QMdiSubWindow_keyboardSingleStep(QMdiSubWindowH handle)
{
	return (int) ((QMdiSubWindow *)handle)->keyboardSingleStep();
}

void QMdiSubWindow_setKeyboardPageStep(QMdiSubWindowH handle, int step)
{
	((QMdiSubWindow *)handle)->setKeyboardPageStep(step);
}

int QMdiSubWindow_keyboardPageStep(QMdiSubWindowH handle)
{
	return (int) ((QMdiSubWindow *)handle)->keyboardPageStep();
}

void QMdiSubWindow_setSystemMenu(QMdiSubWindowH handle, QMenuH systemMenu)
{
	((QMdiSubWindow *)handle)->setSystemMenu((QMenu*)systemMenu);
}

QMenuH QMdiSubWindow_systemMenu(QMdiSubWindowH handle)
{
	return (QMenuH) ((QMdiSubWindow *)handle)->systemMenu();
}

QMdiAreaH QMdiSubWindow_mdiArea(QMdiSubWindowH handle)
{
	return (QMdiAreaH) ((QMdiSubWindow *)handle)->mdiArea();
}

void QMdiSubWindow_showSystemMenu(QMdiSubWindowH handle)
{
	((QMdiSubWindow *)handle)->showSystemMenu();
}

void QMdiSubWindow_showShaded(QMdiSubWindowH handle)
{
	((QMdiSubWindow *)handle)->showShaded();
}

