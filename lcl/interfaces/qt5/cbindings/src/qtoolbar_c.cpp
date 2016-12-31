//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qtoolbar_c.h"

QToolBarH QToolBar_Create(PWideString title, QWidgetH parent)
{
	QString t_title;
	copyPWideStringToQString(title, t_title);
	return (QToolBarH) new QToolBar(t_title, (QWidget*)parent);
}

void QToolBar_Destroy(QToolBarH handle)
{
	delete (QToolBar *)handle;
}

QToolBarH QToolBar_Create2(QWidgetH parent)
{
	return (QToolBarH) new QToolBar((QWidget*)parent);
}

void QToolBar_setMovable(QToolBarH handle, bool movable)
{
	((QToolBar *)handle)->setMovable(movable);
}

bool QToolBar_isMovable(QToolBarH handle)
{
	return (bool) ((QToolBar *)handle)->isMovable();
}

void QToolBar_setAllowedAreas(QToolBarH handle, unsigned int areas)
{
	((QToolBar *)handle)->setAllowedAreas((Qt::ToolBarAreas)areas);
}

unsigned int QToolBar_allowedAreas(QToolBarH handle)
{
	return (unsigned int) ((QToolBar *)handle)->allowedAreas();
}

bool QToolBar_isAreaAllowed(QToolBarH handle, Qt::ToolBarArea area)
{
	return (bool) ((QToolBar *)handle)->isAreaAllowed(area);
}

void QToolBar_setOrientation(QToolBarH handle, Qt::Orientation orientation)
{
	((QToolBar *)handle)->setOrientation(orientation);
}

Qt::Orientation QToolBar_orientation(QToolBarH handle)
{
	return (Qt::Orientation) ((QToolBar *)handle)->orientation();
}

void QToolBar_clear(QToolBarH handle)
{
	((QToolBar *)handle)->clear();
}

QActionH QToolBar_addAction(QToolBarH handle, PWideString text)
{
	QString t_text;
	copyPWideStringToQString(text, t_text);
	return (QActionH) ((QToolBar *)handle)->addAction(t_text);
}

QActionH QToolBar_addAction2(QToolBarH handle, const QIconH icon, PWideString text)
{
	QString t_text;
	copyPWideStringToQString(text, t_text);
	return (QActionH) ((QToolBar *)handle)->addAction(*(const QIcon*)icon, t_text);
}

QActionH QToolBar_addAction3(QToolBarH handle, PWideString text, const QObjectH receiver, const char* member)
{
	QString t_text;
	copyPWideStringToQString(text, t_text);
	return (QActionH) ((QToolBar *)handle)->addAction(t_text, (const QObject*)receiver, member);
}

QActionH QToolBar_addAction4(QToolBarH handle, const QIconH icon, PWideString text, const QObjectH receiver, const char* member)
{
	QString t_text;
	copyPWideStringToQString(text, t_text);
	return (QActionH) ((QToolBar *)handle)->addAction(*(const QIcon*)icon, t_text, (const QObject*)receiver, member);
}

QActionH QToolBar_addSeparator(QToolBarH handle)
{
	return (QActionH) ((QToolBar *)handle)->addSeparator();
}

QActionH QToolBar_insertSeparator(QToolBarH handle, QActionH before)
{
	return (QActionH) ((QToolBar *)handle)->insertSeparator((QAction*)before);
}

QActionH QToolBar_addWidget(QToolBarH handle, QWidgetH widget)
{
	return (QActionH) ((QToolBar *)handle)->addWidget((QWidget*)widget);
}

QActionH QToolBar_insertWidget(QToolBarH handle, QActionH before, QWidgetH widget)
{
	return (QActionH) ((QToolBar *)handle)->insertWidget((QAction*)before, (QWidget*)widget);
}

void QToolBar_actionGeometry(QToolBarH handle, PRect retval, QActionH action)
{
	QRect t_retval;
	t_retval = ((QToolBar *)handle)->actionGeometry((QAction*)action);
	copyQRectToPRect(t_retval, retval);
}

QActionH QToolBar_actionAt(QToolBarH handle, const QPointH p)
{
	return (QActionH) ((QToolBar *)handle)->actionAt(*(const QPoint*)p);
}

QActionH QToolBar_actionAt2(QToolBarH handle, int x, int y)
{
	return (QActionH) ((QToolBar *)handle)->actionAt(x, y);
}

QActionH QToolBar_toggleViewAction(QToolBarH handle)
{
	return (QActionH) ((QToolBar *)handle)->toggleViewAction();
}

void QToolBar_iconSize(QToolBarH handle, PSize retval)
{
	*(QSize *)retval = ((QToolBar *)handle)->iconSize();
}

Qt::ToolButtonStyle QToolBar_toolButtonStyle(QToolBarH handle)
{
	return (Qt::ToolButtonStyle) ((QToolBar *)handle)->toolButtonStyle();
}

QWidgetH QToolBar_widgetForAction(QToolBarH handle, QActionH action)
{
	return (QWidgetH) ((QToolBar *)handle)->widgetForAction((QAction*)action);
}

bool QToolBar_isFloatable(QToolBarH handle)
{
	return (bool) ((QToolBar *)handle)->isFloatable();
}

void QToolBar_setFloatable(QToolBarH handle, bool floatable)
{
	((QToolBar *)handle)->setFloatable(floatable);
}

bool QToolBar_isFloating(QToolBarH handle)
{
	return (bool) ((QToolBar *)handle)->isFloating();
}

void QToolBar_setIconSize(QToolBarH handle, const QSizeH iconSize)
{
	((QToolBar *)handle)->setIconSize(*(const QSize*)iconSize);
}

void QToolBar_setToolButtonStyle(QToolBarH handle, Qt::ToolButtonStyle toolButtonStyle)
{
	((QToolBar *)handle)->setToolButtonStyle(toolButtonStyle);
}

