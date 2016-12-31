//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qtoolbutton_c.h"

QToolButtonH QToolButton_Create(QWidgetH parent)
{
	return (QToolButtonH) new QToolButton((QWidget*)parent);
}

void QToolButton_Destroy(QToolButtonH handle)
{
	delete (QToolButton *)handle;
}

void QToolButton_sizeHint(QToolButtonH handle, PSize retval)
{
	*(QSize *)retval = ((QToolButton *)handle)->sizeHint();
}

void QToolButton_minimumSizeHint(QToolButtonH handle, PSize retval)
{
	*(QSize *)retval = ((QToolButton *)handle)->minimumSizeHint();
}

Qt::ToolButtonStyle QToolButton_toolButtonStyle(QToolButtonH handle)
{
	return (Qt::ToolButtonStyle) ((QToolButton *)handle)->toolButtonStyle();
}

Qt::ArrowType QToolButton_arrowType(QToolButtonH handle)
{
	return (Qt::ArrowType) ((QToolButton *)handle)->arrowType();
}

void QToolButton_setArrowType(QToolButtonH handle, Qt::ArrowType type)
{
	((QToolButton *)handle)->setArrowType(type);
}

void QToolButton_setMenu(QToolButtonH handle, QMenuH menu)
{
	((QToolButton *)handle)->setMenu((QMenu*)menu);
}

QMenuH QToolButton_menu(QToolButtonH handle)
{
	return (QMenuH) ((QToolButton *)handle)->menu();
}

void QToolButton_setPopupMode(QToolButtonH handle, QToolButton::ToolButtonPopupMode mode)
{
	((QToolButton *)handle)->setPopupMode(mode);
}

QToolButton::ToolButtonPopupMode QToolButton_popupMode(QToolButtonH handle)
{
	return (QToolButton::ToolButtonPopupMode) ((QToolButton *)handle)->popupMode();
}

QActionH QToolButton_defaultAction(QToolButtonH handle)
{
	return (QActionH) ((QToolButton *)handle)->defaultAction();
}

void QToolButton_setAutoRaise(QToolButtonH handle, bool enable)
{
	((QToolButton *)handle)->setAutoRaise(enable);
}

bool QToolButton_autoRaise(QToolButtonH handle)
{
	return (bool) ((QToolButton *)handle)->autoRaise();
}

void QToolButton_showMenu(QToolButtonH handle)
{
	((QToolButton *)handle)->showMenu();
}

void QToolButton_setToolButtonStyle(QToolButtonH handle, Qt::ToolButtonStyle style)
{
	((QToolButton *)handle)->setToolButtonStyle(style);
}

void QToolButton_setDefaultAction(QToolButtonH handle, QActionH AnonParam1)
{
	((QToolButton *)handle)->setDefaultAction((QAction*)AnonParam1);
}

