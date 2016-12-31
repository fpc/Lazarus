//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QTOOLBUTTON_C_H
#define QTOOLBUTTON_C_H

#include <QtWidgets>
#include "pascalbind.h"

C_EXPORT QToolButtonH QToolButton_Create(QWidgetH parent);
C_EXPORT void QToolButton_Destroy(QToolButtonH handle);
C_EXPORT void QToolButton_sizeHint(QToolButtonH handle, PSize retval);
C_EXPORT void QToolButton_minimumSizeHint(QToolButtonH handle, PSize retval);
C_EXPORT Qt::ToolButtonStyle QToolButton_toolButtonStyle(QToolButtonH handle);
C_EXPORT Qt::ArrowType QToolButton_arrowType(QToolButtonH handle);
C_EXPORT void QToolButton_setArrowType(QToolButtonH handle, Qt::ArrowType type);
C_EXPORT void QToolButton_setMenu(QToolButtonH handle, QMenuH menu);
C_EXPORT QMenuH QToolButton_menu(QToolButtonH handle);
C_EXPORT void QToolButton_setPopupMode(QToolButtonH handle, QToolButton::ToolButtonPopupMode mode);
C_EXPORT QToolButton::ToolButtonPopupMode QToolButton_popupMode(QToolButtonH handle);
C_EXPORT QActionH QToolButton_defaultAction(QToolButtonH handle);
C_EXPORT void QToolButton_setAutoRaise(QToolButtonH handle, bool enable);
C_EXPORT bool QToolButton_autoRaise(QToolButtonH handle);
C_EXPORT void QToolButton_showMenu(QToolButtonH handle);
C_EXPORT void QToolButton_setToolButtonStyle(QToolButtonH handle, Qt::ToolButtonStyle style);
C_EXPORT void QToolButton_setDefaultAction(QToolButtonH handle, QActionH AnonParam1);

#endif
