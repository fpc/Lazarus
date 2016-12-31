//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QTOOLBAR_C_H
#define QTOOLBAR_C_H

#include <QtWidgets>
#include "pascalbind.h"

C_EXPORT QToolBarH QToolBar_Create(PWideString title, QWidgetH parent);
C_EXPORT void QToolBar_Destroy(QToolBarH handle);
C_EXPORT QToolBarH QToolBar_Create2(QWidgetH parent);
C_EXPORT void QToolBar_setMovable(QToolBarH handle, bool movable);
C_EXPORT bool QToolBar_isMovable(QToolBarH handle);
C_EXPORT void QToolBar_setAllowedAreas(QToolBarH handle, unsigned int areas);
C_EXPORT unsigned int QToolBar_allowedAreas(QToolBarH handle);
C_EXPORT bool QToolBar_isAreaAllowed(QToolBarH handle, Qt::ToolBarArea area);
C_EXPORT void QToolBar_setOrientation(QToolBarH handle, Qt::Orientation orientation);
C_EXPORT Qt::Orientation QToolBar_orientation(QToolBarH handle);
C_EXPORT void QToolBar_clear(QToolBarH handle);
C_EXPORT QActionH QToolBar_addAction(QToolBarH handle, PWideString text);
C_EXPORT QActionH QToolBar_addAction2(QToolBarH handle, const QIconH icon, PWideString text);
C_EXPORT QActionH QToolBar_addAction3(QToolBarH handle, PWideString text, const QObjectH receiver, const char* member);
C_EXPORT QActionH QToolBar_addAction4(QToolBarH handle, const QIconH icon, PWideString text, const QObjectH receiver, const char* member);
C_EXPORT QActionH QToolBar_addSeparator(QToolBarH handle);
C_EXPORT QActionH QToolBar_insertSeparator(QToolBarH handle, QActionH before);
C_EXPORT QActionH QToolBar_addWidget(QToolBarH handle, QWidgetH widget);
C_EXPORT QActionH QToolBar_insertWidget(QToolBarH handle, QActionH before, QWidgetH widget);
C_EXPORT void QToolBar_actionGeometry(QToolBarH handle, PRect retval, QActionH action);
C_EXPORT QActionH QToolBar_actionAt(QToolBarH handle, const QPointH p);
C_EXPORT QActionH QToolBar_actionAt2(QToolBarH handle, int x, int y);
C_EXPORT QActionH QToolBar_toggleViewAction(QToolBarH handle);
C_EXPORT void QToolBar_iconSize(QToolBarH handle, PSize retval);
C_EXPORT Qt::ToolButtonStyle QToolBar_toolButtonStyle(QToolBarH handle);
C_EXPORT QWidgetH QToolBar_widgetForAction(QToolBarH handle, QActionH action);
C_EXPORT bool QToolBar_isFloatable(QToolBarH handle);
C_EXPORT void QToolBar_setFloatable(QToolBarH handle, bool floatable);
C_EXPORT bool QToolBar_isFloating(QToolBarH handle);
C_EXPORT void QToolBar_setIconSize(QToolBarH handle, const QSizeH iconSize);
C_EXPORT void QToolBar_setToolButtonStyle(QToolBarH handle, Qt::ToolButtonStyle toolButtonStyle);

#endif
