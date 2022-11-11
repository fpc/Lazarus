//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QTABBAR_C_H
#define QTABBAR_C_H

#include <QtWidgets>
#include "pascalbind.h"

C_EXPORT QTabBarH QTabBar_Create(QWidgetH parent);
C_EXPORT void QTabBar_Destroy(QTabBarH handle);
C_EXPORT QTabBar::Shape QTabBar_shape(QTabBarH handle);
C_EXPORT void QTabBar_setShape(QTabBarH handle, QTabBar::Shape shape);
C_EXPORT int QTabBar_addTab(QTabBarH handle, PWideString text);
C_EXPORT int QTabBar_addTab2(QTabBarH handle, const QIconH icon, PWideString text);
C_EXPORT int QTabBar_insertTab(QTabBarH handle, int index, PWideString text);
C_EXPORT int QTabBar_insertTab2(QTabBarH handle, int index, const QIconH icon, PWideString text);
C_EXPORT void QTabBar_removeTab(QTabBarH handle, int index);
C_EXPORT void QTabBar_moveTab(QTabBarH handle, int from, int to);
C_EXPORT bool QTabBar_isTabEnabled(QTabBarH handle, int index);
C_EXPORT void QTabBar_setTabEnabled(QTabBarH handle, int index, bool AnonParam2);
C_EXPORT void QTabBar_tabText(QTabBarH handle, PWideString retval, int index);
C_EXPORT void QTabBar_setTabText(QTabBarH handle, int index, PWideString text);
C_EXPORT void QTabBar_tabTextColor(QTabBarH handle, PQColor retval, int index);
C_EXPORT void QTabBar_setTabTextColor(QTabBarH handle, int index, const QColorH color);
C_EXPORT void QTabBar_tabIcon(QTabBarH handle, QIconH retval, int index);
C_EXPORT void QTabBar_setTabIcon(QTabBarH handle, int index, const QIconH icon);
C_EXPORT Qt::TextElideMode QTabBar_elideMode(QTabBarH handle);
C_EXPORT void QTabBar_setElideMode(QTabBarH handle, Qt::TextElideMode AnonParam1);
C_EXPORT void QTabBar_setTabToolTip(QTabBarH handle, int index, PWideString tip);
C_EXPORT void QTabBar_tabToolTip(QTabBarH handle, PWideString retval, int index);
C_EXPORT void QTabBar_setTabWhatsThis(QTabBarH handle, int index, PWideString text);
C_EXPORT void QTabBar_tabWhatsThis(QTabBarH handle, PWideString retval, int index);
C_EXPORT void QTabBar_setTabData(QTabBarH handle, int index, const QVariantH data);
C_EXPORT void QTabBar_tabData(QTabBarH handle, QVariantH retval, int index);
C_EXPORT void QTabBar_tabRect(QTabBarH handle, PRect retval, int index);
C_EXPORT int QTabBar_tabAt(QTabBarH handle, const QPointH pos);
C_EXPORT int QTabBar_currentIndex(QTabBarH handle);
C_EXPORT int QTabBar_count(QTabBarH handle);
C_EXPORT void QTabBar_sizeHint(QTabBarH handle, PSize retval);
C_EXPORT void QTabBar_minimumSizeHint(QTabBarH handle, PSize retval);
C_EXPORT void QTabBar_setDrawBase(QTabBarH handle, bool drawTheBase);
C_EXPORT bool QTabBar_drawBase(QTabBarH handle);
C_EXPORT void QTabBar_iconSize(QTabBarH handle, PSize retval);
C_EXPORT void QTabBar_setIconSize(QTabBarH handle, const QSizeH size);
C_EXPORT bool QTabBar_usesScrollButtons(QTabBarH handle);
C_EXPORT void QTabBar_setUsesScrollButtons(QTabBarH handle, bool useButtons);
C_EXPORT bool QTabBar_tabsClosable(QTabBarH handle);
C_EXPORT void QTabBar_setTabsClosable(QTabBarH handle, bool closable);
C_EXPORT void QTabBar_setTabButton(QTabBarH handle, int index, QTabBar::ButtonPosition position, QWidgetH widget);
C_EXPORT QWidgetH QTabBar_tabButton(QTabBarH handle, int index, QTabBar::ButtonPosition position);
C_EXPORT QTabBar::SelectionBehavior QTabBar_selectionBehaviorOnRemove(QTabBarH handle);
C_EXPORT void QTabBar_setSelectionBehaviorOnRemove(QTabBarH handle, QTabBar::SelectionBehavior behavior);
C_EXPORT bool QTabBar_expanding(QTabBarH handle);
C_EXPORT void QTabBar_setExpanding(QTabBarH handle, bool enabled);
C_EXPORT bool QTabBar_isMovable(QTabBarH handle);
C_EXPORT void QTabBar_setMovable(QTabBarH handle, bool movable);
C_EXPORT bool QTabBar_documentMode(QTabBarH handle);
C_EXPORT void QTabBar_setDocumentMode(QTabBarH handle, bool set);
C_EXPORT void QTabBar_setCurrentIndex(QTabBarH handle, int index);

#endif
