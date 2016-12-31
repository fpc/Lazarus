//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QTABWIDGET_C_H
#define QTABWIDGET_C_H

#include <QtWidgets>
#include "pascalbind.h"

C_EXPORT QTabWidgetH QTabWidget_Create(QWidgetH parent);
C_EXPORT void QTabWidget_Destroy(QTabWidgetH handle);
C_EXPORT int QTabWidget_addTab(QTabWidgetH handle, QWidgetH widget, PWideString AnonParam2);
C_EXPORT int QTabWidget_addTab2(QTabWidgetH handle, QWidgetH widget, const QIconH icon, PWideString label);
C_EXPORT int QTabWidget_insertTab(QTabWidgetH handle, int index, QWidgetH widget, PWideString AnonParam3);
C_EXPORT int QTabWidget_insertTab2(QTabWidgetH handle, int index, QWidgetH widget, const QIconH icon, PWideString label);
C_EXPORT void QTabWidget_removeTab(QTabWidgetH handle, int index);
C_EXPORT bool QTabWidget_isTabEnabled(QTabWidgetH handle, int index);
C_EXPORT void QTabWidget_setTabEnabled(QTabWidgetH handle, int index, bool AnonParam2);
C_EXPORT void QTabWidget_tabText(QTabWidgetH handle, PWideString retval, int index);
C_EXPORT void QTabWidget_setTabText(QTabWidgetH handle, int index, PWideString AnonParam2);
C_EXPORT void QTabWidget_tabIcon(QTabWidgetH handle, QIconH retval, int index);
C_EXPORT void QTabWidget_setTabIcon(QTabWidgetH handle, int index, const QIconH icon);
C_EXPORT void QTabWidget_setTabToolTip(QTabWidgetH handle, int index, PWideString tip);
C_EXPORT void QTabWidget_tabToolTip(QTabWidgetH handle, PWideString retval, int index);
C_EXPORT void QTabWidget_setTabWhatsThis(QTabWidgetH handle, int index, PWideString text);
C_EXPORT void QTabWidget_tabWhatsThis(QTabWidgetH handle, PWideString retval, int index);
C_EXPORT int QTabWidget_currentIndex(QTabWidgetH handle);
C_EXPORT QWidgetH QTabWidget_currentWidget(QTabWidgetH handle);
C_EXPORT QWidgetH QTabWidget_widget(QTabWidgetH handle, int index);
C_EXPORT int QTabWidget_indexOf(QTabWidgetH handle, QWidgetH widget);
C_EXPORT int QTabWidget_count(QTabWidgetH handle);
C_EXPORT QTabWidget::TabPosition QTabWidget_tabPosition(QTabWidgetH handle);
C_EXPORT void QTabWidget_setTabPosition(QTabWidgetH handle, QTabWidget::TabPosition AnonParam1);
C_EXPORT bool QTabWidget_tabsClosable(QTabWidgetH handle);
C_EXPORT void QTabWidget_setTabsClosable(QTabWidgetH handle, bool closeable);
C_EXPORT bool QTabWidget_isMovable(QTabWidgetH handle);
C_EXPORT void QTabWidget_setMovable(QTabWidgetH handle, bool movable);
C_EXPORT QTabWidget::TabShape QTabWidget_tabShape(QTabWidgetH handle);
C_EXPORT void QTabWidget_setTabShape(QTabWidgetH handle, QTabWidget::TabShape s);
C_EXPORT void QTabWidget_sizeHint(QTabWidgetH handle, PSize retval);
C_EXPORT void QTabWidget_minimumSizeHint(QTabWidgetH handle, PSize retval);
C_EXPORT int QTabWidget_heightForWidth(QTabWidgetH handle, int width);
C_EXPORT bool QTabWidget_hasHeightForWidth(QTabWidgetH handle);
C_EXPORT void QTabWidget_setCornerWidget(QTabWidgetH handle, QWidgetH w, Qt::Corner corner);
C_EXPORT QWidgetH QTabWidget_cornerWidget(QTabWidgetH handle, Qt::Corner corner);
C_EXPORT Qt::TextElideMode QTabWidget_elideMode(QTabWidgetH handle);
C_EXPORT void QTabWidget_setElideMode(QTabWidgetH handle, Qt::TextElideMode AnonParam1);
C_EXPORT void QTabWidget_iconSize(QTabWidgetH handle, PSize retval);
C_EXPORT void QTabWidget_setIconSize(QTabWidgetH handle, const QSizeH size);
C_EXPORT bool QTabWidget_usesScrollButtons(QTabWidgetH handle);
C_EXPORT void QTabWidget_setUsesScrollButtons(QTabWidgetH handle, bool useButtons);
C_EXPORT bool QTabWidget_documentMode(QTabWidgetH handle);
C_EXPORT void QTabWidget_setDocumentMode(QTabWidgetH handle, bool set);
C_EXPORT void QTabWidget_clear(QTabWidgetH handle);
C_EXPORT QTabBarH QTabWidget_tabBar(QTabWidgetH handle);
C_EXPORT void QTabWidget_setCurrentIndex(QTabWidgetH handle, int index);
C_EXPORT void QTabWidget_setCurrentWidget(QTabWidgetH handle, QWidgetH widget);

#endif
