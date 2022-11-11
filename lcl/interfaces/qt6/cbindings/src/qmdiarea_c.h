//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QMDIAREA_C_H
#define QMDIAREA_C_H

#include <QtWidgets>
#include "pascalbind.h"

C_EXPORT QMdiAreaH QMdiArea_Create(QWidgetH parent);
C_EXPORT void QMdiArea_Destroy(QMdiAreaH handle);
C_EXPORT void QMdiArea_sizeHint(QMdiAreaH handle, PSize retval);
C_EXPORT void QMdiArea_minimumSizeHint(QMdiAreaH handle, PSize retval);
C_EXPORT QMdiSubWindowH QMdiArea_currentSubWindow(QMdiAreaH handle);
C_EXPORT QMdiSubWindowH QMdiArea_activeSubWindow(QMdiAreaH handle);
C_EXPORT void QMdiArea_subWindowList(QMdiAreaH handle, PPtrIntArray retval, QMdiArea::WindowOrder order);
C_EXPORT QMdiSubWindowH QMdiArea_addSubWindow(QMdiAreaH handle, QWidgetH widget, unsigned int flags);
C_EXPORT void QMdiArea_removeSubWindow(QMdiAreaH handle, QWidgetH widget);
C_EXPORT void QMdiArea_background(QMdiAreaH handle, QBrushH retval);
C_EXPORT void QMdiArea_setBackground(QMdiAreaH handle, const QBrushH background);
C_EXPORT QMdiArea::WindowOrder QMdiArea_activationOrder(QMdiAreaH handle);
C_EXPORT void QMdiArea_setActivationOrder(QMdiAreaH handle, QMdiArea::WindowOrder order);
C_EXPORT void QMdiArea_setOption(QMdiAreaH handle, QMdiArea::AreaOption option, bool on);
C_EXPORT bool QMdiArea_testOption(QMdiAreaH handle, QMdiArea::AreaOption opton);
C_EXPORT void QMdiArea_setViewMode(QMdiAreaH handle, QMdiArea::ViewMode mode);
C_EXPORT QMdiArea::ViewMode QMdiArea_viewMode(QMdiAreaH handle);
C_EXPORT bool QMdiArea_documentMode(QMdiAreaH handle);
C_EXPORT void QMdiArea_setDocumentMode(QMdiAreaH handle, bool enabled);
C_EXPORT void QMdiArea_setTabsClosable(QMdiAreaH handle, bool closable);
C_EXPORT bool QMdiArea_tabsClosable(QMdiAreaH handle);
C_EXPORT void QMdiArea_setTabsMovable(QMdiAreaH handle, bool movable);
C_EXPORT bool QMdiArea_tabsMovable(QMdiAreaH handle);
C_EXPORT void QMdiArea_setTabShape(QMdiAreaH handle, QTabWidget::TabShape shape);
C_EXPORT QTabWidget::TabShape QMdiArea_tabShape(QMdiAreaH handle);
C_EXPORT void QMdiArea_setTabPosition(QMdiAreaH handle, QTabWidget::TabPosition position);
C_EXPORT QTabWidget::TabPosition QMdiArea_tabPosition(QMdiAreaH handle);
C_EXPORT void QMdiArea_setActiveSubWindow(QMdiAreaH handle, QMdiSubWindowH window);
C_EXPORT void QMdiArea_tileSubWindows(QMdiAreaH handle);
C_EXPORT void QMdiArea_cascadeSubWindows(QMdiAreaH handle);
C_EXPORT void QMdiArea_closeActiveSubWindow(QMdiAreaH handle);
C_EXPORT void QMdiArea_closeAllSubWindows(QMdiAreaH handle);
C_EXPORT void QMdiArea_activateNextSubWindow(QMdiAreaH handle);
C_EXPORT void QMdiArea_activatePreviousSubWindow(QMdiAreaH handle);

#endif
