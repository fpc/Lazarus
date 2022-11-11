//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QMAINWINDOW_C_H
#define QMAINWINDOW_C_H

#include <QtWidgets>
#include "pascalbind.h"

C_EXPORT QMainWindowH QMainWindow_Create(QWidgetH parent, unsigned int flags);
C_EXPORT void QMainWindow_Destroy(QMainWindowH handle);
C_EXPORT void QMainWindow_iconSize(QMainWindowH handle, PSize retval);
C_EXPORT void QMainWindow_setIconSize(QMainWindowH handle, const QSizeH iconSize);
C_EXPORT Qt::ToolButtonStyle QMainWindow_toolButtonStyle(QMainWindowH handle);
C_EXPORT void QMainWindow_setToolButtonStyle(QMainWindowH handle, Qt::ToolButtonStyle toolButtonStyle);
C_EXPORT bool QMainWindow_isAnimated(QMainWindowH handle);
C_EXPORT bool QMainWindow_isDockNestingEnabled(QMainWindowH handle);
C_EXPORT bool QMainWindow_documentMode(QMainWindowH handle);
C_EXPORT void QMainWindow_setDocumentMode(QMainWindowH handle, bool enabled);
C_EXPORT QTabWidget::TabShape QMainWindow_tabShape(QMainWindowH handle);
C_EXPORT void QMainWindow_setTabShape(QMainWindowH handle, QTabWidget::TabShape tabShape);
C_EXPORT QTabWidget::TabPosition QMainWindow_tabPosition(QMainWindowH handle, Qt::DockWidgetArea area);
C_EXPORT void QMainWindow_setTabPosition(QMainWindowH handle, unsigned int areas, QTabWidget::TabPosition tabPosition);
C_EXPORT void QMainWindow_setDockOptions(QMainWindowH handle, unsigned int options);
C_EXPORT unsigned int QMainWindow_dockOptions(QMainWindowH handle);
C_EXPORT bool QMainWindow_isSeparator(QMainWindowH handle, const QPointH pos);
C_EXPORT QMenuBarH QMainWindow_menuBar(QMainWindowH handle);
C_EXPORT void QMainWindow_setMenuBar(QMainWindowH handle, QMenuBarH menubar);
C_EXPORT QWidgetH QMainWindow_menuWidget(QMainWindowH handle);
C_EXPORT void QMainWindow_setMenuWidget(QMainWindowH handle, QWidgetH menubar);
C_EXPORT QStatusBarH QMainWindow_statusBar(QMainWindowH handle);
C_EXPORT void QMainWindow_setStatusBar(QMainWindowH handle, QStatusBarH statusbar);
C_EXPORT QWidgetH QMainWindow_centralWidget(QMainWindowH handle);
C_EXPORT void QMainWindow_setCentralWidget(QMainWindowH handle, QWidgetH widget);
C_EXPORT void QMainWindow_setCorner(QMainWindowH handle, Qt::Corner corner, Qt::DockWidgetArea area);
C_EXPORT Qt::DockWidgetArea QMainWindow_corner(QMainWindowH handle, Qt::Corner corner);
C_EXPORT void QMainWindow_addToolBarBreak(QMainWindowH handle, Qt::ToolBarArea area);
C_EXPORT void QMainWindow_insertToolBarBreak(QMainWindowH handle, QToolBarH before);
C_EXPORT void QMainWindow_addToolBar(QMainWindowH handle, Qt::ToolBarArea area, QToolBarH toolbar);
C_EXPORT void QMainWindow_addToolBar2(QMainWindowH handle, QToolBarH toolbar);
C_EXPORT QToolBarH QMainWindow_addToolBar3(QMainWindowH handle, PWideString title);
C_EXPORT void QMainWindow_insertToolBar(QMainWindowH handle, QToolBarH before, QToolBarH toolbar);
C_EXPORT void QMainWindow_removeToolBar(QMainWindowH handle, QToolBarH toolbar);
C_EXPORT void QMainWindow_removeToolBarBreak(QMainWindowH handle, QToolBarH before);
C_EXPORT void QMainWindow_setUnifiedTitleAndToolBarOnMac(QMainWindowH handle, bool set);
C_EXPORT bool QMainWindow_unifiedTitleAndToolBarOnMac(QMainWindowH handle);
C_EXPORT Qt::ToolBarArea QMainWindow_toolBarArea(QMainWindowH handle, QToolBarH toolbar);
C_EXPORT bool QMainWindow_toolBarBreak(QMainWindowH handle, QToolBarH toolbar);
C_EXPORT void QMainWindow_addDockWidget(QMainWindowH handle, Qt::DockWidgetArea area, QDockWidgetH dockwidget);
C_EXPORT void QMainWindow_addDockWidget2(QMainWindowH handle, Qt::DockWidgetArea area, QDockWidgetH dockwidget, Qt::Orientation orientation);
C_EXPORT void QMainWindow_splitDockWidget(QMainWindowH handle, QDockWidgetH after, QDockWidgetH dockwidget, Qt::Orientation orientation);
C_EXPORT void QMainWindow_tabifyDockWidget(QMainWindowH handle, QDockWidgetH first, QDockWidgetH second);
C_EXPORT void QMainWindow_tabifiedDockWidgets(QMainWindowH handle, PPtrIntArray retval, QDockWidgetH dockwidget);
C_EXPORT void QMainWindow_removeDockWidget(QMainWindowH handle, QDockWidgetH dockwidget);
C_EXPORT bool QMainWindow_restoreDockWidget(QMainWindowH handle, QDockWidgetH dockwidget);
C_EXPORT Qt::DockWidgetArea QMainWindow_dockWidgetArea(QMainWindowH handle, QDockWidgetH dockwidget);
C_EXPORT void QMainWindow_saveState(QMainWindowH handle, QByteArrayH retval, int version);
C_EXPORT bool QMainWindow_restoreState(QMainWindowH handle, const QByteArrayH state, int version);
C_EXPORT QMenuH QMainWindow_createPopupMenu(QMainWindowH handle);
C_EXPORT void QMainWindow_setAnimated(QMainWindowH handle, bool enabled);
C_EXPORT void QMainWindow_setDockNestingEnabled(QMainWindowH handle, bool enabled);
C_EXPORT QWidgetH QMainWindow_takeCentralWidget(QMainWindowH handle);

#endif
