//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qmainwindow_c.h"

QMainWindowH QMainWindow_Create(QWidgetH parent, unsigned int flags)
{
	return (QMainWindowH) new QMainWindow((QWidget*)parent, (Qt::WindowFlags)flags);
}

void QMainWindow_Destroy(QMainWindowH handle)
{
	delete (QMainWindow *)handle;
}

void QMainWindow_iconSize(QMainWindowH handle, PSize retval)
{
	*(QSize *)retval = ((QMainWindow *)handle)->iconSize();
}

void QMainWindow_setIconSize(QMainWindowH handle, const QSizeH iconSize)
{
	((QMainWindow *)handle)->setIconSize(*(const QSize*)iconSize);
}

Qt::ToolButtonStyle QMainWindow_toolButtonStyle(QMainWindowH handle)
{
	return (Qt::ToolButtonStyle) ((QMainWindow *)handle)->toolButtonStyle();
}

void QMainWindow_setToolButtonStyle(QMainWindowH handle, Qt::ToolButtonStyle toolButtonStyle)
{
	((QMainWindow *)handle)->setToolButtonStyle(toolButtonStyle);
}

bool QMainWindow_isAnimated(QMainWindowH handle)
{
	return (bool) ((QMainWindow *)handle)->isAnimated();
}

bool QMainWindow_isDockNestingEnabled(QMainWindowH handle)
{
	return (bool) ((QMainWindow *)handle)->isDockNestingEnabled();
}

bool QMainWindow_documentMode(QMainWindowH handle)
{
	return (bool) ((QMainWindow *)handle)->documentMode();
}

void QMainWindow_setDocumentMode(QMainWindowH handle, bool enabled)
{
	((QMainWindow *)handle)->setDocumentMode(enabled);
}

QTabWidget::TabShape QMainWindow_tabShape(QMainWindowH handle)
{
	return (QTabWidget::TabShape) ((QMainWindow *)handle)->tabShape();
}

void QMainWindow_setTabShape(QMainWindowH handle, QTabWidget::TabShape tabShape)
{
	((QMainWindow *)handle)->setTabShape(tabShape);
}

QTabWidget::TabPosition QMainWindow_tabPosition(QMainWindowH handle, Qt::DockWidgetArea area)
{
	return (QTabWidget::TabPosition) ((QMainWindow *)handle)->tabPosition(area);
}

void QMainWindow_setTabPosition(QMainWindowH handle, unsigned int areas, QTabWidget::TabPosition tabPosition)
{
	((QMainWindow *)handle)->setTabPosition((Qt::DockWidgetAreas)areas, tabPosition);
}

void QMainWindow_setDockOptions(QMainWindowH handle, unsigned int options)
{
	((QMainWindow *)handle)->setDockOptions((QMainWindow::DockOptions)options);
}

unsigned int QMainWindow_dockOptions(QMainWindowH handle)
{
	return (unsigned int) ((QMainWindow *)handle)->dockOptions();
}

bool QMainWindow_isSeparator(QMainWindowH handle, const QPointH pos)
{
	return (bool) ((QMainWindow *)handle)->isSeparator(*(const QPoint*)pos);
}

QMenuBarH QMainWindow_menuBar(QMainWindowH handle)
{
	return (QMenuBarH) ((QMainWindow *)handle)->menuBar();
}

void QMainWindow_setMenuBar(QMainWindowH handle, QMenuBarH menubar)
{
	((QMainWindow *)handle)->setMenuBar((QMenuBar*)menubar);
}

QWidgetH QMainWindow_menuWidget(QMainWindowH handle)
{
	return (QWidgetH) ((QMainWindow *)handle)->menuWidget();
}

void QMainWindow_setMenuWidget(QMainWindowH handle, QWidgetH menubar)
{
	((QMainWindow *)handle)->setMenuWidget((QWidget*)menubar);
}

QStatusBarH QMainWindow_statusBar(QMainWindowH handle)
{
	return (QStatusBarH) ((QMainWindow *)handle)->statusBar();
}

void QMainWindow_setStatusBar(QMainWindowH handle, QStatusBarH statusbar)
{
	((QMainWindow *)handle)->setStatusBar((QStatusBar*)statusbar);
}

QWidgetH QMainWindow_centralWidget(QMainWindowH handle)
{
	return (QWidgetH) ((QMainWindow *)handle)->centralWidget();
}

void QMainWindow_setCentralWidget(QMainWindowH handle, QWidgetH widget)
{
	((QMainWindow *)handle)->setCentralWidget((QWidget*)widget);
}

void QMainWindow_setCorner(QMainWindowH handle, Qt::Corner corner, Qt::DockWidgetArea area)
{
	((QMainWindow *)handle)->setCorner(corner, area);
}

Qt::DockWidgetArea QMainWindow_corner(QMainWindowH handle, Qt::Corner corner)
{
	return (Qt::DockWidgetArea) ((QMainWindow *)handle)->corner(corner);
}

void QMainWindow_addToolBarBreak(QMainWindowH handle, Qt::ToolBarArea area)
{
	((QMainWindow *)handle)->addToolBarBreak(area);
}

void QMainWindow_insertToolBarBreak(QMainWindowH handle, QToolBarH before)
{
	((QMainWindow *)handle)->insertToolBarBreak((QToolBar*)before);
}

void QMainWindow_addToolBar(QMainWindowH handle, Qt::ToolBarArea area, QToolBarH toolbar)
{
	((QMainWindow *)handle)->addToolBar(area, (QToolBar*)toolbar);
}

void QMainWindow_addToolBar2(QMainWindowH handle, QToolBarH toolbar)
{
	((QMainWindow *)handle)->addToolBar((QToolBar*)toolbar);
}

QToolBarH QMainWindow_addToolBar3(QMainWindowH handle, PWideString title)
{
	QString t_title;
	copyPWideStringToQString(title, t_title);
	return (QToolBarH) ((QMainWindow *)handle)->addToolBar(t_title);
}

void QMainWindow_insertToolBar(QMainWindowH handle, QToolBarH before, QToolBarH toolbar)
{
	((QMainWindow *)handle)->insertToolBar((QToolBar*)before, (QToolBar*)toolbar);
}

void QMainWindow_removeToolBar(QMainWindowH handle, QToolBarH toolbar)
{
	((QMainWindow *)handle)->removeToolBar((QToolBar*)toolbar);
}

void QMainWindow_removeToolBarBreak(QMainWindowH handle, QToolBarH before)
{
	((QMainWindow *)handle)->removeToolBarBreak((QToolBar*)before);
}

void QMainWindow_setUnifiedTitleAndToolBarOnMac(QMainWindowH handle, bool set)
{
	((QMainWindow *)handle)->setUnifiedTitleAndToolBarOnMac(set);
}

bool QMainWindow_unifiedTitleAndToolBarOnMac(QMainWindowH handle)
{
	return (bool) ((QMainWindow *)handle)->unifiedTitleAndToolBarOnMac();
}

Qt::ToolBarArea QMainWindow_toolBarArea(QMainWindowH handle, QToolBarH toolbar)
{
	return (Qt::ToolBarArea) ((QMainWindow *)handle)->toolBarArea((QToolBar*)toolbar);
}

bool QMainWindow_toolBarBreak(QMainWindowH handle, QToolBarH toolbar)
{
	return (bool) ((QMainWindow *)handle)->toolBarBreak((QToolBar*)toolbar);
}

void QMainWindow_addDockWidget(QMainWindowH handle, Qt::DockWidgetArea area, QDockWidgetH dockwidget)
{
	((QMainWindow *)handle)->addDockWidget(area, (QDockWidget*)dockwidget);
}

void QMainWindow_addDockWidget2(QMainWindowH handle, Qt::DockWidgetArea area, QDockWidgetH dockwidget, Qt::Orientation orientation)
{
	((QMainWindow *)handle)->addDockWidget(area, (QDockWidget*)dockwidget, orientation);
}

void QMainWindow_splitDockWidget(QMainWindowH handle, QDockWidgetH after, QDockWidgetH dockwidget, Qt::Orientation orientation)
{
	((QMainWindow *)handle)->splitDockWidget((QDockWidget*)after, (QDockWidget*)dockwidget, orientation);
}

void QMainWindow_tabifyDockWidget(QMainWindowH handle, QDockWidgetH first, QDockWidgetH second)
{
	((QMainWindow *)handle)->tabifyDockWidget((QDockWidget*)first, (QDockWidget*)second);
}

void QMainWindow_tabifiedDockWidgets(QMainWindowH handle, PPtrIntArray retval, QDockWidgetH dockwidget)
{
	QList<QDockWidget*> t_retval;
	t_retval = ((QMainWindow *)handle)->tabifiedDockWidgets((QDockWidget*)dockwidget);
	copyQListTemplateToPtrIntArray(t_retval, retval);
}

void QMainWindow_removeDockWidget(QMainWindowH handle, QDockWidgetH dockwidget)
{
	((QMainWindow *)handle)->removeDockWidget((QDockWidget*)dockwidget);
}

bool QMainWindow_restoreDockWidget(QMainWindowH handle, QDockWidgetH dockwidget)
{
	return (bool) ((QMainWindow *)handle)->restoreDockWidget((QDockWidget*)dockwidget);
}

Qt::DockWidgetArea QMainWindow_dockWidgetArea(QMainWindowH handle, QDockWidgetH dockwidget)
{
	return (Qt::DockWidgetArea) ((QMainWindow *)handle)->dockWidgetArea((QDockWidget*)dockwidget);
}

void QMainWindow_saveState(QMainWindowH handle, QByteArrayH retval, int version)
{
	*(QByteArray *)retval = ((QMainWindow *)handle)->saveState(version);
}

bool QMainWindow_restoreState(QMainWindowH handle, const QByteArrayH state, int version)
{
	return (bool) ((QMainWindow *)handle)->restoreState(*(const QByteArray*)state, version);
}

QMenuH QMainWindow_createPopupMenu(QMainWindowH handle)
{
	return (QMenuH) ((QMainWindow *)handle)->createPopupMenu();
}

void QMainWindow_setAnimated(QMainWindowH handle, bool enabled)
{
	((QMainWindow *)handle)->setAnimated(enabled);
}

void QMainWindow_setDockNestingEnabled(QMainWindowH handle, bool enabled)
{
	((QMainWindow *)handle)->setDockNestingEnabled(enabled);
}

QWidgetH QMainWindow_takeCentralWidget(QMainWindowH handle)
{
  return (QWidgetH) ((QMainWindow *)handle)->takeCentralWidget();
}
