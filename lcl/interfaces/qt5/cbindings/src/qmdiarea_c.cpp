//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qmdiarea_c.h"

QMdiAreaH QMdiArea_Create(QWidgetH parent)
{
	return (QMdiAreaH) new QMdiArea((QWidget*)parent);
}

void QMdiArea_Destroy(QMdiAreaH handle)
{
	delete (QMdiArea *)handle;
}

void QMdiArea_sizeHint(QMdiAreaH handle, PSize retval)
{
	*(QSize *)retval = ((QMdiArea *)handle)->sizeHint();
}

void QMdiArea_minimumSizeHint(QMdiAreaH handle, PSize retval)
{
	*(QSize *)retval = ((QMdiArea *)handle)->minimumSizeHint();
}

QMdiSubWindowH QMdiArea_currentSubWindow(QMdiAreaH handle)
{
	return (QMdiSubWindowH) ((QMdiArea *)handle)->currentSubWindow();
}

QMdiSubWindowH QMdiArea_activeSubWindow(QMdiAreaH handle)
{
	return (QMdiSubWindowH) ((QMdiArea *)handle)->activeSubWindow();
}

void QMdiArea_subWindowList(QMdiAreaH handle, PPtrIntArray retval, QMdiArea::WindowOrder order)
{
	QList<QMdiSubWindow*> t_retval;
	t_retval = ((QMdiArea *)handle)->subWindowList(order);
	copyQListTemplateToPtrIntArray(t_retval, retval);
}

QMdiSubWindowH QMdiArea_addSubWindow(QMdiAreaH handle, QWidgetH widget, unsigned int flags)
{
	return (QMdiSubWindowH) ((QMdiArea *)handle)->addSubWindow((QWidget*)widget, (Qt::WindowFlags)flags);
}

void QMdiArea_removeSubWindow(QMdiAreaH handle, QWidgetH widget)
{
	((QMdiArea *)handle)->removeSubWindow((QWidget*)widget);
}

void QMdiArea_background(QMdiAreaH handle, QBrushH retval)
{
	*(QBrush *)retval = ((QMdiArea *)handle)->background();
}

void QMdiArea_setBackground(QMdiAreaH handle, const QBrushH background)
{
	((QMdiArea *)handle)->setBackground(*(const QBrush*)background);
}

QMdiArea::WindowOrder QMdiArea_activationOrder(QMdiAreaH handle)
{
	return (QMdiArea::WindowOrder) ((QMdiArea *)handle)->activationOrder();
}

void QMdiArea_setActivationOrder(QMdiAreaH handle, QMdiArea::WindowOrder order)
{
	((QMdiArea *)handle)->setActivationOrder(order);
}

void QMdiArea_setOption(QMdiAreaH handle, QMdiArea::AreaOption option, bool on)
{
	((QMdiArea *)handle)->setOption(option, on);
}

bool QMdiArea_testOption(QMdiAreaH handle, QMdiArea::AreaOption opton)
{
	return (bool) ((QMdiArea *)handle)->testOption(opton);
}

void QMdiArea_setViewMode(QMdiAreaH handle, QMdiArea::ViewMode mode)
{
	((QMdiArea *)handle)->setViewMode(mode);
}

QMdiArea::ViewMode QMdiArea_viewMode(QMdiAreaH handle)
{
	return (QMdiArea::ViewMode) ((QMdiArea *)handle)->viewMode();
}

bool QMdiArea_documentMode(QMdiAreaH handle)
{
	return (bool) ((QMdiArea *)handle)->documentMode();
}

void QMdiArea_setDocumentMode(QMdiAreaH handle, bool enabled)
{
	((QMdiArea *)handle)->setDocumentMode(enabled);
}

void QMdiArea_setTabsClosable(QMdiAreaH handle, bool closable)
{
	((QMdiArea *)handle)->setTabsClosable(closable);
}

bool QMdiArea_tabsClosable(QMdiAreaH handle)
{
	return (bool) ((QMdiArea *)handle)->tabsClosable();
}

void QMdiArea_setTabsMovable(QMdiAreaH handle, bool movable)
{
	((QMdiArea *)handle)->setTabsMovable(movable);
}

bool QMdiArea_tabsMovable(QMdiAreaH handle)
{
	return (bool) ((QMdiArea *)handle)->tabsMovable();
}

void QMdiArea_setTabShape(QMdiAreaH handle, QTabWidget::TabShape shape)
{
	((QMdiArea *)handle)->setTabShape(shape);
}

QTabWidget::TabShape QMdiArea_tabShape(QMdiAreaH handle)
{
	return (QTabWidget::TabShape) ((QMdiArea *)handle)->tabShape();
}

void QMdiArea_setTabPosition(QMdiAreaH handle, QTabWidget::TabPosition position)
{
	((QMdiArea *)handle)->setTabPosition(position);
}

QTabWidget::TabPosition QMdiArea_tabPosition(QMdiAreaH handle)
{
	return (QTabWidget::TabPosition) ((QMdiArea *)handle)->tabPosition();
}

void QMdiArea_setActiveSubWindow(QMdiAreaH handle, QMdiSubWindowH window)
{
	((QMdiArea *)handle)->setActiveSubWindow((QMdiSubWindow*)window);
}

void QMdiArea_tileSubWindows(QMdiAreaH handle)
{
	((QMdiArea *)handle)->tileSubWindows();
}

void QMdiArea_cascadeSubWindows(QMdiAreaH handle)
{
	((QMdiArea *)handle)->cascadeSubWindows();
}

void QMdiArea_closeActiveSubWindow(QMdiAreaH handle)
{
	((QMdiArea *)handle)->closeActiveSubWindow();
}

void QMdiArea_closeAllSubWindows(QMdiAreaH handle)
{
	((QMdiArea *)handle)->closeAllSubWindows();
}

void QMdiArea_activateNextSubWindow(QMdiAreaH handle)
{
	((QMdiArea *)handle)->activateNextSubWindow();
}

void QMdiArea_activatePreviousSubWindow(QMdiAreaH handle)
{
	((QMdiArea *)handle)->activatePreviousSubWindow();
}

