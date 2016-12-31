//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qtabbar_c.h"

QTabBarH QTabBar_Create(QWidgetH parent)
{
	return (QTabBarH) new QTabBar((QWidget*)parent);
}

void QTabBar_Destroy(QTabBarH handle)
{
	delete (QTabBar *)handle;
}

QTabBar::Shape QTabBar_shape(QTabBarH handle)
{
	return (QTabBar::Shape) ((QTabBar *)handle)->shape();
}

void QTabBar_setShape(QTabBarH handle, QTabBar::Shape shape)
{
	((QTabBar *)handle)->setShape(shape);
}

int QTabBar_addTab(QTabBarH handle, PWideString text)
{
	QString t_text;
	copyPWideStringToQString(text, t_text);
	return (int) ((QTabBar *)handle)->addTab(t_text);
}

int QTabBar_addTab2(QTabBarH handle, const QIconH icon, PWideString text)
{
	QString t_text;
	copyPWideStringToQString(text, t_text);
	return (int) ((QTabBar *)handle)->addTab(*(const QIcon*)icon, t_text);
}

int QTabBar_insertTab(QTabBarH handle, int index, PWideString text)
{
	QString t_text;
	copyPWideStringToQString(text, t_text);
	return (int) ((QTabBar *)handle)->insertTab(index, t_text);
}

int QTabBar_insertTab2(QTabBarH handle, int index, const QIconH icon, PWideString text)
{
	QString t_text;
	copyPWideStringToQString(text, t_text);
	return (int) ((QTabBar *)handle)->insertTab(index, *(const QIcon*)icon, t_text);
}

void QTabBar_removeTab(QTabBarH handle, int index)
{
	((QTabBar *)handle)->removeTab(index);
}

void QTabBar_moveTab(QTabBarH handle, int from, int to)
{
	((QTabBar *)handle)->moveTab(from, to);
}

bool QTabBar_isTabEnabled(QTabBarH handle, int index)
{
	return (bool) ((QTabBar *)handle)->isTabEnabled(index);
}

void QTabBar_setTabEnabled(QTabBarH handle, int index, bool AnonParam2)
{
	((QTabBar *)handle)->setTabEnabled(index, AnonParam2);
}

void QTabBar_tabText(QTabBarH handle, PWideString retval, int index)
{
	QString t_retval;
	t_retval = ((QTabBar *)handle)->tabText(index);
	copyQStringToPWideString(t_retval, retval);
}

void QTabBar_setTabText(QTabBarH handle, int index, PWideString text)
{
	QString t_text;
	copyPWideStringToQString(text, t_text);
	((QTabBar *)handle)->setTabText(index, t_text);
}

void QTabBar_tabTextColor(QTabBarH handle, PQColor retval, int index)
{
	*(QColor *)retval = ((QTabBar *)handle)->tabTextColor(index);
}

void QTabBar_setTabTextColor(QTabBarH handle, int index, const QColorH color)
{
	((QTabBar *)handle)->setTabTextColor(index, *(const QColor*)color);
}

void QTabBar_tabIcon(QTabBarH handle, QIconH retval, int index)
{
	*(QIcon *)retval = ((QTabBar *)handle)->tabIcon(index);
}

void QTabBar_setTabIcon(QTabBarH handle, int index, const QIconH icon)
{
	((QTabBar *)handle)->setTabIcon(index, *(const QIcon*)icon);
}

Qt::TextElideMode QTabBar_elideMode(QTabBarH handle)
{
	return (Qt::TextElideMode) ((QTabBar *)handle)->elideMode();
}

void QTabBar_setElideMode(QTabBarH handle, Qt::TextElideMode AnonParam1)
{
	((QTabBar *)handle)->setElideMode(AnonParam1);
}

void QTabBar_setTabToolTip(QTabBarH handle, int index, PWideString tip)
{
	QString t_tip;
	copyPWideStringToQString(tip, t_tip);
	((QTabBar *)handle)->setTabToolTip(index, t_tip);
}

void QTabBar_tabToolTip(QTabBarH handle, PWideString retval, int index)
{
	QString t_retval;
	t_retval = ((QTabBar *)handle)->tabToolTip(index);
	copyQStringToPWideString(t_retval, retval);
}

void QTabBar_setTabWhatsThis(QTabBarH handle, int index, PWideString text)
{
	QString t_text;
	copyPWideStringToQString(text, t_text);
	((QTabBar *)handle)->setTabWhatsThis(index, t_text);
}

void QTabBar_tabWhatsThis(QTabBarH handle, PWideString retval, int index)
{
	QString t_retval;
	t_retval = ((QTabBar *)handle)->tabWhatsThis(index);
	copyQStringToPWideString(t_retval, retval);
}

void QTabBar_setTabData(QTabBarH handle, int index, const QVariantH data)
{
	((QTabBar *)handle)->setTabData(index, *(const QVariant*)data);
}

void QTabBar_tabData(QTabBarH handle, QVariantH retval, int index)
{
	*(QVariant *)retval = ((QTabBar *)handle)->tabData(index);
}

void QTabBar_tabRect(QTabBarH handle, PRect retval, int index)
{
	QRect t_retval;
	t_retval = ((QTabBar *)handle)->tabRect(index);
	copyQRectToPRect(t_retval, retval);
}

int QTabBar_tabAt(QTabBarH handle, const QPointH pos)
{
	return (int) ((QTabBar *)handle)->tabAt(*(const QPoint*)pos);
}

int QTabBar_currentIndex(QTabBarH handle)
{
	return (int) ((QTabBar *)handle)->currentIndex();
}

int QTabBar_count(QTabBarH handle)
{
	return (int) ((QTabBar *)handle)->count();
}

void QTabBar_sizeHint(QTabBarH handle, PSize retval)
{
	*(QSize *)retval = ((QTabBar *)handle)->sizeHint();
}

void QTabBar_minimumSizeHint(QTabBarH handle, PSize retval)
{
	*(QSize *)retval = ((QTabBar *)handle)->minimumSizeHint();
}

void QTabBar_setDrawBase(QTabBarH handle, bool drawTheBase)
{
	((QTabBar *)handle)->setDrawBase(drawTheBase);
}

bool QTabBar_drawBase(QTabBarH handle)
{
	return (bool) ((QTabBar *)handle)->drawBase();
}

void QTabBar_iconSize(QTabBarH handle, PSize retval)
{
	*(QSize *)retval = ((QTabBar *)handle)->iconSize();
}

void QTabBar_setIconSize(QTabBarH handle, const QSizeH size)
{
	((QTabBar *)handle)->setIconSize(*(const QSize*)size);
}

bool QTabBar_usesScrollButtons(QTabBarH handle)
{
	return (bool) ((QTabBar *)handle)->usesScrollButtons();
}

void QTabBar_setUsesScrollButtons(QTabBarH handle, bool useButtons)
{
	((QTabBar *)handle)->setUsesScrollButtons(useButtons);
}

bool QTabBar_tabsClosable(QTabBarH handle)
{
	return (bool) ((QTabBar *)handle)->tabsClosable();
}

void QTabBar_setTabsClosable(QTabBarH handle, bool closable)
{
	((QTabBar *)handle)->setTabsClosable(closable);
}

void QTabBar_setTabButton(QTabBarH handle, int index, QTabBar::ButtonPosition position, QWidgetH widget)
{
	((QTabBar *)handle)->setTabButton(index, position, (QWidget*)widget);
}

QWidgetH QTabBar_tabButton(QTabBarH handle, int index, QTabBar::ButtonPosition position)
{
	return (QWidgetH) ((QTabBar *)handle)->tabButton(index, position);
}

QTabBar::SelectionBehavior QTabBar_selectionBehaviorOnRemove(QTabBarH handle)
{
	return (QTabBar::SelectionBehavior) ((QTabBar *)handle)->selectionBehaviorOnRemove();
}

void QTabBar_setSelectionBehaviorOnRemove(QTabBarH handle, QTabBar::SelectionBehavior behavior)
{
	((QTabBar *)handle)->setSelectionBehaviorOnRemove(behavior);
}

bool QTabBar_expanding(QTabBarH handle)
{
	return (bool) ((QTabBar *)handle)->expanding();
}

void QTabBar_setExpanding(QTabBarH handle, bool enabled)
{
	((QTabBar *)handle)->setExpanding(enabled);
}

bool QTabBar_isMovable(QTabBarH handle)
{
	return (bool) ((QTabBar *)handle)->isMovable();
}

void QTabBar_setMovable(QTabBarH handle, bool movable)
{
	((QTabBar *)handle)->setMovable(movable);
}

bool QTabBar_documentMode(QTabBarH handle)
{
	return (bool) ((QTabBar *)handle)->documentMode();
}

void QTabBar_setDocumentMode(QTabBarH handle, bool set)
{
	((QTabBar *)handle)->setDocumentMode(set);
}

void QTabBar_setCurrentIndex(QTabBarH handle, int index)
{
	((QTabBar *)handle)->setCurrentIndex(index);
}

