//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qmenu_c.h"

QMenuH QMenu_Create(QWidgetH parent)
{
	return (QMenuH) new QMenu((QWidget*)parent);
}

void QMenu_Destroy(QMenuH handle)
{
	delete (QMenu *)handle;
}

QMenuH QMenu_Create2(PWideString title, QWidgetH parent)
{
	QString t_title;
	copyPWideStringToQString(title, t_title);
	return (QMenuH) new QMenu(t_title, (QWidget*)parent);
}

QActionH QMenu_addAction(QMenuH handle, PWideString text)
{
	QString t_text;
	copyPWideStringToQString(text, t_text);
	return (QActionH) ((QMenu *)handle)->addAction(t_text);
}

QActionH QMenu_addAction2(QMenuH handle, const QIconH icon, PWideString text)
{
	QString t_text;
	copyPWideStringToQString(text, t_text);
	return (QActionH) ((QMenu *)handle)->addAction(*(const QIcon*)icon, t_text);
}

QActionH QMenu_addAction3(QMenuH handle, PWideString text, const QObjectH receiver, const char* member, const QKeySequenceH shortcut)
{
	QString t_text;
	copyPWideStringToQString(text, t_text);
	return (QActionH) ((QMenu *)handle)->addAction(t_text, (const QObject*)receiver, member, *(const QKeySequence*)shortcut);
}

QActionH QMenu_addAction4(QMenuH handle, const QIconH icon, PWideString text, const QObjectH receiver, const char* member, const QKeySequenceH shortcut)
{
	QString t_text;
	copyPWideStringToQString(text, t_text);
	return (QActionH) ((QMenu *)handle)->addAction(*(const QIcon*)icon, t_text, (const QObject*)receiver, member, *(const QKeySequence*)shortcut);
}

QActionH QMenu_addMenu(QMenuH handle, QMenuH menu)
{
	return (QActionH) ((QMenu *)handle)->addMenu((QMenu*)menu);
}

QMenuH QMenu_addMenu2(QMenuH handle, PWideString title)
{
	QString t_title;
	copyPWideStringToQString(title, t_title);
	return (QMenuH) ((QMenu *)handle)->addMenu(t_title);
}

QMenuH QMenu_addMenu3(QMenuH handle, const QIconH icon, PWideString title)
{
	QString t_title;
	copyPWideStringToQString(title, t_title);
	return (QMenuH) ((QMenu *)handle)->addMenu(*(const QIcon*)icon, t_title);
}

QActionH QMenu_addSeparator(QMenuH handle)
{
	return (QActionH) ((QMenu *)handle)->addSeparator();
}

QActionH QMenu_addSection(QMenuH handle, PWideString text)
{
	QString t_text;
	copyPWideStringToQString(text, t_text);
	return (QActionH) ((QMenu *)handle)->addSection(t_text);
}

QActionH QMenu_addSection2(QMenuH handle, const QIconH icon, PWideString text)
{
	QString t_text;
	copyPWideStringToQString(text, t_text);
	return (QActionH) ((QMenu *)handle)->addSection(*(const QIcon*)icon, t_text);
}

QActionH QMenu_insertMenu(QMenuH handle, QActionH before, QMenuH menu)
{
	return (QActionH) ((QMenu *)handle)->insertMenu((QAction*)before, (QMenu*)menu);
}

QActionH QMenu_insertSeparator(QMenuH handle, QActionH before)
{
	return (QActionH) ((QMenu *)handle)->insertSeparator((QAction*)before);
}

QActionH QMenu_insertSection(QMenuH handle, QActionH before, PWideString text)
{
	QString t_text;
	copyPWideStringToQString(text, t_text);
	return (QActionH) ((QMenu *)handle)->insertSection((QAction*)before, t_text);
}

QActionH QMenu_insertSection2(QMenuH handle, QActionH before, const QIconH icon, PWideString text)
{
	QString t_text;
	copyPWideStringToQString(text, t_text);
	return (QActionH) ((QMenu *)handle)->insertSection((QAction*)before, *(const QIcon*)icon, t_text);
}

bool QMenu_isEmpty(QMenuH handle)
{
	return (bool) ((QMenu *)handle)->isEmpty();
}

void QMenu_clear(QMenuH handle)
{
	((QMenu *)handle)->clear();
}

void QMenu_setTearOffEnabled(QMenuH handle, bool AnonParam1)
{
	((QMenu *)handle)->setTearOffEnabled(AnonParam1);
}

bool QMenu_isTearOffEnabled(QMenuH handle)
{
	return (bool) ((QMenu *)handle)->isTearOffEnabled();
}

bool QMenu_isTearOffMenuVisible(QMenuH handle)
{
	return (bool) ((QMenu *)handle)->isTearOffMenuVisible();
}

void QMenu_hideTearOffMenu(QMenuH handle)
{
	((QMenu *)handle)->hideTearOffMenu();
}

void QMenu_setDefaultAction(QMenuH handle, QActionH AnonParam1)
{
	((QMenu *)handle)->setDefaultAction((QAction*)AnonParam1);
}

QActionH QMenu_defaultAction(QMenuH handle)
{
	return (QActionH) ((QMenu *)handle)->defaultAction();
}

void QMenu_setActiveAction(QMenuH handle, QActionH act)
{
	((QMenu *)handle)->setActiveAction((QAction*)act);
}

QActionH QMenu_activeAction(QMenuH handle)
{
	return (QActionH) ((QMenu *)handle)->activeAction();
}

void QMenu_popup(QMenuH handle, const QPointH pos, QActionH at)
{
	((QMenu *)handle)->popup(*(const QPoint*)pos, (QAction*)at);
}

QActionH QMenu_exec(QMenuH handle)
{
	return (QActionH) ((QMenu *)handle)->exec();
}

QActionH QMenu_exec2(QMenuH handle, const QPointH pos, QActionH at)
{
	return (QActionH) ((QMenu *)handle)->exec(*(const QPoint*)pos, (QAction*)at);
}

QActionH QMenu_exec3(PPtrIntArray actions, const QPointH pos, QActionH at, QWidgetH parent)
{
	QList<QAction*> t_actions;
	copyPtrIntArrayToQListTemplate(actions, t_actions);
	return (QActionH) QMenu::exec(t_actions, *(const QPoint*)pos, (QAction*)at, (QWidget*)parent);
}

void QMenu_sizeHint(QMenuH handle, PSize retval)
{
	*(QSize *)retval = ((QMenu *)handle)->sizeHint();
}

void QMenu_actionGeometry(QMenuH handle, PRect retval, QActionH AnonParam1)
{
	QRect t_retval;
	t_retval = ((QMenu *)handle)->actionGeometry((QAction*)AnonParam1);
	copyQRectToPRect(t_retval, retval);
}

QActionH QMenu_actionAt(QMenuH handle, const QPointH AnonParam1)
{
	return (QActionH) ((QMenu *)handle)->actionAt(*(const QPoint*)AnonParam1);
}

QActionH QMenu_menuAction(QMenuH handle)
{
	return (QActionH) ((QMenu *)handle)->menuAction();
}

void QMenu_title(QMenuH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QMenu *)handle)->title();
	copyQStringToPWideString(t_retval, retval);
}

void QMenu_setTitle(QMenuH handle, PWideString title)
{
	QString t_title;
	copyPWideStringToQString(title, t_title);
	((QMenu *)handle)->setTitle(t_title);
}

void QMenu_icon(QMenuH handle, QIconH retval)
{
	*(QIcon *)retval = ((QMenu *)handle)->icon();
}

void QMenu_setIcon(QMenuH handle, const QIconH icon)
{
	((QMenu *)handle)->setIcon(*(const QIcon*)icon);
}

void QMenu_setNoReplayFor(QMenuH handle, QWidgetH widget)
{
	((QMenu *)handle)->setNoReplayFor((QWidget*)widget);
}

bool QMenu_separatorsCollapsible(QMenuH handle)
{
	return (bool) ((QMenu *)handle)->separatorsCollapsible();
}

void QMenu_setSeparatorsCollapsible(QMenuH handle, bool collapse)
{
	((QMenu *)handle)->setSeparatorsCollapsible(collapse);
}

bool QMenu_toolTipsVisible(QMenuH handle)
{
	return (bool) ((QMenu *)handle)->toolTipsVisible();
}

void QMenu_setToolTipsVisible(QMenuH handle, bool visible)
{
	((QMenu *)handle)->setToolTipsVisible(visible);
}

#if defined DARWIN
void QMenu_setAsDockMenu(QMenuH handle)
{
  ((QMenu *)handle)->setAsDockMenu();
}
#endif

void QMenu_showTearOffMenu(QMenuH handle, const QPointH pos)
{
  ((QMenu *)handle)->showTearOffMenu(*(const QPoint*)pos);
}

void QMenu_showTearOffMenu2(QMenuH handle)
{
  ((QMenu *)handle)->showTearOffMenu();
}

