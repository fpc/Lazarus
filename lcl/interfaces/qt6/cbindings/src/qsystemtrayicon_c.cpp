//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qsystemtrayicon_c.h"

QSystemTrayIconH QSystemTrayIcon_Create(QObjectH parent)
{
	return (QSystemTrayIconH) new QSystemTrayIcon((QObject*)parent);
}

void QSystemTrayIcon_Destroy(QSystemTrayIconH handle)
{
	delete (QSystemTrayIcon *)handle;
}

QSystemTrayIconH QSystemTrayIcon_Create2(const QIconH icon, QObjectH parent)
{
	return (QSystemTrayIconH) new QSystemTrayIcon(*(const QIcon*)icon, (QObject*)parent);
}

void QSystemTrayIcon_setContextMenu(QSystemTrayIconH handle, QMenuH menu)
{
	((QSystemTrayIcon *)handle)->setContextMenu((QMenu*)menu);
}

QMenuH QSystemTrayIcon_contextMenu(QSystemTrayIconH handle)
{
	return (QMenuH) ((QSystemTrayIcon *)handle)->contextMenu();
}

void QSystemTrayIcon_icon(QSystemTrayIconH handle, QIconH retval)
{
	*(QIcon *)retval = ((QSystemTrayIcon *)handle)->icon();
}

void QSystemTrayIcon_setIcon(QSystemTrayIconH handle, const QIconH icon)
{
	((QSystemTrayIcon *)handle)->setIcon(*(const QIcon*)icon);
}

void QSystemTrayIcon_toolTip(QSystemTrayIconH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QSystemTrayIcon *)handle)->toolTip();
	copyQStringToPWideString(t_retval, retval);
}

void QSystemTrayIcon_setToolTip(QSystemTrayIconH handle, PWideString tip)
{
	QString t_tip;
	copyPWideStringToQString(tip, t_tip);
	((QSystemTrayIcon *)handle)->setToolTip(t_tip);
}

bool QSystemTrayIcon_isSystemTrayAvailable()
{
	return (bool) QSystemTrayIcon::isSystemTrayAvailable();
}

bool QSystemTrayIcon_supportsMessages()
{
	return (bool) QSystemTrayIcon::supportsMessages();
}

void QSystemTrayIcon_showMessage(QSystemTrayIconH handle, PWideString title, PWideString msg, QSystemTrayIcon::MessageIcon icon, int msecs)
{
	QString t_title;
	QString t_msg;
	copyPWideStringToQString(title, t_title);
	copyPWideStringToQString(msg, t_msg);
	((QSystemTrayIcon *)handle)->showMessage(t_title, t_msg, icon, msecs);
}

void QSystemTrayIcon_showMessage(QSystemTrayIconH handle, PWideString title, PWideString msg, const QIconH icon, int msecs)
{
	QString t_title;
	QString t_msg;
	copyPWideStringToQString(title, t_title);
	copyPWideStringToQString(msg, t_msg);
	((QSystemTrayIcon *)handle)->showMessage(t_title, t_msg, *(const QIcon*)icon, msecs);
}

void QSystemTrayIcon_geometry(QSystemTrayIconH handle, PRect retval)
{
	QRect t_retval;
	t_retval = ((QSystemTrayIcon *)handle)->geometry();
	copyQRectToPRect(t_retval, retval);
}

bool QSystemTrayIcon_isVisible(QSystemTrayIconH handle)
{
	return (bool) ((QSystemTrayIcon *)handle)->isVisible();
}

void QSystemTrayIcon_setVisible(QSystemTrayIconH handle, bool visible)
{
	((QSystemTrayIcon *)handle)->setVisible(visible);
}

void QSystemTrayIcon_show(QSystemTrayIconH handle)
{
	((QSystemTrayIcon *)handle)->show();
}

void QSystemTrayIcon_hide(QSystemTrayIconH handle)
{
	((QSystemTrayIcon *)handle)->hide();
}

