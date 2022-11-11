//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QSYSTEMTRAYICON_C_H
#define QSYSTEMTRAYICON_C_H

#include <QtWidgets>
#include "pascalbind.h"

C_EXPORT QSystemTrayIconH QSystemTrayIcon_Create(QObjectH parent);
C_EXPORT void QSystemTrayIcon_Destroy(QSystemTrayIconH handle);
C_EXPORT QSystemTrayIconH QSystemTrayIcon_Create2(const QIconH icon, QObjectH parent);
C_EXPORT void QSystemTrayIcon_setContextMenu(QSystemTrayIconH handle, QMenuH menu);
C_EXPORT QMenuH QSystemTrayIcon_contextMenu(QSystemTrayIconH handle);
C_EXPORT void QSystemTrayIcon_icon(QSystemTrayIconH handle, QIconH retval);
C_EXPORT void QSystemTrayIcon_setIcon(QSystemTrayIconH handle, const QIconH icon);
C_EXPORT void QSystemTrayIcon_toolTip(QSystemTrayIconH handle, PWideString retval);
C_EXPORT void QSystemTrayIcon_setToolTip(QSystemTrayIconH handle, PWideString tip);
C_EXPORT void QSystemTrayIcon_showMessage(QSystemTrayIconH handle, PWideString title, PWideString msg, QSystemTrayIcon::MessageIcon icon, int msecs);
C_EXPORT void QSystemTrayIcon_showMessage2(QSystemTrayIconH handle, PWideString title, PWideString msg, const QIconH icon, int msecs);
C_EXPORT void QSystemTrayIcon_geometry(QSystemTrayIconH handle, PRect retval);
C_EXPORT bool QSystemTrayIcon_isVisible(QSystemTrayIconH handle);
C_EXPORT void QSystemTrayIcon_setVisible(QSystemTrayIconH handle, bool visible);
C_EXPORT void QSystemTrayIcon_show(QSystemTrayIconH handle);
C_EXPORT void QSystemTrayIcon_hide(QSystemTrayIconH handle);

C_EXPORT bool QSystemTrayIcon_isSystemTrayAvailable();
C_EXPORT bool QSystemTrayIcon_supportsMessages();

#endif
