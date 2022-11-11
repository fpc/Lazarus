//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QMENU_C_H
#define QMENU_C_H

#include <QtWidgets>
#include "pascalbind.h"

C_EXPORT QMenuH QMenu_Create(QWidgetH parent);
C_EXPORT void QMenu_Destroy(QMenuH handle);
C_EXPORT QMenuH QMenu_Create2(PWideString title, QWidgetH parent);
C_EXPORT QActionH QMenu_addAction(QMenuH handle, PWideString text);
C_EXPORT QActionH QMenu_addAction2(QMenuH handle, const QIconH icon, PWideString text);
C_EXPORT QActionH QMenu_addAction3(QMenuH handle, PWideString text, const QObjectH receiver, const char* member, const QKeySequenceH shortcut);
C_EXPORT QActionH QMenu_addAction4(QMenuH handle, const QIconH icon, PWideString text, const QObjectH receiver, const char* member, const QKeySequenceH shortcut);
C_EXPORT QActionH QMenu_addMenu(QMenuH handle, QMenuH menu);
C_EXPORT QMenuH QMenu_addMenu2(QMenuH handle, PWideString title);
C_EXPORT QMenuH QMenu_addMenu3(QMenuH handle, const QIconH icon, PWideString title);
C_EXPORT QActionH QMenu_addSeparator(QMenuH handle);
C_EXPORT QActionH QMenu_addSection(QMenuH handle, PWideString text);
C_EXPORT QActionH QMenu_addSection2(QMenuH handle, const QIconH icon, PWideString text);
C_EXPORT QActionH QMenu_insertMenu(QMenuH handle, QActionH before, QMenuH menu);
C_EXPORT QActionH QMenu_insertSeparator(QMenuH handle, QActionH before);
C_EXPORT QActionH QMenu_insertSection(QMenuH handle, QActionH before, PWideString text);
C_EXPORT QActionH QMenu_insertSection2(QMenuH handle, QActionH before, const QIconH icon, PWideString text);
C_EXPORT bool QMenu_isEmpty(QMenuH handle);
C_EXPORT void QMenu_clear(QMenuH handle);
C_EXPORT void QMenu_setTearOffEnabled(QMenuH handle, bool AnonParam1);
C_EXPORT bool QMenu_isTearOffEnabled(QMenuH handle);
C_EXPORT bool QMenu_isTearOffMenuVisible(QMenuH handle);
C_EXPORT void QMenu_hideTearOffMenu(QMenuH handle);
C_EXPORT void QMenu_setDefaultAction(QMenuH handle, QActionH AnonParam1);
C_EXPORT QActionH QMenu_defaultAction(QMenuH handle);
C_EXPORT void QMenu_setActiveAction(QMenuH handle, QActionH act);
C_EXPORT QActionH QMenu_activeAction(QMenuH handle);
C_EXPORT void QMenu_popup(QMenuH handle, const QPointH pos, QActionH at);
C_EXPORT QActionH QMenu_exec(QMenuH handle);
C_EXPORT QActionH QMenu_exec2(QMenuH handle, const QPointH pos, QActionH at);
C_EXPORT QActionH QMenu_exec3(PPtrIntArray actions, const QPointH pos, QActionH at, QWidgetH parent);
C_EXPORT void QMenu_sizeHint(QMenuH handle, PSize retval);
C_EXPORT void QMenu_actionGeometry(QMenuH handle, PRect retval, QActionH AnonParam1);
C_EXPORT QActionH QMenu_actionAt(QMenuH handle, const QPointH AnonParam1);
C_EXPORT QActionH QMenu_menuAction(QMenuH handle);
C_EXPORT void QMenu_title(QMenuH handle, PWideString retval);
C_EXPORT void QMenu_setTitle(QMenuH handle, PWideString title);
C_EXPORT void QMenu_icon(QMenuH handle, QIconH retval);
C_EXPORT void QMenu_setIcon(QMenuH handle, const QIconH icon);
C_EXPORT void QMenu_setNoReplayFor(QMenuH handle, QWidgetH widget);
C_EXPORT bool QMenu_separatorsCollapsible(QMenuH handle);
C_EXPORT void QMenu_setSeparatorsCollapsible(QMenuH handle, bool collapse);
C_EXPORT bool QMenu_toolTipsVisible(QMenuH handle);
C_EXPORT void QMenu_setToolTipsVisible(QMenuH handle, bool visible);
#if defined DARWIN
C_EXPORT void QMenu_setAsDockMenu(QMenuH handle);
#endif
C_EXPORT void QMenu_showTearOffMenu(QMenuH handle, const QPointH pos);
C_EXPORT void QMenu_showTearOffMenu2(QMenuH handle);

#endif
