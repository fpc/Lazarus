//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QACTION_C_H
#define QACTION_C_H

#include <QtWidgets>
#include "pascalbind.h"

C_EXPORT QActionH QAction_Create(QObjectH parent);
C_EXPORT void QAction_Destroy(QActionH handle);
C_EXPORT QActionH QAction_Create2(PWideString text, QObjectH parent);
C_EXPORT QActionH QAction_Create3(const QIconH icon, PWideString text, QObjectH parent);
C_EXPORT void QAction_setActionGroup(QActionH handle, QActionGroupH group);
C_EXPORT QActionGroupH QAction_actionGroup(QActionH handle);
C_EXPORT void QAction_setIcon(QActionH handle, const QIconH icon);
C_EXPORT void QAction_icon(QActionH handle, QIconH retval);
C_EXPORT void QAction_setText(QActionH handle, PWideString text);
C_EXPORT void QAction_text(QActionH handle, PWideString retval);
C_EXPORT void QAction_setIconText(QActionH handle, PWideString text);
C_EXPORT void QAction_iconText(QActionH handle, PWideString retval);
C_EXPORT void QAction_setToolTip(QActionH handle, PWideString tip);
C_EXPORT void QAction_toolTip(QActionH handle, PWideString retval);
C_EXPORT void QAction_setStatusTip(QActionH handle, PWideString statusTip);
C_EXPORT void QAction_statusTip(QActionH handle, PWideString retval);
C_EXPORT void QAction_setWhatsThis(QActionH handle, PWideString what);
C_EXPORT void QAction_whatsThis(QActionH handle, PWideString retval);
C_EXPORT void QAction_setPriority(QActionH handle, QAction::Priority priority);
C_EXPORT QAction::Priority QAction_priority(QActionH handle);
C_EXPORT QMenuH QAction_menu(QActionH handle);
C_EXPORT void QAction_setMenu(QActionH handle, QMenuH menu);
C_EXPORT void QAction_setSeparator(QActionH handle, bool b);
C_EXPORT bool QAction_isSeparator(QActionH handle);
C_EXPORT void QAction_setShortcut(QActionH handle, const QKeySequenceH shortcut);
C_EXPORT void QAction_shortcut(QActionH handle, QKeySequenceH retval);
C_EXPORT void QAction_setShortcutContext(QActionH handle, Qt::ShortcutContext context);
C_EXPORT Qt::ShortcutContext QAction_shortcutContext(QActionH handle);
C_EXPORT void QAction_setAutoRepeat(QActionH handle, bool AnonParam1);
C_EXPORT bool QAction_autoRepeat(QActionH handle);
C_EXPORT void QAction_setFont(QActionH handle, const QFontH font);
C_EXPORT void QAction_font(QActionH handle, QFontH retval);
C_EXPORT void QAction_setCheckable(QActionH handle, bool AnonParam1);
C_EXPORT bool QAction_isCheckable(QActionH handle);
C_EXPORT void QAction_data(QActionH handle, QVariantH retval);
C_EXPORT void QAction_setData(QActionH handle, const QVariantH var);
C_EXPORT bool QAction_isChecked(QActionH handle);
C_EXPORT bool QAction_isEnabled(QActionH handle);
C_EXPORT bool QAction_isVisible(QActionH handle);
C_EXPORT void QAction_activate(QActionH handle, QAction::ActionEvent event);
C_EXPORT bool QAction_showStatusText(QActionH handle, QWidgetH widget);
C_EXPORT void QAction_setMenuRole(QActionH handle, QAction::MenuRole menuRole);
C_EXPORT QAction::MenuRole QAction_menuRole(QActionH handle);
C_EXPORT void QAction_setIconVisibleInMenu(QActionH handle, bool visible);
C_EXPORT bool QAction_isIconVisibleInMenu(QActionH handle);
C_EXPORT QWidgetH QAction_parentWidget(QActionH handle);
C_EXPORT void QAction_associatedWidgets(QActionH handle, PPtrIntArray retval);
C_EXPORT void QAction_associatedGraphicsWidgets(QActionH handle, PPtrIntArray retval);
C_EXPORT void QAction_trigger(QActionH handle);
C_EXPORT void QAction_hover(QActionH handle);
C_EXPORT void QAction_setChecked(QActionH handle, bool AnonParam1);
C_EXPORT void QAction_toggle(QActionH handle);
C_EXPORT void QAction_setEnabled(QActionH handle, bool AnonParam1);
C_EXPORT void QAction_setDisabled(QActionH handle, bool b);
C_EXPORT void QAction_setVisible(QActionH handle, bool AnonParam1);

#endif
