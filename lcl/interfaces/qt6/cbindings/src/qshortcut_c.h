//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QSHORTCUT_C_H
#define QSHORTCUT_C_H

#include <QtWidgets>
#include "pascalbind.h"

C_EXPORT QShortcutH QShortcut_Create(QWidgetH parent);
C_EXPORT void QShortcut_Destroy(QShortcutH handle);
C_EXPORT QShortcutH QShortcut_Create2(const QKeySequenceH key, QWidgetH parent, const char* member, const char* ambiguousMember, Qt::ShortcutContext context);
C_EXPORT void QShortcut_setKey(QShortcutH handle, const QKeySequenceH key);
C_EXPORT void QShortcut_key(QShortcutH handle, QKeySequenceH retval);
C_EXPORT void QShortcut_setEnabled(QShortcutH handle, bool enable);
C_EXPORT bool QShortcut_isEnabled(QShortcutH handle);
C_EXPORT void QShortcut_setContext(QShortcutH handle, Qt::ShortcutContext context);
C_EXPORT Qt::ShortcutContext QShortcut_context(QShortcutH handle);
C_EXPORT void QShortcut_setWhatsThis(QShortcutH handle, PWideString text);
C_EXPORT void QShortcut_whatsThis(QShortcutH handle, PWideString retval);
C_EXPORT void QShortcut_setAutoRepeat(QShortcutH handle, bool on);
C_EXPORT bool QShortcut_autoRepeat(QShortcutH handle);
C_EXPORT int QShortcut_id(QShortcutH handle);
C_EXPORT QWidgetH QShortcut_parentWidget(QShortcutH handle);

#endif
