//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QOBJECT_C_H
#define QOBJECT_C_H

#include <QtCore>
#include "pascalbind.h"

C_EXPORT QObjectH QObject_Create(QObjectH parent);
C_EXPORT void QObject_Destroy(QObjectH handle);
C_EXPORT bool QObject_event(QObjectH handle, QEventH AnonParam1);
C_EXPORT bool QObject_eventFilter(QObjectH handle, QObjectH AnonParam1, QEventH AnonParam2);
C_EXPORT void QObject_objectName(QObjectH handle, PWideString retval);
C_EXPORT void QObject_setObjectName(QObjectH handle, PWideString name);
C_EXPORT bool QObject_isWidgetType(QObjectH handle);
C_EXPORT bool QObject_isWindowType(QObjectH handle);
C_EXPORT bool QObject_signalsBlocked(QObjectH handle);
C_EXPORT bool QObject_blockSignals(QObjectH handle, bool b);
C_EXPORT QThreadH QObject_thread(QObjectH handle);
C_EXPORT void QObject_moveToThread(QObjectH handle, QThreadH thread);
C_EXPORT int QObject_startTimer(QObjectH handle, int interval, Qt::TimerType timerType);
C_EXPORT void QObject_killTimer(QObjectH handle, int id);
C_EXPORT void QObject_children(QObjectH handle, PPtrIntArray retval);
C_EXPORT void QObject_setParent(QObjectH handle, QObjectH AnonParam1);
C_EXPORT void QObject_installEventFilter(QObjectH handle, QObjectH AnonParam1);
C_EXPORT void QObject_removeEventFilter(QObjectH handle, QObjectH AnonParam1);
C_EXPORT void QObject_connect(const QObjectH sender, const char* signal, const QObjectH receiver, const char* member, Qt::ConnectionType AnonParam5);
C_EXPORT void QObject_connect2(const QObjectH sender, const QMetaMethodH signal, const QObjectH receiver, const QMetaMethodH method, Qt::ConnectionType type);
C_EXPORT void QObject_connect3(QObjectH handle, const QObjectH sender, const char* signal, const char* member, Qt::ConnectionType type);
C_EXPORT bool QObject_disconnect(const QObjectH sender, const char* signal, const QObjectH receiver, const char* member);
C_EXPORT bool QObject_disconnect2(const QObjectH sender, const QMetaMethodH signal, const QObjectH receiver, const QMetaMethodH member);
C_EXPORT bool QObject_disconnect4(QObjectH handle, const QObjectH receiver, const char* member);
C_EXPORT void QObject_dumpObjectTree(QObjectH handle);
C_EXPORT void QObject_dumpObjectInfo(QObjectH handle);
C_EXPORT bool QObject_setProperty(QObjectH handle, const char* name, const QVariantH value);
C_EXPORT void QObject_property(QObjectH handle, QVariantH retval, const char* name);
C_EXPORT QObjectH QObject_parent(QObjectH handle);
C_EXPORT bool QObject_inherits(QObjectH handle, const char* classname);
C_EXPORT void QObject_deleteLater(QObjectH handle);
C_EXPORT const QMetaObjectH QObject_metaObject(QObjectH handle);

#endif
