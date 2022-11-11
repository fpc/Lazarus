//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QCOREEVENT_C_H
#define QCOREEVENT_C_H

#include <QtCore>
#include "pascalbind.h"

C_EXPORT QEventH QEvent_Create(QEvent::Type type);
C_EXPORT void QEvent_Destroy(QEventH handle);
C_EXPORT QEvent::Type QEvent_type(QEventH handle);
C_EXPORT bool QEvent_spontaneous(QEventH handle);
C_EXPORT void QEvent_setAccepted(QEventH handle, bool accepted);
C_EXPORT bool QEvent_isAccepted(QEventH handle);
C_EXPORT void QEvent_accept(QEventH handle);
C_EXPORT void QEvent_ignore(QEventH handle);
C_EXPORT int QEvent_registerEventType(int hint);
C_EXPORT bool QEvent_isInputEvent(QEventH handle);
C_EXPORT bool QEvent_isPointerEvent(QEventH handle);
C_EXPORT bool QEvent_isSinglePointEvent(QEventH handle);

C_EXPORT QTimerEventH QTimerEvent_Create(int timerId);
C_EXPORT void QTimerEvent_Destroy(QTimerEventH handle);
C_EXPORT int QTimerEvent_timerId(QTimerEventH handle);
C_EXPORT QChildEventH QChildEvent_Create(QEvent::Type type, QObjectH child);
C_EXPORT void QChildEvent_Destroy(QChildEventH handle);
C_EXPORT QObjectH QChildEvent_child(QChildEventH handle);
C_EXPORT bool QChildEvent_added(QChildEventH handle);
C_EXPORT bool QChildEvent_polished(QChildEventH handle);
C_EXPORT bool QChildEvent_removed(QChildEventH handle);
C_EXPORT QDynamicPropertyChangeEventH QDynamicPropertyChangeEvent_Create(const QByteArrayH name);
C_EXPORT void QDynamicPropertyChangeEvent_Destroy(QDynamicPropertyChangeEventH handle);
C_EXPORT void QDynamicPropertyChangeEvent_propertyName(QDynamicPropertyChangeEventH handle, QByteArrayH retval);

#endif
