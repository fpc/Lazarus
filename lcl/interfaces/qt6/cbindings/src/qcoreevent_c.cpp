//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qcoreevent_c.h"

QEventH QEvent_Create(QEvent::Type type)
{
	return (QEventH) new QEvent(type);
}

void QEvent_Destroy(QEventH handle)
{
	delete (QEvent *)handle;
}

QEvent::Type QEvent_type(QEventH handle)
{
	return (QEvent::Type) ((QEvent *)handle)->type();
}

bool QEvent_spontaneous(QEventH handle)
{
	return (bool) ((QEvent *)handle)->spontaneous();
}

void QEvent_setAccepted(QEventH handle, bool accepted)
{
	((QEvent *)handle)->setAccepted(accepted);
}

bool QEvent_isAccepted(QEventH handle)
{
	return (bool) ((QEvent *)handle)->isAccepted();
}

void QEvent_accept(QEventH handle)
{
	((QEvent *)handle)->accept();
}

void QEvent_ignore(QEventH handle)
{
	((QEvent *)handle)->ignore();
}

bool QEvent_isInputEvent(QEventH handle)
{
  return (bool) ((QEvent *)handle)->isInputEvent();
}

bool QEvent_isPointerEvent(QEventH handle)
{
  return (bool) ((QEvent *)handle)->isPointerEvent();
}

bool QEvent_isSinglePointEvent(QEventH handle)
{
  return (bool) ((QEvent *)handle)->isSinglePointEvent();
}


int QEvent_registerEventType(int hint)
{
	return (int) QEvent::registerEventType(hint);
}

QTimerEventH QTimerEvent_Create(int timerId)
{
	return (QTimerEventH) new QTimerEvent(timerId);
}

void QTimerEvent_Destroy(QTimerEventH handle)
{
	delete (QTimerEvent *)handle;
}

int QTimerEvent_timerId(QTimerEventH handle)
{
	return (int) ((QTimerEvent *)handle)->timerId();
}

QChildEventH QChildEvent_Create(QEvent::Type type, QObjectH child)
{
	return (QChildEventH) new QChildEvent(type, (QObject*)child);
}

void QChildEvent_Destroy(QChildEventH handle)
{
	delete (QChildEvent *)handle;
}

QObjectH QChildEvent_child(QChildEventH handle)
{
	return (QObjectH) ((QChildEvent *)handle)->child();
}

bool QChildEvent_added(QChildEventH handle)
{
	return (bool) ((QChildEvent *)handle)->added();
}

bool QChildEvent_polished(QChildEventH handle)
{
	return (bool) ((QChildEvent *)handle)->polished();
}

bool QChildEvent_removed(QChildEventH handle)
{
	return (bool) ((QChildEvent *)handle)->removed();
}

QDynamicPropertyChangeEventH QDynamicPropertyChangeEvent_Create(const QByteArrayH name)
{
	return (QDynamicPropertyChangeEventH) new QDynamicPropertyChangeEvent(*(const QByteArray*)name);
}

void QDynamicPropertyChangeEvent_Destroy(QDynamicPropertyChangeEventH handle)
{
	delete (QDynamicPropertyChangeEvent *)handle;
}

void QDynamicPropertyChangeEvent_propertyName(QDynamicPropertyChangeEventH handle, QByteArrayH retval)
{
	*(QByteArray *)retval = ((QDynamicPropertyChangeEvent *)handle)->propertyName();
}

