//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qobject_c.h"

QObjectH QObject_Create(QObjectH parent)
{
	return (QObjectH) new QObject((QObject*)parent);
}

void QObject_Destroy(QObjectH handle)
{
	delete (QObject *)handle;
}

bool QObject_event(QObjectH handle, QEventH AnonParam1)
{
	return (bool) ((QObject *)handle)->event((QEvent*)AnonParam1);
}

bool QObject_eventFilter(QObjectH handle, QObjectH AnonParam1, QEventH AnonParam2)
{
	return (bool) ((QObject *)handle)->eventFilter((QObject*)AnonParam1, (QEvent*)AnonParam2);
}

void QObject_objectName(QObjectH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QObject *)handle)->objectName();
	copyQStringToPWideString(t_retval, retval);
}

void QObject_setObjectName(QObjectH handle, PWideString name)
{
	QString t_name;
	copyPWideStringToQString(name, t_name);
	((QObject *)handle)->setObjectName(t_name);
}

bool QObject_isWidgetType(QObjectH handle)
{
	return (bool) ((QObject *)handle)->isWidgetType();
}

bool QObject_isWindowType(QObjectH handle)
{
	return (bool) ((QObject *)handle)->isWindowType();
}

bool QObject_signalsBlocked(QObjectH handle)
{
	return (bool) ((QObject *)handle)->signalsBlocked();
}

bool QObject_blockSignals(QObjectH handle, bool b)
{
	return (bool) ((QObject *)handle)->blockSignals(b);
}

QThreadH QObject_thread(QObjectH handle)
{
	return (QThreadH) ((QObject *)handle)->thread();
}

void QObject_moveToThread(QObjectH handle, QThreadH thread)
{
	((QObject *)handle)->moveToThread((QThread*)thread);
}

int QObject_startTimer(QObjectH handle, int interval, Qt::TimerType timerType)
{
	return (int) ((QObject *)handle)->startTimer(interval, timerType);
}

void QObject_killTimer(QObjectH handle, int id)
{
	((QObject *)handle)->killTimer(id);
}

void QObject_children(QObjectH handle, PPtrIntArray retval)
{
	QObjectList t_retval;
	t_retval = ((QObject *)handle)->children();
	copyQListTemplateToPtrIntArray(t_retval, retval);
}

void QObject_setParent(QObjectH handle, QObjectH AnonParam1)
{
	((QObject *)handle)->setParent((QObject*)AnonParam1);
}

void QObject_installEventFilter(QObjectH handle, QObjectH AnonParam1)
{
	((QObject *)handle)->installEventFilter((QObject*)AnonParam1);
}

void QObject_removeEventFilter(QObjectH handle, QObjectH AnonParam1)
{
	((QObject *)handle)->removeEventFilter((QObject*)AnonParam1);
}

void QObject_connect(const QObjectH sender, const char* signal, const QObjectH receiver, const char* member, Qt::ConnectionType AnonParam5)
{
	QObject::connect((const QObject*)sender, signal, (const QObject*)receiver, member, AnonParam5);
}

void QObject_connect2(const QObjectH sender, const QMetaMethodH signal, const QObjectH receiver, const QMetaMethodH method, Qt::ConnectionType type)
{
	QObject::connect((const QObject*)sender, *(const QMetaMethod*)signal, (const QObject*)receiver, *(const QMetaMethod*)method, type);
}

void QObject_connect3(QObjectH handle, const QObjectH sender, const char* signal, const char* member, Qt::ConnectionType type)
{
	((QObject *)handle)->connect((const QObject*)sender, signal, member, type);
}

bool QObject_disconnect(const QObjectH sender, const char* signal, const QObjectH receiver, const char* member)
{
	return (bool) QObject::disconnect((const QObject*)sender, signal, (const QObject*)receiver, member);
}

bool QObject_disconnect2(const QObjectH sender, const QMetaMethodH signal, const QObjectH receiver, const QMetaMethodH member)
{
	return (bool) QObject::disconnect((const QObject*)sender, *(const QMetaMethod*)signal, (const QObject*)receiver, *(const QMetaMethod*)member);
}

bool QObject_disconnect4(QObjectH handle, const QObjectH receiver, const char* member)
{
	return (bool) ((QObject *)handle)->disconnect((const QObject*)receiver, member);
}

void QObject_dumpObjectTree(QObjectH handle)
{
	((QObject *)handle)->dumpObjectTree();
}

void QObject_dumpObjectInfo(QObjectH handle)
{
	((QObject *)handle)->dumpObjectInfo();
}

bool QObject_setProperty(QObjectH handle, const char* name, const QVariantH value)
{
	return (bool) ((QObject *)handle)->setProperty(name, *(const QVariant*)value);
}

void QObject_property(QObjectH handle, QVariantH retval, const char* name)
{
	*(QVariant *)retval = ((QObject *)handle)->property(name);
}

QObjectH QObject_parent(QObjectH handle)
{
	return (QObjectH) ((QObject *)handle)->parent();
}

bool QObject_inherits(QObjectH handle, const char* classname)
{
	return (bool) ((QObject *)handle)->inherits(classname);
}

void QObject_deleteLater(QObjectH handle)
{
	((QObject *)handle)->deleteLater();
}

const QMetaObjectH QObject_metaObject(QObjectH handle)
{
  const QMetaObject *meta = ((QObject *)handle)->metaObject();
  if (!meta)
    return NULL;
  return (const QMetaObjectH) meta;
}
