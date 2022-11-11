//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qobjectdefs_c.h"

QGenericReturnArgumentH QGenericReturnArgument_Create(const char* aName, void* aData)
{
	return (QGenericReturnArgumentH) new QGenericReturnArgument(aName, aData);
}

void QGenericReturnArgument_Destroy(QGenericReturnArgumentH handle)
{
	delete (QGenericReturnArgument *)handle;
}

const char* QMetaObject_className(QMetaObjectH handle)
{
	return (const char*) ((QMetaObject *)handle)->className();
}

const QMetaObjectH QMetaObject_superClass(QMetaObjectH handle)
{
	return (const QMetaObjectH) ((QMetaObject *)handle)->superClass();
}

QObjectH QMetaObject_cast(QMetaObjectH handle, QObjectH obj)
{
	return (QObjectH) ((QMetaObject *)handle)->cast((QObject*)obj);
}

void QMetaObject_tr(QMetaObjectH handle, PWideString retval, const char* s, const char* c, int n)
{
	QString t_retval;
	t_retval = ((QMetaObject *)handle)->tr(s, c, n);
	copyQStringToPWideString(t_retval, retval);
}

int QMetaObject_methodOffset(QMetaObjectH handle)
{
	return (int) ((QMetaObject *)handle)->methodOffset();
}

int QMetaObject_enumeratorOffset(QMetaObjectH handle)
{
	return (int) ((QMetaObject *)handle)->enumeratorOffset();
}

int QMetaObject_propertyOffset(QMetaObjectH handle)
{
	return (int) ((QMetaObject *)handle)->propertyOffset();
}

int QMetaObject_classInfoOffset(QMetaObjectH handle)
{
	return (int) ((QMetaObject *)handle)->classInfoOffset();
}

int QMetaObject_constructorCount(QMetaObjectH handle)
{
	return (int) ((QMetaObject *)handle)->constructorCount();
}

int QMetaObject_methodCount(QMetaObjectH handle)
{
	return (int) ((QMetaObject *)handle)->methodCount();
}

int QMetaObject_enumeratorCount(QMetaObjectH handle)
{
	return (int) ((QMetaObject *)handle)->enumeratorCount();
}

int QMetaObject_propertyCount(QMetaObjectH handle)
{
	return (int) ((QMetaObject *)handle)->propertyCount();
}

int QMetaObject_classInfoCount(QMetaObjectH handle)
{
	return (int) ((QMetaObject *)handle)->classInfoCount();
}

int QMetaObject_indexOfConstructor(QMetaObjectH handle, const char* constructor)
{
	return (int) ((QMetaObject *)handle)->indexOfConstructor(constructor);
}

int QMetaObject_indexOfMethod(QMetaObjectH handle, const char* method)
{
	return (int) ((QMetaObject *)handle)->indexOfMethod(method);
}

int QMetaObject_indexOfSignal(QMetaObjectH handle, const char* signal)
{
	return (int) ((QMetaObject *)handle)->indexOfSignal(signal);
}

int QMetaObject_indexOfSlot(QMetaObjectH handle, const char* slot)
{
	return (int) ((QMetaObject *)handle)->indexOfSlot(slot);
}

int QMetaObject_indexOfEnumerator(QMetaObjectH handle, const char* name)
{
	return (int) ((QMetaObject *)handle)->indexOfEnumerator(name);
}

int QMetaObject_indexOfProperty(QMetaObjectH handle, const char* name)
{
	return (int) ((QMetaObject *)handle)->indexOfProperty(name);
}

int QMetaObject_indexOfClassInfo(QMetaObjectH handle, const char* name)
{
	return (int) ((QMetaObject *)handle)->indexOfClassInfo(name);
}

void QMetaObject_constructor(QMetaObjectH handle, QMetaMethodH retval, int index)
{
	*(QMetaMethod *)retval = ((QMetaObject *)handle)->constructor(index);
}

void QMetaObject_method(QMetaObjectH handle, QMetaMethodH retval, int index)
{
	*(QMetaMethod *)retval = ((QMetaObject *)handle)->method(index);
}

void QMetaObject_enumerator(QMetaObjectH handle, QMetaEnumH retval, int index)
{
	*(QMetaEnum *)retval = ((QMetaObject *)handle)->enumerator(index);
}

void QMetaObject_property(QMetaObjectH handle, QMetaPropertyH retval, int index)
{
	*(QMetaProperty *)retval = ((QMetaObject *)handle)->property(index);
}

void QMetaObject_classInfo(QMetaObjectH handle, QMetaClassInfoH retval, int index)
{
	*(QMetaClassInfo *)retval = ((QMetaObject *)handle)->classInfo(index);
}

void QMetaObject_userProperty(QMetaObjectH handle, QMetaPropertyH retval)
{
	*(QMetaProperty *)retval = ((QMetaObject *)handle)->userProperty();
}

bool QMetaObject_checkConnectArgs(const char* signal, const char* method)
{
	return (bool) QMetaObject::checkConnectArgs(signal, method);
}

bool QMetaObject_checkConnectArgs2(const QMetaMethodH signal, const QMetaMethodH method)
{
	return (bool) QMetaObject::checkConnectArgs(*(const QMetaMethod*)signal, *(const QMetaMethod*)method);
}

void QMetaObject_normalizedSignature(QByteArrayH retval, const char* method)
{
	*(QByteArray *)retval = QMetaObject::normalizedSignature(method);
}

void QMetaObject_normalizedType(QByteArrayH retval, const char* type)
{
	*(QByteArray *)retval = QMetaObject::normalizedType(type);
}

bool QMetaObject_disconnect(const QObjectH sender, int signal_index, const QObjectH receiver, int method_index)
{
	return (bool) QMetaObject::disconnect((const QObject*)sender, signal_index, (const QObject*)receiver, method_index);
}

bool QMetaObject_disconnectOne(const QObjectH sender, int signal_index, const QObjectH receiver, int method_index)
{
	return (bool) QMetaObject::disconnectOne((const QObject*)sender, signal_index, (const QObject*)receiver, method_index);
}

void QMetaObject_connectSlotsByName(QObjectH o)
{
	QMetaObject::connectSlotsByName((QObject*)o);
}

void QMetaObject_activate(QObjectH sender, int signal_index, void** argv)
{
	QMetaObject::activate((QObject*)sender, signal_index, argv);
}

void QMetaObject_activate2(QObjectH sender, const QMetaObjectH AnonParam2, int local_signal_index, void** argv)
{
	QMetaObject::activate((QObject*)sender, (const QMetaObject*)AnonParam2, local_signal_index, argv);
}

void QMetaObject_activate3(QObjectH sender, int signal_offset, int local_signal_index, void** argv)
{
	QMetaObject::activate((QObject*)sender, signal_offset, local_signal_index, argv);
}

int QMetaObject_static_metacall(QMetaObjectH handle, QMetaObject::Call AnonParam1, int AnonParam2, void** AnonParam3)
{
	return (int) ((QMetaObject *)handle)->static_metacall(AnonParam1, AnonParam2, AnonParam3);
}

int QMetaObject_metacall(QObjectH AnonParam1, QMetaObject::Call AnonParam2, int AnonParam3, void** AnonParam4)
{
	return (int) QMetaObject::metacall((QObject*)AnonParam1, AnonParam2, AnonParam3, AnonParam4);
}

