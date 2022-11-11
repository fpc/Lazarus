//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qmetaobject_c.h"

QMetaMethodH QMetaMethod_Create()
{
	return (QMetaMethodH) new QMetaMethod();
}

void QMetaMethod_Destroy(QMetaMethodH handle)
{
	delete (QMetaMethod *)handle;
}

void QMetaMethod_methodSignature(QMetaMethodH handle, QByteArrayH retval)
{
	*(QByteArray *)retval = ((QMetaMethod *)handle)->methodSignature();
}

void QMetaMethod_name(QMetaMethodH handle, QByteArrayH retval)
{
	*(QByteArray *)retval = ((QMetaMethod *)handle)->name();
}

const char* QMetaMethod_typeName(QMetaMethodH handle)
{
	return (const char*) ((QMetaMethod *)handle)->typeName();
}

int QMetaMethod_returnType(QMetaMethodH handle)
{
	return (int) ((QMetaMethod *)handle)->returnType();
}

int QMetaMethod_parameterCount(QMetaMethodH handle)
{
	return (int) ((QMetaMethod *)handle)->parameterCount();
}

int QMetaMethod_parameterType(QMetaMethodH handle, int index)
{
	return (int) ((QMetaMethod *)handle)->parameterType(index);
}

void QMetaMethod_getParameterTypes(QMetaMethodH handle, int* types)
{
	((QMetaMethod *)handle)->getParameterTypes(types);
}

const char* QMetaMethod_tag(QMetaMethodH handle)
{
	return (const char*) ((QMetaMethod *)handle)->tag();
}

QMetaMethod::Access QMetaMethod_access(QMetaMethodH handle)
{
	return (QMetaMethod::Access) ((QMetaMethod *)handle)->access();
}

QMetaMethod::MethodType QMetaMethod_methodType(QMetaMethodH handle)
{
	return (QMetaMethod::MethodType) ((QMetaMethod *)handle)->methodType();
}

int QMetaMethod_attributes(QMetaMethodH handle)
{
	return (int) ((QMetaMethod *)handle)->attributes();
}

int QMetaMethod_methodIndex(QMetaMethodH handle)
{
	return (int) ((QMetaMethod *)handle)->methodIndex();
}

int QMetaMethod_revision(QMetaMethodH handle)
{
	return (int) ((QMetaMethod *)handle)->revision();
}

const QMetaObjectH QMetaMethod_enclosingMetaObject(QMetaMethodH handle)
{
	return (const QMetaObjectH) ((QMetaMethod *)handle)->enclosingMetaObject();
}

bool QMetaMethod_isValid(QMetaMethodH handle)
{
	return (bool) ((QMetaMethod *)handle)->isValid();
}

QMetaEnumH QMetaEnum_Create()
{
	return (QMetaEnumH) new QMetaEnum();
}

void QMetaEnum_Destroy(QMetaEnumH handle)
{
	delete (QMetaEnum *)handle;
}

const char* QMetaEnum_name(QMetaEnumH handle)
{
	return (const char*) ((QMetaEnum *)handle)->name();
}

bool QMetaEnum_isFlag(QMetaEnumH handle)
{
	return (bool) ((QMetaEnum *)handle)->isFlag();
}

int QMetaEnum_keyCount(QMetaEnumH handle)
{
	return (int) ((QMetaEnum *)handle)->keyCount();
}

const char* QMetaEnum_key(QMetaEnumH handle, int index)
{
	return (const char*) ((QMetaEnum *)handle)->key(index);
}

int QMetaEnum_value(QMetaEnumH handle, int index)
{
	return (int) ((QMetaEnum *)handle)->value(index);
}

const char* QMetaEnum_scope(QMetaEnumH handle)
{
	return (const char*) ((QMetaEnum *)handle)->scope();
}

int QMetaEnum_keyToValue(QMetaEnumH handle, const char* key, bool* ok)
{
	return (int) ((QMetaEnum *)handle)->keyToValue(key, ok);
}

const char* QMetaEnum_valueToKey(QMetaEnumH handle, int value)
{
	return (const char*) ((QMetaEnum *)handle)->valueToKey(value);
}

int QMetaEnum_keysToValue(QMetaEnumH handle, const char* keys, bool* ok)
{
	return (int) ((QMetaEnum *)handle)->keysToValue(keys, ok);
}

void QMetaEnum_valueToKeys(QMetaEnumH handle, QByteArrayH retval, int value)
{
	*(QByteArray *)retval = ((QMetaEnum *)handle)->valueToKeys(value);
}

const QMetaObjectH QMetaEnum_enclosingMetaObject(QMetaEnumH handle)
{
	return (const QMetaObjectH) ((QMetaEnum *)handle)->enclosingMetaObject();
}

bool QMetaEnum_isValid(QMetaEnumH handle)
{
	return (bool) ((QMetaEnum *)handle)->isValid();
}

QMetaPropertyH QMetaProperty_Create()
{
	return (QMetaPropertyH) new QMetaProperty();
}

void QMetaProperty_Destroy(QMetaPropertyH handle)
{
	delete (QMetaProperty *)handle;
}

const char* QMetaProperty_name(QMetaPropertyH handle)
{
	return (const char*) ((QMetaProperty *)handle)->name();
}

const char* QMetaProperty_typeName(QMetaPropertyH handle)
{
	return (const char*) ((QMetaProperty *)handle)->typeName();
}

QVariant::Type QMetaProperty_type(QMetaPropertyH handle)
{
	return (QVariant::Type) ((QMetaProperty *)handle)->type();
}

int QMetaProperty_userType(QMetaPropertyH handle)
{
	return (int) ((QMetaProperty *)handle)->userType();
}

int QMetaProperty_propertyIndex(QMetaPropertyH handle)
{
	return (int) ((QMetaProperty *)handle)->propertyIndex();
}

bool QMetaProperty_isReadable(QMetaPropertyH handle)
{
	return (bool) ((QMetaProperty *)handle)->isReadable();
}

bool QMetaProperty_isWritable(QMetaPropertyH handle)
{
	return (bool) ((QMetaProperty *)handle)->isWritable();
}

bool QMetaProperty_isResettable(QMetaPropertyH handle)
{
	return (bool) ((QMetaProperty *)handle)->isResettable();
}

bool QMetaProperty_isConstant(QMetaPropertyH handle)
{
	return (bool) ((QMetaProperty *)handle)->isConstant();
}

bool QMetaProperty_isFinal(QMetaPropertyH handle)
{
	return (bool) ((QMetaProperty *)handle)->isFinal();
}

bool QMetaProperty_isFlagType(QMetaPropertyH handle)
{
	return (bool) ((QMetaProperty *)handle)->isFlagType();
}

bool QMetaProperty_isEnumType(QMetaPropertyH handle)
{
	return (bool) ((QMetaProperty *)handle)->isEnumType();
}

void QMetaProperty_enumerator(QMetaPropertyH handle, QMetaEnumH retval)
{
	*(QMetaEnum *)retval = ((QMetaProperty *)handle)->enumerator();
}

bool QMetaProperty_hasNotifySignal(QMetaPropertyH handle)
{
	return (bool) ((QMetaProperty *)handle)->hasNotifySignal();
}

void QMetaProperty_notifySignal(QMetaPropertyH handle, QMetaMethodH retval)
{
	*(QMetaMethod *)retval = ((QMetaProperty *)handle)->notifySignal();
}

int QMetaProperty_notifySignalIndex(QMetaPropertyH handle)
{
	return (int) ((QMetaProperty *)handle)->notifySignalIndex();
}

int QMetaProperty_revision(QMetaPropertyH handle)
{
	return (int) ((QMetaProperty *)handle)->revision();
}

void QMetaProperty_read(QMetaPropertyH handle, QVariantH retval, const QObjectH obj)
{
	*(QVariant *)retval = ((QMetaProperty *)handle)->read((const QObject*)obj);
}

bool QMetaProperty_write(QMetaPropertyH handle, QObjectH obj, const QVariantH value)
{
	return (bool) ((QMetaProperty *)handle)->write((QObject*)obj, *(const QVariant*)value);
}

bool QMetaProperty_reset(QMetaPropertyH handle, QObjectH obj)
{
	return (bool) ((QMetaProperty *)handle)->reset((QObject*)obj);
}

bool QMetaProperty_hasStdCppSet(QMetaPropertyH handle)
{
	return (bool) ((QMetaProperty *)handle)->hasStdCppSet();
}

bool QMetaProperty_isValid(QMetaPropertyH handle)
{
	return (bool) ((QMetaProperty *)handle)->isValid();
}

const QMetaObjectH QMetaProperty_enclosingMetaObject(QMetaPropertyH handle)
{
	return (const QMetaObjectH) ((QMetaProperty *)handle)->enclosingMetaObject();
}

QMetaClassInfoH QMetaClassInfo_Create()
{
	return (QMetaClassInfoH) new QMetaClassInfo();
}

void QMetaClassInfo_Destroy(QMetaClassInfoH handle)
{
	delete (QMetaClassInfo *)handle;
}

const char* QMetaClassInfo_name(QMetaClassInfoH handle)
{
	return (const char*) ((QMetaClassInfo *)handle)->name();
}

const char* QMetaClassInfo_value(QMetaClassInfoH handle)
{
	return (const char*) ((QMetaClassInfo *)handle)->value();
}

const QMetaObjectH QMetaClassInfo_enclosingMetaObject(QMetaClassInfoH handle)
{
	return (const QMetaObjectH) ((QMetaClassInfo *)handle)->enclosingMetaObject();
}

