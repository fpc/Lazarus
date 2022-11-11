//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QMETAOBJECT_C_H
#define QMETAOBJECT_C_H

#include <QtCore>
#include "pascalbind.h"

C_EXPORT QMetaMethodH QMetaMethod_Create();
C_EXPORT void QMetaMethod_Destroy(QMetaMethodH handle);
C_EXPORT void QMetaMethod_methodSignature(QMetaMethodH handle, QByteArrayH retval);
C_EXPORT void QMetaMethod_name(QMetaMethodH handle, QByteArrayH retval);
C_EXPORT const char* QMetaMethod_typeName(QMetaMethodH handle);
C_EXPORT int QMetaMethod_returnType(QMetaMethodH handle);
C_EXPORT int QMetaMethod_parameterCount(QMetaMethodH handle);
C_EXPORT int QMetaMethod_parameterType(QMetaMethodH handle, int index);
C_EXPORT void QMetaMethod_getParameterTypes(QMetaMethodH handle, int* types);
C_EXPORT const char* QMetaMethod_tag(QMetaMethodH handle);
C_EXPORT QMetaMethod::Access QMetaMethod_access(QMetaMethodH handle);
C_EXPORT QMetaMethod::MethodType QMetaMethod_methodType(QMetaMethodH handle);
C_EXPORT int QMetaMethod_attributes(QMetaMethodH handle);
C_EXPORT int QMetaMethod_methodIndex(QMetaMethodH handle);
C_EXPORT int QMetaMethod_revision(QMetaMethodH handle);
C_EXPORT const QMetaObjectH QMetaMethod_enclosingMetaObject(QMetaMethodH handle);
C_EXPORT bool QMetaMethod_isValid(QMetaMethodH handle);
C_EXPORT QMetaEnumH QMetaEnum_Create();
C_EXPORT void QMetaEnum_Destroy(QMetaEnumH handle);
C_EXPORT const char* QMetaEnum_name(QMetaEnumH handle);
C_EXPORT bool QMetaEnum_isFlag(QMetaEnumH handle);
C_EXPORT int QMetaEnum_keyCount(QMetaEnumH handle);
C_EXPORT const char* QMetaEnum_key(QMetaEnumH handle, int index);
C_EXPORT int QMetaEnum_value(QMetaEnumH handle, int index);
C_EXPORT const char* QMetaEnum_scope(QMetaEnumH handle);
C_EXPORT int QMetaEnum_keyToValue(QMetaEnumH handle, const char* key, bool* ok);
C_EXPORT const char* QMetaEnum_valueToKey(QMetaEnumH handle, int value);
C_EXPORT int QMetaEnum_keysToValue(QMetaEnumH handle, const char* keys, bool* ok);
C_EXPORT void QMetaEnum_valueToKeys(QMetaEnumH handle, QByteArrayH retval, int value);
C_EXPORT const QMetaObjectH QMetaEnum_enclosingMetaObject(QMetaEnumH handle);
C_EXPORT bool QMetaEnum_isValid(QMetaEnumH handle);
C_EXPORT QMetaPropertyH QMetaProperty_Create();
C_EXPORT void QMetaProperty_Destroy(QMetaPropertyH handle);
C_EXPORT const char* QMetaProperty_name(QMetaPropertyH handle);
C_EXPORT const char* QMetaProperty_typeName(QMetaPropertyH handle);
C_EXPORT QVariant::Type QMetaProperty_type(QMetaPropertyH handle);
C_EXPORT int QMetaProperty_userType(QMetaPropertyH handle);
C_EXPORT int QMetaProperty_propertyIndex(QMetaPropertyH handle);
C_EXPORT bool QMetaProperty_isReadable(QMetaPropertyH handle);
C_EXPORT bool QMetaProperty_isWritable(QMetaPropertyH handle);
C_EXPORT bool QMetaProperty_isResettable(QMetaPropertyH handle);
C_EXPORT bool QMetaProperty_isConstant(QMetaPropertyH handle);
C_EXPORT bool QMetaProperty_isFinal(QMetaPropertyH handle);
C_EXPORT bool QMetaProperty_isFlagType(QMetaPropertyH handle);
C_EXPORT bool QMetaProperty_isEnumType(QMetaPropertyH handle);
C_EXPORT void QMetaProperty_enumerator(QMetaPropertyH handle, QMetaEnumH retval);
C_EXPORT bool QMetaProperty_hasNotifySignal(QMetaPropertyH handle);
C_EXPORT void QMetaProperty_notifySignal(QMetaPropertyH handle, QMetaMethodH retval);
C_EXPORT int QMetaProperty_notifySignalIndex(QMetaPropertyH handle);
C_EXPORT int QMetaProperty_revision(QMetaPropertyH handle);
C_EXPORT void QMetaProperty_read(QMetaPropertyH handle, QVariantH retval, const QObjectH obj);
C_EXPORT bool QMetaProperty_write(QMetaPropertyH handle, QObjectH obj, const QVariantH value);
C_EXPORT bool QMetaProperty_reset(QMetaPropertyH handle, QObjectH obj);
C_EXPORT bool QMetaProperty_hasStdCppSet(QMetaPropertyH handle);
C_EXPORT bool QMetaProperty_isValid(QMetaPropertyH handle);
C_EXPORT const QMetaObjectH QMetaProperty_enclosingMetaObject(QMetaPropertyH handle);
C_EXPORT QMetaClassInfoH QMetaClassInfo_Create();
C_EXPORT void QMetaClassInfo_Destroy(QMetaClassInfoH handle);
C_EXPORT const char* QMetaClassInfo_name(QMetaClassInfoH handle);
C_EXPORT const char* QMetaClassInfo_value(QMetaClassInfoH handle);
C_EXPORT const QMetaObjectH QMetaClassInfo_enclosingMetaObject(QMetaClassInfoH handle);

#endif
