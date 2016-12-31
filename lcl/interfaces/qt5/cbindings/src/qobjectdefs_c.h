//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QOBJECTDEFS_C_H
#define QOBJECTDEFS_C_H

#include <QtCore>
#include "pascalbind.h"

C_EXPORT QGenericReturnArgumentH QGenericReturnArgument_Create(const char* aName, void* aData);
C_EXPORT void QGenericReturnArgument_Destroy(QGenericReturnArgumentH handle);
C_EXPORT const char* QMetaObject_className(QMetaObjectH handle);
C_EXPORT const QMetaObjectH QMetaObject_superClass(QMetaObjectH handle);
C_EXPORT QObjectH QMetaObject_cast(QMetaObjectH handle, QObjectH obj);
C_EXPORT void QMetaObject_tr(QMetaObjectH handle, PWideString retval, const char* s, const char* c, int n);
C_EXPORT int QMetaObject_methodOffset(QMetaObjectH handle);
C_EXPORT int QMetaObject_enumeratorOffset(QMetaObjectH handle);
C_EXPORT int QMetaObject_propertyOffset(QMetaObjectH handle);
C_EXPORT int QMetaObject_classInfoOffset(QMetaObjectH handle);
C_EXPORT int QMetaObject_constructorCount(QMetaObjectH handle);
C_EXPORT int QMetaObject_methodCount(QMetaObjectH handle);
C_EXPORT int QMetaObject_enumeratorCount(QMetaObjectH handle);
C_EXPORT int QMetaObject_propertyCount(QMetaObjectH handle);
C_EXPORT int QMetaObject_classInfoCount(QMetaObjectH handle);
C_EXPORT int QMetaObject_indexOfConstructor(QMetaObjectH handle, const char* constructor);
C_EXPORT int QMetaObject_indexOfMethod(QMetaObjectH handle, const char* method);
C_EXPORT int QMetaObject_indexOfSignal(QMetaObjectH handle, const char* signal);
C_EXPORT int QMetaObject_indexOfSlot(QMetaObjectH handle, const char* slot);
C_EXPORT int QMetaObject_indexOfEnumerator(QMetaObjectH handle, const char* name);
C_EXPORT int QMetaObject_indexOfProperty(QMetaObjectH handle, const char* name);
C_EXPORT int QMetaObject_indexOfClassInfo(QMetaObjectH handle, const char* name);
C_EXPORT void QMetaObject_constructor(QMetaObjectH handle, QMetaMethodH retval, int index);
C_EXPORT void QMetaObject_method(QMetaObjectH handle, QMetaMethodH retval, int index);
C_EXPORT void QMetaObject_enumerator(QMetaObjectH handle, QMetaEnumH retval, int index);
C_EXPORT void QMetaObject_property(QMetaObjectH handle, QMetaPropertyH retval, int index);
C_EXPORT void QMetaObject_classInfo(QMetaObjectH handle, QMetaClassInfoH retval, int index);
C_EXPORT void QMetaObject_userProperty(QMetaObjectH handle, QMetaPropertyH retval);
C_EXPORT bool QMetaObject_checkConnectArgs(const char* signal, const char* method);
C_EXPORT bool QMetaObject_checkConnectArgs2(const QMetaMethodH signal, const QMetaMethodH method);
C_EXPORT void QMetaObject_normalizedSignature(QByteArrayH retval, const char* method);
C_EXPORT void QMetaObject_normalizedType(QByteArrayH retval, const char* type);
C_EXPORT bool QMetaObject_disconnect(const QObjectH sender, int signal_index, const QObjectH receiver, int method_index);
C_EXPORT bool QMetaObject_disconnectOne(const QObjectH sender, int signal_index, const QObjectH receiver, int method_index);
C_EXPORT void QMetaObject_connectSlotsByName(QObjectH o);
C_EXPORT void QMetaObject_activate(QObjectH sender, int signal_index, void** argv);
C_EXPORT void QMetaObject_activate2(QObjectH sender, const QMetaObjectH AnonParam2, int local_signal_index, void** argv);
C_EXPORT void QMetaObject_activate3(QObjectH sender, int signal_offset, int local_signal_index, void** argv);
C_EXPORT int QMetaObject_static_metacall(QMetaObjectH handle, QMetaObject::Call AnonParam1, int AnonParam2, void** AnonParam3);
C_EXPORT int QMetaObject_metacall(QObjectH AnonParam1, QMetaObject::Call AnonParam2, int AnonParam3, void** AnonParam4);

#endif
