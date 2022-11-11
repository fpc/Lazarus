//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QBYTEARRAY_C_H
#define QBYTEARRAY_C_H

#include <QtCore>
#include "pascalbind.h"

C_EXPORT QByteArrayH QByteArray_Create();
C_EXPORT void QByteArray_Destroy(QByteArrayH handle);
C_EXPORT QByteArrayH QByteArray_Create2(const char* AnonParam1, int size);
C_EXPORT QByteArrayH QByteArray_Create3(int size, char c);
C_EXPORT QByteArrayH QByteArray_Create4(int size, Qt::Initialization AnonParam2);
C_EXPORT QByteArrayH QByteArray_Create5(const QByteArrayH AnonParam1);
C_EXPORT void QByteArray_swap(QByteArrayH handle, QByteArrayH other);
C_EXPORT int QByteArray_size(QByteArrayH handle);
C_EXPORT bool QByteArray_isEmpty(QByteArrayH handle);
C_EXPORT void QByteArray_resize(QByteArrayH handle, int size);
C_EXPORT QByteArrayH QByteArray_fill(QByteArrayH handle, char c, int size);
C_EXPORT int QByteArray_capacity(QByteArrayH handle);
C_EXPORT void QByteArray_reserve(QByteArrayH handle, int size);
C_EXPORT void QByteArray_squeeze(QByteArrayH handle);
C_EXPORT char* QByteArray_data(QByteArrayH handle);
C_EXPORT const char* QByteArray_constData(QByteArrayH handle);
C_EXPORT void QByteArray_detach(QByteArrayH handle);
C_EXPORT bool QByteArray_isDetached(QByteArrayH handle);
C_EXPORT bool QByteArray_isSharedWith(QByteArrayH handle, const QByteArrayH other);
C_EXPORT void QByteArray_clear(QByteArrayH handle);
C_EXPORT char QByteArray_at(QByteArrayH handle, int i);
C_EXPORT int QByteArray_indexOf(QByteArrayH handle, char c, int from);
C_EXPORT int QByteArray_indexOf2(QByteArrayH handle, const char* c, int from);
C_EXPORT int QByteArray_indexOf3(QByteArrayH handle, const QByteArrayH a, int from);
C_EXPORT int QByteArray_lastIndexOf(QByteArrayH handle, char c, int from);
C_EXPORT int QByteArray_lastIndexOf2(QByteArrayH handle, const char* c, int from);
C_EXPORT int QByteArray_lastIndexOf3(QByteArrayH handle, const QByteArrayH a, int from);
C_EXPORT bool QByteArray_contains(QByteArrayH handle, char c);
C_EXPORT bool QByteArray_contains2(QByteArrayH handle, const char* a);
C_EXPORT bool QByteArray_contains3(QByteArrayH handle, const QByteArrayH a);
C_EXPORT int QByteArray_count(QByteArrayH handle, char c);
C_EXPORT int QByteArray_count2(QByteArrayH handle, const char* a);
C_EXPORT int QByteArray_count3(QByteArrayH handle, const QByteArrayH a);
C_EXPORT void QByteArray_left(QByteArrayH handle, QByteArrayH retval, int len);
C_EXPORT void QByteArray_right(QByteArrayH handle, QByteArrayH retval, int len);
C_EXPORT void QByteArray_mid(QByteArrayH handle, QByteArrayH retval, int index, int len);
C_EXPORT bool QByteArray_startsWith(QByteArrayH handle, const QByteArrayH a);
C_EXPORT bool QByteArray_startsWith2(QByteArrayH handle, char c);
C_EXPORT bool QByteArray_startsWith3(QByteArrayH handle, const char* c);
C_EXPORT bool QByteArray_endsWith(QByteArrayH handle, const QByteArrayH a);
C_EXPORT bool QByteArray_endsWith2(QByteArrayH handle, char c);
C_EXPORT bool QByteArray_endsWith3(QByteArrayH handle, const char* c);
C_EXPORT void QByteArray_truncate(QByteArrayH handle, int pos);
C_EXPORT void QByteArray_chop(QByteArrayH handle, int n);
C_EXPORT void QByteArray_toLower(QByteArrayH handle, QByteArrayH retval);
C_EXPORT void QByteArray_toUpper(QByteArrayH handle, QByteArrayH retval);
C_EXPORT void QByteArray_trimmed(QByteArrayH handle, QByteArrayH retval);
C_EXPORT void QByteArray_simplified(QByteArrayH handle, QByteArrayH retval);
C_EXPORT void QByteArray_leftJustified(QByteArrayH handle, QByteArrayH retval, int width, char fill, bool truncate);
C_EXPORT void QByteArray_rightJustified(QByteArrayH handle, QByteArrayH retval, int width, char fill, bool truncate);
C_EXPORT QByteArrayH QByteArray_prepend(QByteArrayH handle, char c);
C_EXPORT QByteArrayH QByteArray_prepend2(QByteArrayH handle, const char* s);
C_EXPORT QByteArrayH QByteArray_prepend3(QByteArrayH handle, const char* s, int len);
C_EXPORT QByteArrayH QByteArray_prepend4(QByteArrayH handle, const QByteArrayH a);
C_EXPORT QByteArrayH QByteArray_append(QByteArrayH handle, char c);
C_EXPORT QByteArrayH QByteArray_append2(QByteArrayH handle, const char* s);
C_EXPORT QByteArrayH QByteArray_append3(QByteArrayH handle, const char* s, int len);
C_EXPORT QByteArrayH QByteArray_append4(QByteArrayH handle, const QByteArrayH a);
C_EXPORT QByteArrayH QByteArray_insert(QByteArrayH handle, int i, char c);
C_EXPORT QByteArrayH QByteArray_insert2(QByteArrayH handle, int i, const char* s);
C_EXPORT QByteArrayH QByteArray_insert3(QByteArrayH handle, int i, const char* s, int len);
C_EXPORT QByteArrayH QByteArray_insert4(QByteArrayH handle, int i, const QByteArrayH a);
C_EXPORT QByteArrayH QByteArray_remove(QByteArrayH handle, int index, int len);
C_EXPORT QByteArrayH QByteArray_replace(QByteArrayH handle, int index, int len, const char* s);
C_EXPORT QByteArrayH QByteArray_replace2(QByteArrayH handle, int index, int len, const char* s, int alen);
C_EXPORT QByteArrayH QByteArray_replace3(QByteArrayH handle, int index, int len, const QByteArrayH s);
C_EXPORT QByteArrayH QByteArray_replace4(QByteArrayH handle, char before, const char* after);
C_EXPORT QByteArrayH QByteArray_replace5(QByteArrayH handle, char before, const QByteArrayH after);
C_EXPORT QByteArrayH QByteArray_replace6(QByteArrayH handle, const char* before, const char* after);
C_EXPORT QByteArrayH QByteArray_replace7(QByteArrayH handle, const char* before, int bsize, const char* after, int asize);
C_EXPORT QByteArrayH QByteArray_replace8(QByteArrayH handle, const QByteArrayH before, const QByteArrayH after);
C_EXPORT QByteArrayH QByteArray_replace9(QByteArrayH handle, const QByteArrayH before, const char* after);
C_EXPORT QByteArrayH QByteArray_replace10(QByteArrayH handle, const char* before, const QByteArrayH after);
C_EXPORT QByteArrayH QByteArray_replace11(QByteArrayH handle, char before, char after);
C_EXPORT void QByteArray_repeated(QByteArrayH handle, QByteArrayH retval, int times);
C_EXPORT QByteArrayH QByteArray_append5(QByteArrayH handle, PWideString s);
C_EXPORT QByteArrayH QByteArray_insert5(QByteArrayH handle, int i, PWideString s);
C_EXPORT QByteArrayH QByteArray_replace12(QByteArrayH handle, PWideString before, const char* after);
C_EXPORT QByteArrayH QByteArray_replace13(QByteArrayH handle, char c, PWideString after);
C_EXPORT QByteArrayH QByteArray_replace14(QByteArrayH handle, PWideString before, const QByteArrayH after);
C_EXPORT int QByteArray_indexOf4(QByteArrayH handle, PWideString s, int from);
C_EXPORT int QByteArray_lastIndexOf4(QByteArrayH handle, PWideString s, int from);
C_EXPORT short QByteArray_toShort(QByteArrayH handle, bool* ok, int base);
C_EXPORT ushort QByteArray_toUShort(QByteArrayH handle, bool* ok, int base);
C_EXPORT int QByteArray_toInt(QByteArrayH handle, bool* ok, int base);
C_EXPORT uint QByteArray_toUInt(QByteArrayH handle, bool* ok, int base);
C_EXPORT long QByteArray_toLong(QByteArrayH handle, bool* ok, int base);
C_EXPORT ulong QByteArray_toULong(QByteArrayH handle, bool* ok, int base);
C_EXPORT qlonglong QByteArray_toLongLong(QByteArrayH handle, bool* ok, int base);
C_EXPORT qulonglong QByteArray_toULongLong(QByteArrayH handle, bool* ok, int base);
C_EXPORT float QByteArray_toFloat(QByteArrayH handle, bool* ok);
C_EXPORT double QByteArray_toDouble(QByteArrayH handle, bool* ok);
C_EXPORT void QByteArray_toBase64(QByteArrayH handle, QByteArrayH retval);
C_EXPORT void QByteArray_toHex(QByteArrayH handle, QByteArrayH retval);
C_EXPORT void QByteArray_toPercentEncoding(QByteArrayH handle, QByteArrayH retval, const QByteArrayH exclude, const QByteArrayH include, char percent);
C_EXPORT QByteArrayH QByteArray_setNum(QByteArrayH handle, short AnonParam1, int base);
C_EXPORT QByteArrayH QByteArray_setNum2(QByteArrayH handle, ushort AnonParam1, int base);
C_EXPORT QByteArrayH QByteArray_setNum3(QByteArrayH handle, int AnonParam1, int base);
C_EXPORT QByteArrayH QByteArray_setNum4(QByteArrayH handle, uint AnonParam1, int base);
C_EXPORT QByteArrayH QByteArray_setNum5(QByteArrayH handle, qlonglong AnonParam1, int base);
C_EXPORT QByteArrayH QByteArray_setNum6(QByteArrayH handle, qulonglong AnonParam1, int base);
C_EXPORT QByteArrayH QByteArray_setNum7(QByteArrayH handle, float AnonParam1, char f, int prec);
C_EXPORT QByteArrayH QByteArray_setNum8(QByteArrayH handle, double AnonParam1, char f, int prec);
C_EXPORT QByteArrayH QByteArray_setRawData(QByteArrayH handle, const char* a, uint n);
C_EXPORT void QByteArray_number(QByteArrayH retval, int AnonParam1, int base);
C_EXPORT void QByteArray_number2(QByteArrayH retval, uint AnonParam1, int base);
C_EXPORT void QByteArray_number3(QByteArrayH retval, qlonglong AnonParam1, int base);
C_EXPORT void QByteArray_number4(QByteArrayH retval, qulonglong AnonParam1, int base);
C_EXPORT void QByteArray_number5(QByteArrayH retval, double AnonParam1, char f, int prec);
C_EXPORT void QByteArray_fromRawData(QByteArrayH retval, const char* AnonParam1, int size);
C_EXPORT void QByteArray_fromBase64(QByteArrayH retval, const QByteArrayH base64);
C_EXPORT void QByteArray_fromHex(QByteArrayH retval, const QByteArrayH hexEncoded);
C_EXPORT void QByteArray_fromPercentEncoding(QByteArrayH retval, const QByteArrayH pctEncoded, char percent);
C_EXPORT void QByteArray_push_back(QByteArrayH handle, char c);
C_EXPORT void QByteArray_push_back2(QByteArrayH handle, const char* c);
C_EXPORT void QByteArray_push_back3(QByteArrayH handle, const QByteArrayH a);
C_EXPORT void QByteArray_push_front(QByteArrayH handle, char c);
C_EXPORT void QByteArray_push_front2(QByteArrayH handle, const char* c);
C_EXPORT void QByteArray_push_front3(QByteArrayH handle, const QByteArrayH a);
C_EXPORT int QByteArray_count4(QByteArrayH handle);
C_EXPORT int QByteArray_length(QByteArrayH handle);
C_EXPORT bool QByteArray_isNull(QByteArrayH handle);

#endif
