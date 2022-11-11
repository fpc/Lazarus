//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qbytearray_c.h"

QByteArrayH QByteArray_Create()
{
	return (QByteArrayH) new QByteArray();
}

void QByteArray_Destroy(QByteArrayH handle)
{
	delete (QByteArray *)handle;
}

QByteArrayH QByteArray_Create2(const char* AnonParam1, int size)
{
	return (QByteArrayH) new QByteArray(AnonParam1, size);
}

QByteArrayH QByteArray_Create3(int size, char c)
{
	return (QByteArrayH) new QByteArray(size, c);
}

QByteArrayH QByteArray_Create4(int size, Qt::Initialization AnonParam2)
{
	return (QByteArrayH) new QByteArray(size, AnonParam2);
}

QByteArrayH QByteArray_Create5(const QByteArrayH AnonParam1)
{
	return (QByteArrayH) new QByteArray(*(const QByteArray*)AnonParam1);
}

void QByteArray_swap(QByteArrayH handle, QByteArrayH other)
{
	((QByteArray *)handle)->swap(*(QByteArray*)other);
}

int QByteArray_size(QByteArrayH handle)
{
	return (int) ((QByteArray *)handle)->size();
}

bool QByteArray_isEmpty(QByteArrayH handle)
{
	return (bool) ((QByteArray *)handle)->isEmpty();
}

void QByteArray_resize(QByteArrayH handle, int size)
{
	((QByteArray *)handle)->resize(size);
}

QByteArrayH QByteArray_fill(QByteArrayH handle, char c, int size)
{
	return (QByteArrayH) &((QByteArray *)handle)->fill(c, size);
}

int QByteArray_capacity(QByteArrayH handle)
{
	return (int) ((QByteArray *)handle)->capacity();
}

void QByteArray_reserve(QByteArrayH handle, int size)
{
	((QByteArray *)handle)->reserve(size);
}

void QByteArray_squeeze(QByteArrayH handle)
{
	((QByteArray *)handle)->squeeze();
}

char* QByteArray_data(QByteArrayH handle)
{
	return (char*) ((QByteArray *)handle)->data();
}

const char* QByteArray_constData(QByteArrayH handle)
{
	return (const char*) ((QByteArray *)handle)->constData();
}

void QByteArray_detach(QByteArrayH handle)
{
	((QByteArray *)handle)->detach();
}

bool QByteArray_isDetached(QByteArrayH handle)
{
	return (bool) ((QByteArray *)handle)->isDetached();
}

bool QByteArray_isSharedWith(QByteArrayH handle, const QByteArrayH other)
{
	return (bool) ((QByteArray *)handle)->isSharedWith(*(const QByteArray*)other);
}

void QByteArray_clear(QByteArrayH handle)
{
	((QByteArray *)handle)->clear();
}

char QByteArray_at(QByteArrayH handle, int i)
{
	return (char) ((QByteArray *)handle)->at(i);
}

int QByteArray_indexOf(QByteArrayH handle, char c, int from)
{
	return (int) ((QByteArray *)handle)->indexOf(c, from);
}

int QByteArray_indexOf2(QByteArrayH handle, const char* c, int from)
{
	return (int) ((QByteArray *)handle)->indexOf(c, from);
}

int QByteArray_indexOf3(QByteArrayH handle, const QByteArrayH a, int from)
{
	return (int) ((QByteArray *)handle)->indexOf(*(const QByteArray*)a, from);
}

int QByteArray_lastIndexOf(QByteArrayH handle, char c, int from)
{
	return (int) ((QByteArray *)handle)->lastIndexOf(c, from);
}

int QByteArray_lastIndexOf2(QByteArrayH handle, const char* c, int from)
{
	return (int) ((QByteArray *)handle)->lastIndexOf(c, from);
}

int QByteArray_lastIndexOf3(QByteArrayH handle, const QByteArrayH a, int from)
{
	return (int) ((QByteArray *)handle)->lastIndexOf(*(const QByteArray*)a, from);
}

bool QByteArray_contains(QByteArrayH handle, char c)
{
	return (bool) ((QByteArray *)handle)->contains(c);
}

bool QByteArray_contains2(QByteArrayH handle, const char* a)
{
	return (bool) ((QByteArray *)handle)->contains(a);
}

bool QByteArray_contains3(QByteArrayH handle, const QByteArrayH a)
{
	return (bool) ((QByteArray *)handle)->contains(*(const QByteArray*)a);
}

int QByteArray_count(QByteArrayH handle, char c)
{
	return (int) ((QByteArray *)handle)->count(c);
}

int QByteArray_count2(QByteArrayH handle, const char* a)
{
	return (int) ((QByteArray *)handle)->count(a);
}

int QByteArray_count3(QByteArrayH handle, const QByteArrayH a)
{
	return (int) ((QByteArray *)handle)->count(*(const QByteArray*)a);
}

void QByteArray_left(QByteArrayH handle, QByteArrayH retval, int len)
{
	*(QByteArray *)retval = ((QByteArray *)handle)->left(len);
}

void QByteArray_right(QByteArrayH handle, QByteArrayH retval, int len)
{
	*(QByteArray *)retval = ((QByteArray *)handle)->right(len);
}

void QByteArray_mid(QByteArrayH handle, QByteArrayH retval, int index, int len)
{
	*(QByteArray *)retval = ((QByteArray *)handle)->mid(index, len);
}

bool QByteArray_startsWith(QByteArrayH handle, const QByteArrayH a)
{
	return (bool) ((QByteArray *)handle)->startsWith(*(const QByteArray*)a);
}

bool QByteArray_startsWith2(QByteArrayH handle, char c)
{
	return (bool) ((QByteArray *)handle)->startsWith(c);
}

bool QByteArray_startsWith3(QByteArrayH handle, const char* c)
{
	return (bool) ((QByteArray *)handle)->startsWith(c);
}

bool QByteArray_endsWith(QByteArrayH handle, const QByteArrayH a)
{
	return (bool) ((QByteArray *)handle)->endsWith(*(const QByteArray*)a);
}

bool QByteArray_endsWith2(QByteArrayH handle, char c)
{
	return (bool) ((QByteArray *)handle)->endsWith(c);
}

bool QByteArray_endsWith3(QByteArrayH handle, const char* c)
{
	return (bool) ((QByteArray *)handle)->endsWith(c);
}

void QByteArray_truncate(QByteArrayH handle, int pos)
{
	((QByteArray *)handle)->truncate(pos);
}

void QByteArray_chop(QByteArrayH handle, int n)
{
	((QByteArray *)handle)->chop(n);
}

void QByteArray_toLower(QByteArrayH handle, QByteArrayH retval)
{
	*(QByteArray *)retval = ((QByteArray *)handle)->toLower();
}

void QByteArray_toUpper(QByteArrayH handle, QByteArrayH retval)
{
	*(QByteArray *)retval = ((QByteArray *)handle)->toUpper();
}

void QByteArray_trimmed(QByteArrayH handle, QByteArrayH retval)
{
	*(QByteArray *)retval = ((QByteArray *)handle)->trimmed();
}

void QByteArray_simplified(QByteArrayH handle, QByteArrayH retval)
{
	*(QByteArray *)retval = ((QByteArray *)handle)->simplified();
}

void QByteArray_leftJustified(QByteArrayH handle, QByteArrayH retval, int width, char fill, bool truncate)
{
	*(QByteArray *)retval = ((QByteArray *)handle)->leftJustified(width, fill, truncate);
}

void QByteArray_rightJustified(QByteArrayH handle, QByteArrayH retval, int width, char fill, bool truncate)
{
	*(QByteArray *)retval = ((QByteArray *)handle)->rightJustified(width, fill, truncate);
}

QByteArrayH QByteArray_prepend(QByteArrayH handle, char c)
{
	return (QByteArrayH) &((QByteArray *)handle)->prepend(c);
}

QByteArrayH QByteArray_prepend2(QByteArrayH handle, const char* s)
{
	return (QByteArrayH) &((QByteArray *)handle)->prepend(s);
}

QByteArrayH QByteArray_prepend3(QByteArrayH handle, const char* s, int len)
{
	return (QByteArrayH) &((QByteArray *)handle)->prepend(s, len);
}

QByteArrayH QByteArray_prepend4(QByteArrayH handle, const QByteArrayH a)
{
	return (QByteArrayH) &((QByteArray *)handle)->prepend(*(const QByteArray*)a);
}

QByteArrayH QByteArray_append(QByteArrayH handle, char c)
{
	return (QByteArrayH) &((QByteArray *)handle)->append(c);
}

QByteArrayH QByteArray_append2(QByteArrayH handle, const char* s)
{
	return (QByteArrayH) &((QByteArray *)handle)->append(s);
}

QByteArrayH QByteArray_append3(QByteArrayH handle, const char* s, int len)
{
	return (QByteArrayH) &((QByteArray *)handle)->append(s, len);
}

QByteArrayH QByteArray_append4(QByteArrayH handle, const QByteArrayH a)
{
	return (QByteArrayH) &((QByteArray *)handle)->append(*(const QByteArray*)a);
}

QByteArrayH QByteArray_insert(QByteArrayH handle, int i, char c)
{
	return (QByteArrayH) &((QByteArray *)handle)->insert(i, c);
}

QByteArrayH QByteArray_insert2(QByteArrayH handle, int i, const char* s)
{
	return (QByteArrayH) &((QByteArray *)handle)->insert(i, s);
}

QByteArrayH QByteArray_insert3(QByteArrayH handle, int i, const char* s, int len)
{
	return (QByteArrayH) &((QByteArray *)handle)->insert(i, s, len);
}

QByteArrayH QByteArray_insert4(QByteArrayH handle, int i, const QByteArrayH a)
{
	return (QByteArrayH) &((QByteArray *)handle)->insert(i, *(const QByteArray*)a);
}

QByteArrayH QByteArray_remove(QByteArrayH handle, int index, int len)
{
	return (QByteArrayH) &((QByteArray *)handle)->remove(index, len);
}

QByteArrayH QByteArray_replace(QByteArrayH handle, int index, int len, const char* s)
{
	return (QByteArrayH) &((QByteArray *)handle)->replace(index, len, s);
}

QByteArrayH QByteArray_replace2(QByteArrayH handle, int index, int len, const char* s, int alen)
{
	return (QByteArrayH) &((QByteArray *)handle)->replace(index, len, s, alen);
}

QByteArrayH QByteArray_replace3(QByteArrayH handle, int index, int len, const QByteArrayH s)
{
	return (QByteArrayH) &((QByteArray *)handle)->replace(index, len, *(const QByteArray*)s);
}

QByteArrayH QByteArray_replace4(QByteArrayH handle, char before, const char* after)
{
	return (QByteArrayH) &((QByteArray *)handle)->replace(before, after);
}

QByteArrayH QByteArray_replace5(QByteArrayH handle, char before, const QByteArrayH after)
{
	return (QByteArrayH) &((QByteArray *)handle)->replace(before, *(const QByteArray*)after);
}

QByteArrayH QByteArray_replace6(QByteArrayH handle, const char* before, const char* after)
{
	return (QByteArrayH) &((QByteArray *)handle)->replace(before, after);
}

QByteArrayH QByteArray_replace7(QByteArrayH handle, const char* before, int bsize, const char* after, int asize)
{
	return (QByteArrayH) &((QByteArray *)handle)->replace(before, bsize, after, asize);
}

QByteArrayH QByteArray_replace8(QByteArrayH handle, const QByteArrayH before, const QByteArrayH after)
{
	return (QByteArrayH) &((QByteArray *)handle)->replace(*(const QByteArray*)before, *(const QByteArray*)after);
}

QByteArrayH QByteArray_replace9(QByteArrayH handle, const QByteArrayH before, const char* after)
{
	return (QByteArrayH) &((QByteArray *)handle)->replace(*(const QByteArray*)before, after);
}

QByteArrayH QByteArray_replace10(QByteArrayH handle, const char* before, const QByteArrayH after)
{
	return (QByteArrayH) &((QByteArray *)handle)->replace(before, *(const QByteArray*)after);
}

QByteArrayH QByteArray_replace11(QByteArrayH handle, char before, char after)
{
	return (QByteArrayH) &((QByteArray *)handle)->replace(before, after);
}

void QByteArray_repeated(QByteArrayH handle, QByteArrayH retval, int times)
{
	*(QByteArray *)retval = ((QByteArray *)handle)->repeated(times);
}

QByteArrayH QByteArray_append5(QByteArrayH handle, PWideString s)
{
	QString t_s;
	copyPWideStringToQString(s, t_s);
	return (QByteArrayH) &((QByteArray *)handle)->append(t_s.toUtf8());
}

QByteArrayH QByteArray_insert5(QByteArrayH handle, int i, PWideString s)
{
	QString t_s;
	copyPWideStringToQString(s, t_s);
	return (QByteArrayH) &((QByteArray *)handle)->insert(i, t_s.toUtf8());
}

QByteArrayH QByteArray_replace12(QByteArrayH handle, PWideString before, const char* after)
{
	QString t_before;
	copyPWideStringToQString(before, t_before);
	return (QByteArrayH) &((QByteArray *)handle)->replace(t_before.toUtf8(), after);
}

QByteArrayH QByteArray_replace13(QByteArrayH handle, char c, PWideString after)
{
	QString t_after;
	copyPWideStringToQString(after, t_after);
	return (QByteArrayH) &((QByteArray *)handle)->replace(c, t_after.toUtf8());
}

QByteArrayH QByteArray_replace14(QByteArrayH handle, PWideString before, const QByteArrayH after)
{
	QString t_before;
	copyPWideStringToQString(before, t_before);
	return (QByteArrayH) &((QByteArray *)handle)->replace(t_before.toUtf8(), *(const QByteArray*)after);
}

int QByteArray_indexOf4(QByteArrayH handle, PWideString s, int from)
{
	QString t_s;
	copyPWideStringToQString(s, t_s);
	return (int) ((QByteArray *)handle)->indexOf(t_s.toUtf8(), from);
}

int QByteArray_lastIndexOf4(QByteArrayH handle, PWideString s, int from)
{
	QString t_s;
	copyPWideStringToQString(s, t_s);
	return (int) ((QByteArray *)handle)->lastIndexOf(t_s.toUtf8(), from);
}

short QByteArray_toShort(QByteArrayH handle, bool* ok, int base)
{
	return (short) ((QByteArray *)handle)->toShort(ok, base);
}

ushort QByteArray_toUShort(QByteArrayH handle, bool* ok, int base)
{
	return (ushort) ((QByteArray *)handle)->toUShort(ok, base);
}

int QByteArray_toInt(QByteArrayH handle, bool* ok, int base)
{
	return (int) ((QByteArray *)handle)->toInt(ok, base);
}

uint QByteArray_toUInt(QByteArrayH handle, bool* ok, int base)
{
	return (uint) ((QByteArray *)handle)->toUInt(ok, base);
}

long QByteArray_toLong(QByteArrayH handle, bool* ok, int base)
{
	return (long) ((QByteArray *)handle)->toLong(ok, base);
}

ulong QByteArray_toULong(QByteArrayH handle, bool* ok, int base)
{
	return (ulong) ((QByteArray *)handle)->toULong(ok, base);
}

qlonglong QByteArray_toLongLong(QByteArrayH handle, bool* ok, int base)
{
	return (qlonglong) ((QByteArray *)handle)->toLongLong(ok, base);
}

qulonglong QByteArray_toULongLong(QByteArrayH handle, bool* ok, int base)
{
	return (qulonglong) ((QByteArray *)handle)->toULongLong(ok, base);
}

float QByteArray_toFloat(QByteArrayH handle, bool* ok)
{
	return (float) ((QByteArray *)handle)->toFloat(ok);
}

double QByteArray_toDouble(QByteArrayH handle, bool* ok)
{
	return (double) ((QByteArray *)handle)->toDouble(ok);
}

void QByteArray_toBase64(QByteArrayH handle, QByteArrayH retval)
{
	*(QByteArray *)retval = ((QByteArray *)handle)->toBase64();
}

void QByteArray_toHex(QByteArrayH handle, QByteArrayH retval)
{
	*(QByteArray *)retval = ((QByteArray *)handle)->toHex();
}

void QByteArray_toPercentEncoding(QByteArrayH handle, QByteArrayH retval, const QByteArrayH exclude, const QByteArrayH include, char percent)
{
	*(QByteArray *)retval = ((QByteArray *)handle)->toPercentEncoding(*(const QByteArray*)exclude, *(const QByteArray*)include, percent);
}

QByteArrayH QByteArray_setNum(QByteArrayH handle, short AnonParam1, int base)
{
	return (QByteArrayH) &((QByteArray *)handle)->setNum(AnonParam1, base);
}

QByteArrayH QByteArray_setNum2(QByteArrayH handle, ushort AnonParam1, int base)
{
	return (QByteArrayH) &((QByteArray *)handle)->setNum(AnonParam1, base);
}

QByteArrayH QByteArray_setNum3(QByteArrayH handle, int AnonParam1, int base)
{
	return (QByteArrayH) &((QByteArray *)handle)->setNum(AnonParam1, base);
}

QByteArrayH QByteArray_setNum4(QByteArrayH handle, uint AnonParam1, int base)
{
	return (QByteArrayH) &((QByteArray *)handle)->setNum(AnonParam1, base);
}

QByteArrayH QByteArray_setNum5(QByteArrayH handle, qlonglong AnonParam1, int base)
{
	return (QByteArrayH) &((QByteArray *)handle)->setNum(AnonParam1, base);
}

QByteArrayH QByteArray_setNum6(QByteArrayH handle, qulonglong AnonParam1, int base)
{
	return (QByteArrayH) &((QByteArray *)handle)->setNum(AnonParam1, base);
}

QByteArrayH QByteArray_setNum7(QByteArrayH handle, float AnonParam1, char f, int prec)
{
	return (QByteArrayH) &((QByteArray *)handle)->setNum(AnonParam1, f, prec);
}

QByteArrayH QByteArray_setNum8(QByteArrayH handle, double AnonParam1, char f, int prec)
{
	return (QByteArrayH) &((QByteArray *)handle)->setNum(AnonParam1, f, prec);
}

QByteArrayH QByteArray_setRawData(QByteArrayH handle, const char* a, uint n)
{
	return (QByteArrayH) &((QByteArray *)handle)->setRawData(a, n);
}

void QByteArray_number(QByteArrayH retval, int AnonParam1, int base)
{
	*(QByteArray *)retval = QByteArray::number(AnonParam1, base);
}

void QByteArray_number2(QByteArrayH retval, uint AnonParam1, int base)
{
	*(QByteArray *)retval = QByteArray::number(AnonParam1, base);
}

void QByteArray_number3(QByteArrayH retval, qlonglong AnonParam1, int base)
{
	*(QByteArray *)retval = QByteArray::number(AnonParam1, base);
}

void QByteArray_number4(QByteArrayH retval, qulonglong AnonParam1, int base)
{
	*(QByteArray *)retval = QByteArray::number(AnonParam1, base);
}

void QByteArray_number5(QByteArrayH retval, double AnonParam1, char f, int prec)
{
	*(QByteArray *)retval = QByteArray::number(AnonParam1, f, prec);
}

void QByteArray_fromRawData(QByteArrayH retval, const char* AnonParam1, int size)
{
	*(QByteArray *)retval = QByteArray::fromRawData(AnonParam1, size);
}

void QByteArray_fromBase64(QByteArrayH retval, const QByteArrayH base64)
{
	*(QByteArray *)retval = QByteArray::fromBase64(*(const QByteArray*)base64);
}

void QByteArray_fromHex(QByteArrayH retval, const QByteArrayH hexEncoded)
{
	*(QByteArray *)retval = QByteArray::fromHex(*(const QByteArray*)hexEncoded);
}

void QByteArray_fromPercentEncoding(QByteArrayH retval, const QByteArrayH pctEncoded, char percent)
{
	*(QByteArray *)retval = QByteArray::fromPercentEncoding(*(const QByteArray*)pctEncoded, percent);
}

void QByteArray_push_back(QByteArrayH handle, char c)
{
	((QByteArray *)handle)->push_back(c);
}

void QByteArray_push_back2(QByteArrayH handle, const char* c)
{
	((QByteArray *)handle)->push_back(c);
}

void QByteArray_push_back3(QByteArrayH handle, const QByteArrayH a)
{
	((QByteArray *)handle)->push_back(*(const QByteArray*)a);
}

void QByteArray_push_front(QByteArrayH handle, char c)
{
	((QByteArray *)handle)->push_front(c);
}

void QByteArray_push_front2(QByteArrayH handle, const char* c)
{
	((QByteArray *)handle)->push_front(c);
}

void QByteArray_push_front3(QByteArrayH handle, const QByteArrayH a)
{
	((QByteArray *)handle)->push_front(*(const QByteArray*)a);
}

int QByteArray_count4(QByteArrayH handle)
{
	return (int) ((QByteArray *)handle)->count();
}

int QByteArray_length(QByteArrayH handle)
{
	return (int) ((QByteArray *)handle)->length();
}

bool QByteArray_isNull(QByteArrayH handle)
{
	return (bool) ((QByteArray *)handle)->isNull();
}

