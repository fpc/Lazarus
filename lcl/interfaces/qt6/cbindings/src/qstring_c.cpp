//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qstring_c.h"

QStringH QString_Create()
{
	return (QStringH) new QString();
}

void QString_Destroy(QStringH handle)
{
	delete (QString *)handle;
}

QStringH QString_Create2(const QCharH unicode, int size)
{
	return (QStringH) new QString((const QChar*)unicode, size);
}

QStringH QString_Create3(PWideChar c)
{
	return (QStringH) new QString(*(QChar *)c);
}

QStringH QString_Create5(PWideString AnonParam1)
{
	QString t_AnonParam1;
	copyPWideStringToQString(AnonParam1, t_AnonParam1);
	return (QStringH) new QString(t_AnonParam1);
}

void QString_swap(QStringH handle, PWideString other)
{
	QString t_other;
	copyPWideStringToQString(other, t_other);
	((QString *)handle)->swap(t_other);
	copyQStringToPWideString(t_other, other);
}

int QString_size(QStringH handle)
{
	return (int) ((QString *)handle)->size();
}

int QString_count(QStringH handle)
{
	return (int) ((QString *)handle)->count();
}

int QString_length(QStringH handle)
{
	return (int) ((QString *)handle)->length();
}

bool QString_isEmpty(QStringH handle)
{
	return (bool) ((QString *)handle)->isEmpty();
}

void QString_resize(QStringH handle, int size)
{
	((QString *)handle)->resize(size);
}

void QString_fill(QStringH handle, PWideString retval, PWideChar c, int size)
{
	QString t_retval;
	t_retval = ((QString *)handle)->fill(*(QChar *)c, size);
	copyQStringToPWideString(t_retval, retval);
}

void QString_truncate(QStringH handle, int pos)
{
	((QString *)handle)->truncate(pos);
}

void QString_chop(QStringH handle, int n)
{
	((QString *)handle)->chop(n);
}

int QString_capacity(QStringH handle)
{
	return (int) ((QString *)handle)->capacity();
}

void QString_reserve(QStringH handle, int size)
{
	((QString *)handle)->reserve(size);
}

void QString_squeeze(QStringH handle)
{
	((QString *)handle)->squeeze();
}

const QCharH QString_unicode(QStringH handle)
{
	return (const QCharH) ((QString *)handle)->unicode();
}

QCharH QString_data(QStringH handle)
{
	return (QCharH) ((QString *)handle)->data();
}

const QCharH QString_constData(QStringH handle)
{
	return (const QCharH) ((QString *)handle)->constData();
}

void QString_detach(QStringH handle)
{
	((QString *)handle)->detach();
}

bool QString_isDetached(QStringH handle)
{
	return (bool) ((QString *)handle)->isDetached();
}

bool QString_isSharedWith(QStringH handle, PWideString other)
{
	QString t_other;
	copyPWideStringToQString(other, t_other);
	return (bool) ((QString *)handle)->isSharedWith(t_other);
}

void QString_clear(QStringH handle)
{
	((QString *)handle)->clear();
}

void QString_at(QStringH handle, PWideChar retval, int i)
{
	*(QChar *)retval = ((QString *)handle)->at(i);
}

void QString_arg(QStringH handle, PWideString retval, qlonglong a, int fieldwidth, int base, PWideChar fillChar)
{
	QString t_retval;
	t_retval = ((QString *)handle)->arg(a, fieldwidth, base, *(QChar *)fillChar);
	copyQStringToPWideString(t_retval, retval);
}

void QString_arg2(QStringH handle, PWideString retval, qulonglong a, int fieldwidth, int base, PWideChar fillChar)
{
	QString t_retval;
	t_retval = ((QString *)handle)->arg(a, fieldwidth, base, *(QChar *)fillChar);
	copyQStringToPWideString(t_retval, retval);
}

void QString_arg4(QStringH handle, PWideString retval, ulong a, int fieldwidth, int base, PWideChar fillChar)
{
	QString t_retval;
	t_retval = ((QString *)handle)->arg(a, fieldwidth, base, *(QChar *)fillChar);
	copyQStringToPWideString(t_retval, retval);
}

void QString_arg7(QStringH handle, PWideString retval, short a, int fieldWidth, int base, PWideChar fillChar)
{
	QString t_retval;
	t_retval = ((QString *)handle)->arg(a, fieldWidth, base, *(QChar *)fillChar);
	copyQStringToPWideString(t_retval, retval);
}

void QString_arg8(QStringH handle, PWideString retval, ushort a, int fieldWidth, int base, PWideChar fillChar)
{
	QString t_retval;
	t_retval = ((QString *)handle)->arg(a, fieldWidth, base, *(QChar *)fillChar);
	copyQStringToPWideString(t_retval, retval);
}

void QString_arg9(QStringH handle, PWideString retval, double a, int fieldWidth, char fmt, int prec, PWideChar fillChar)
{
	QString t_retval;
	t_retval = ((QString *)handle)->arg(a, fieldWidth, fmt, prec, *(QChar *)fillChar);
	copyQStringToPWideString(t_retval, retval);
}

void QString_arg10(QStringH handle, PWideString retval, char a, int fieldWidth, PWideChar fillChar)
{
	QString t_retval;
	t_retval = ((QString *)handle)->arg(a, fieldWidth, *(QChar *)fillChar);
	copyQStringToPWideString(t_retval, retval);
}

void QString_arg11(QStringH handle, PWideString retval, PWideChar a, int fieldWidth, PWideChar fillChar)
{
	QString t_retval;
	t_retval = ((QString *)handle)->arg(*(QChar *)a, fieldWidth, *(QChar *)fillChar);
	copyQStringToPWideString(t_retval, retval);
}

void QString_arg12(QStringH handle, PWideString retval, PWideString a, int fieldWidth, PWideChar fillChar)
{
	QString t_retval;
	QString t_a;
	copyPWideStringToQString(a, t_a);
	t_retval = ((QString *)handle)->arg(t_a, fieldWidth, *(QChar *)fillChar);
	copyQStringToPWideString(t_retval, retval);
}

void QString_arg13(QStringH handle, PWideString retval, PWideString a1, PWideString a2)
{
	QString t_retval;
	QString t_a1;
	QString t_a2;
	copyPWideStringToQString(a1, t_a1);
	copyPWideStringToQString(a2, t_a2);
	t_retval = ((QString *)handle)->arg(t_a1, t_a2);
	copyQStringToPWideString(t_retval, retval);
}

void QString_arg14(QStringH handle, PWideString retval, PWideString a1, PWideString a2, PWideString a3)
{
	QString t_retval;
	QString t_a1;
	QString t_a2;
	QString t_a3;
	copyPWideStringToQString(a1, t_a1);
	copyPWideStringToQString(a2, t_a2);
	copyPWideStringToQString(a3, t_a3);
	t_retval = ((QString *)handle)->arg(t_a1, t_a2, t_a3);
	copyQStringToPWideString(t_retval, retval);
}

void QString_arg15(QStringH handle, PWideString retval, PWideString a1, PWideString a2, PWideString a3, PWideString a4)
{
	QString t_retval;
	QString t_a1;
	QString t_a2;
	QString t_a3;
	QString t_a4;
	copyPWideStringToQString(a1, t_a1);
	copyPWideStringToQString(a2, t_a2);
	copyPWideStringToQString(a3, t_a3);
	copyPWideStringToQString(a4, t_a4);
	t_retval = ((QString *)handle)->arg(t_a1, t_a2, t_a3, t_a4);
	copyQStringToPWideString(t_retval, retval);
}

void QString_arg16(QStringH handle, PWideString retval, PWideString a1, PWideString a2, PWideString a3, PWideString a4, PWideString a5)
{
	QString t_retval;
	QString t_a1;
	QString t_a2;
	QString t_a3;
	QString t_a4;
	QString t_a5;
	copyPWideStringToQString(a1, t_a1);
	copyPWideStringToQString(a2, t_a2);
	copyPWideStringToQString(a3, t_a3);
	copyPWideStringToQString(a4, t_a4);
	copyPWideStringToQString(a5, t_a5);
	t_retval = ((QString *)handle)->arg(t_a1, t_a2, t_a3, t_a4, t_a5);
	copyQStringToPWideString(t_retval, retval);
}

void QString_arg17(QStringH handle, PWideString retval, PWideString a1, PWideString a2, PWideString a3, PWideString a4, PWideString a5, PWideString a6)
{
	QString t_retval;
	QString t_a1;
	QString t_a2;
	QString t_a3;
	QString t_a4;
	QString t_a5;
	QString t_a6;
	copyPWideStringToQString(a1, t_a1);
	copyPWideStringToQString(a2, t_a2);
	copyPWideStringToQString(a3, t_a3);
	copyPWideStringToQString(a4, t_a4);
	copyPWideStringToQString(a5, t_a5);
	copyPWideStringToQString(a6, t_a6);
	t_retval = ((QString *)handle)->arg(t_a1, t_a2, t_a3, t_a4, t_a5, t_a6);
	copyQStringToPWideString(t_retval, retval);
}

void QString_arg18(QStringH handle, PWideString retval, PWideString a1, PWideString a2, PWideString a3, PWideString a4, PWideString a5, PWideString a6, PWideString a7)
{
	QString t_retval;
	QString t_a1;
	QString t_a2;
	QString t_a3;
	QString t_a4;
	QString t_a5;
	QString t_a6;
	QString t_a7;
	copyPWideStringToQString(a1, t_a1);
	copyPWideStringToQString(a2, t_a2);
	copyPWideStringToQString(a3, t_a3);
	copyPWideStringToQString(a4, t_a4);
	copyPWideStringToQString(a5, t_a5);
	copyPWideStringToQString(a6, t_a6);
	copyPWideStringToQString(a7, t_a7);
	t_retval = ((QString *)handle)->arg(t_a1, t_a2, t_a3, t_a4, t_a5, t_a6, t_a7);
	copyQStringToPWideString(t_retval, retval);
}

void QString_arg19(QStringH handle, PWideString retval, PWideString a1, PWideString a2, PWideString a3, PWideString a4, PWideString a5, PWideString a6, PWideString a7, PWideString a8)
{
	QString t_retval;
	QString t_a1;
	QString t_a2;
	QString t_a3;
	QString t_a4;
	QString t_a5;
	QString t_a6;
	QString t_a7;
	QString t_a8;
	copyPWideStringToQString(a1, t_a1);
	copyPWideStringToQString(a2, t_a2);
	copyPWideStringToQString(a3, t_a3);
	copyPWideStringToQString(a4, t_a4);
	copyPWideStringToQString(a5, t_a5);
	copyPWideStringToQString(a6, t_a6);
	copyPWideStringToQString(a7, t_a7);
	copyPWideStringToQString(a8, t_a8);
	t_retval = ((QString *)handle)->arg(t_a1, t_a2, t_a3, t_a4, t_a5, t_a6, t_a7, t_a8);
	copyQStringToPWideString(t_retval, retval);
}

void QString_arg20(QStringH handle, PWideString retval, PWideString a1, PWideString a2, PWideString a3, PWideString a4, PWideString a5, PWideString a6, PWideString a7, PWideString a8, PWideString a9)
{
	QString t_retval;
	QString t_a1;
	QString t_a2;
	QString t_a3;
	QString t_a4;
	QString t_a5;
	QString t_a6;
	QString t_a7;
	QString t_a8;
	QString t_a9;
	copyPWideStringToQString(a1, t_a1);
	copyPWideStringToQString(a2, t_a2);
	copyPWideStringToQString(a3, t_a3);
	copyPWideStringToQString(a4, t_a4);
	copyPWideStringToQString(a5, t_a5);
	copyPWideStringToQString(a6, t_a6);
	copyPWideStringToQString(a7, t_a7);
	copyPWideStringToQString(a8, t_a8);
	copyPWideStringToQString(a9, t_a9);
	t_retval = ((QString *)handle)->arg(t_a1, t_a2, t_a3, t_a4, t_a5, t_a6, t_a7, t_a8, t_a9);
	copyQStringToPWideString(t_retval, retval);
}

int QString_indexOf(QStringH handle, PWideChar c, int from, Qt::CaseSensitivity cs)
{
	return (int) ((QString *)handle)->indexOf(*(QChar *)c, from, cs);
}

int QString_lastIndexOf(QStringH handle, PWideChar c, int from, Qt::CaseSensitivity cs)
{
	return (int) ((QString *)handle)->lastIndexOf(*(QChar *)c, from, cs);
}

bool QString_contains(QStringH handle, PWideChar c, Qt::CaseSensitivity cs)
{
	return (bool) ((QString *)handle)->contains(*(QChar *)c, cs);
}

bool QString_contains2(QStringH handle, PWideString s, Qt::CaseSensitivity cs)
{
	QString t_s;
	copyPWideStringToQString(s, t_s);
	return (bool) ((QString *)handle)->contains(t_s, cs);
}

int QString_count2(QStringH handle, PWideChar c, Qt::CaseSensitivity cs)
{
	return (int) ((QString *)handle)->count(*(QChar *)c, cs);
}

int QString_count3(QStringH handle, PWideString s, Qt::CaseSensitivity cs)
{
	QString t_s;
	copyPWideStringToQString(s, t_s);
	return (int) ((QString *)handle)->count(t_s, cs);
}

bool QString_contains5(QStringH handle, const QRegularExpressionH re)
{
	return (bool) ((QString *)handle)->contains(*(const QRegularExpression*)re);
}

bool QString_contains6(QStringH handle, const QRegularExpressionH re, QRegularExpressionMatchH match)
{
	return (bool) ((QString *)handle)->contains(*(const QRegularExpression*)re, (QRegularExpressionMatch*)match);
}

int QString_count5(QStringH handle, const QRegularExpressionH re)
{
	return (int) ((QString *)handle)->count(*(const QRegularExpression*)re);
}

void QString_section(QStringH handle, PWideString retval, PWideChar sep, int start, int end, unsigned int flags)
{
	QString t_retval;
	t_retval = ((QString *)handle)->section(*(QChar *)sep, start, end, (QString::SectionFlags)flags);
	copyQStringToPWideString(t_retval, retval);
}

void QString_section2(QStringH handle, PWideString retval, PWideString in_sep, int start, int end, unsigned int flags)
{
	QString t_retval;
	QString t_in_sep;
	copyPWideStringToQString(in_sep, t_in_sep);
	t_retval = ((QString *)handle)->section(t_in_sep, start, end, (QString::SectionFlags)flags);
	copyQStringToPWideString(t_retval, retval);
}

void QString_section4(QStringH handle, PWideString retval, const QRegularExpressionH re, int start, int end, unsigned int flags)
{
	QString t_retval;
	t_retval = ((QString *)handle)->section(*(const QRegularExpression*)re, start, end, (QString::SectionFlags)flags);
	copyQStringToPWideString(t_retval, retval);
}

void QString_left(QStringH handle, PWideString retval, int n)
{
	QString t_retval;
	t_retval = ((QString *)handle)->left(n);
	copyQStringToPWideString(t_retval, retval);
}

void QString_right(QStringH handle, PWideString retval, int n)
{
	QString t_retval;
	t_retval = ((QString *)handle)->right(n);
	copyQStringToPWideString(t_retval, retval);
}

void QString_mid(QStringH handle, PWideString retval, int position, int n)
{
	QString t_retval;
	t_retval = ((QString *)handle)->mid(position, n);
	copyQStringToPWideString(t_retval, retval);
}

bool QString_startsWith(QStringH handle, PWideString s, Qt::CaseSensitivity cs)
{
	QString t_s;
	copyPWideStringToQString(s, t_s);
	return (bool) ((QString *)handle)->startsWith(t_s, cs);
}

bool QString_startsWith2(QStringH handle, PWideChar c, Qt::CaseSensitivity cs)
{
	return (bool) ((QString *)handle)->startsWith(*(QChar *)c, cs);
}

bool QString_endsWith(QStringH handle, PWideString s, Qt::CaseSensitivity cs)
{
	QString t_s;
	copyPWideStringToQString(s, t_s);
	return (bool) ((QString *)handle)->endsWith(t_s, cs);
}

bool QString_endsWith2(QStringH handle, PWideChar c, Qt::CaseSensitivity cs)
{
	return (bool) ((QString *)handle)->endsWith(*(QChar *)c, cs);
}

void QString_leftJustified(QStringH handle, PWideString retval, int width, PWideChar fill, bool trunc)
{
	QString t_retval;
	t_retval = ((QString *)handle)->leftJustified(width, *(QChar *)fill, trunc);
	copyQStringToPWideString(t_retval, retval);
}

void QString_rightJustified(QStringH handle, PWideString retval, int width, PWideChar fill, bool trunc)
{
	QString t_retval;
	t_retval = ((QString *)handle)->rightJustified(width, *(QChar *)fill, trunc);
	copyQStringToPWideString(t_retval, retval);
}

void QString_toLower(QStringH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QString *)handle)->toLower();
	copyQStringToPWideString(t_retval, retval);
}

void QString_toUpper(QStringH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QString *)handle)->toUpper();
	copyQStringToPWideString(t_retval, retval);
}

void QString_toCaseFolded(QStringH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QString *)handle)->toCaseFolded();
	copyQStringToPWideString(t_retval, retval);
}

void QString_trimmed(QStringH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QString *)handle)->trimmed();
	copyQStringToPWideString(t_retval, retval);
}

void QString_simplified(QStringH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QString *)handle)->simplified();
	copyQStringToPWideString(t_retval, retval);
}

void QString_toHtmlEscaped(QStringH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QString *)handle)->toHtmlEscaped();
	copyQStringToPWideString(t_retval, retval);
}

void QString_insert(QStringH handle, PWideString retval, int i, PWideChar c)
{
	QString t_retval;
	t_retval = ((QString *)handle)->insert(i, *(QChar *)c);
	copyQStringToPWideString(t_retval, retval);
}

void QString_insert2(QStringH handle, PWideString retval, int i, const QCharH uc, int len)
{
	QString t_retval;
	t_retval = ((QString *)handle)->insert(i, (const QChar*)uc, len);
	copyQStringToPWideString(t_retval, retval);
}

void QString_insert3(QStringH handle, PWideString retval, int i, PWideString s)
{
	QString t_retval;
	QString t_s;
	copyPWideStringToQString(s, t_s);
	t_retval = ((QString *)handle)->insert(i, t_s);
	copyQStringToPWideString(t_retval, retval);
}

void QString_append(QStringH handle, PWideString retval, PWideChar c)
{
	QString t_retval;
	t_retval = ((QString *)handle)->append(*(QChar *)c);
	copyQStringToPWideString(t_retval, retval);
}

void QString_append2(QStringH handle, PWideString retval, const QCharH uc, int len)
{
	QString t_retval;
	t_retval = ((QString *)handle)->append((const QChar*)uc, len);
	copyQStringToPWideString(t_retval, retval);
}

void QString_append3(QStringH handle, PWideString retval, PWideString s)
{
	QString t_retval;
	QString t_s;
	copyPWideStringToQString(s, t_s);
	t_retval = ((QString *)handle)->append(t_s);
	copyQStringToPWideString(t_retval, retval);
}

void QString_prepend(QStringH handle, PWideString retval, PWideChar c)
{
	QString t_retval;
	t_retval = ((QString *)handle)->prepend(*(QChar *)c);
	copyQStringToPWideString(t_retval, retval);
}

void QString_prepend2(QStringH handle, PWideString retval, PWideString s)
{
	QString t_retval;
	QString t_s;
	copyPWideStringToQString(s, t_s);
	t_retval = ((QString *)handle)->prepend(t_s);
	copyQStringToPWideString(t_retval, retval);
}

void QString_remove(QStringH handle, PWideString retval, int i, int len)
{
	QString t_retval;
	t_retval = ((QString *)handle)->remove(i, len);
	copyQStringToPWideString(t_retval, retval);
}

void QString_remove2(QStringH handle, PWideString retval, PWideChar c, Qt::CaseSensitivity cs)
{
	QString t_retval;
	t_retval = ((QString *)handle)->remove(*(QChar *)c, cs);
	copyQStringToPWideString(t_retval, retval);
}

void QString_remove3(QStringH handle, PWideString retval, PWideString s, Qt::CaseSensitivity cs)
{
	QString t_retval;
	QString t_s;
	copyPWideStringToQString(s, t_s);
	t_retval = ((QString *)handle)->remove(t_s, cs);
	copyQStringToPWideString(t_retval, retval);
}

void QString_replace(QStringH handle, PWideString retval, int i, int len, PWideChar after)
{
	QString t_retval;
	t_retval = ((QString *)handle)->replace(i, len, *(QChar *)after);
	copyQStringToPWideString(t_retval, retval);
}

void QString_replace2(QStringH handle, PWideString retval, int i, int len, const QCharH s, int slen)
{
	QString t_retval;
	t_retval = ((QString *)handle)->replace(i, len, (const QChar*)s, slen);
	copyQStringToPWideString(t_retval, retval);
}

void QString_replace3(QStringH handle, PWideString retval, int i, int len, PWideString after)
{
	QString t_retval;
	QString t_after;
	copyPWideStringToQString(after, t_after);
	t_retval = ((QString *)handle)->replace(i, len, t_after);
	copyQStringToPWideString(t_retval, retval);
}

void QString_replace4(QStringH handle, PWideString retval, PWideChar before, PWideChar after, Qt::CaseSensitivity cs)
{
	QString t_retval;
	t_retval = ((QString *)handle)->replace(*(QChar *)before, *(QChar *)after, cs);
	copyQStringToPWideString(t_retval, retval);
}

void QString_replace5(QStringH handle, PWideString retval, const QCharH before, int blen, const QCharH after, int alen, Qt::CaseSensitivity cs)
{
	QString t_retval;
	t_retval = ((QString *)handle)->replace((const QChar*)before, blen, (const QChar*)after, alen, cs);
	copyQStringToPWideString(t_retval, retval);
}

void QString_replace6(QStringH handle, PWideString retval, PWideString before, PWideString after, Qt::CaseSensitivity cs)
{
	QString t_retval;
	QString t_before;
	QString t_after;
	copyPWideStringToQString(before, t_before);
	copyPWideStringToQString(after, t_after);
	t_retval = ((QString *)handle)->replace(t_before, t_after, cs);
	copyQStringToPWideString(t_retval, retval);
}

void QString_replace7(QStringH handle, PWideString retval, PWideChar c, PWideString after, Qt::CaseSensitivity cs)
{
	QString t_retval;
	QString t_after;
	copyPWideStringToQString(after, t_after);
	t_retval = ((QString *)handle)->replace(*(QChar *)c, t_after, cs);
	copyQStringToPWideString(t_retval, retval);
}


void QString_replace9(QStringH handle, PWideString retval, const QRegularExpressionH re, PWideString after)
{
	QString t_retval;
	QString t_after;
	copyPWideStringToQString(after, t_after);
	t_retval = ((QString *)handle)->replace(*(const QRegularExpression*)re, t_after);
	copyQStringToPWideString(t_retval, retval);
}

void QString_remove5(QStringH handle, PWideString retval, const QRegularExpressionH re)
{
	QString t_retval;
	t_retval = ((QString *)handle)->remove(*(const QRegularExpression*)re);
	copyQStringToPWideString(t_retval, retval);
}

void QString_split(QStringH handle, QStringListH retval, PWideString sep, Qt::SplitBehavior behavior, Qt::CaseSensitivity cs)
{
	QString t_sep;
	copyPWideStringToQString(sep, t_sep);
	*(QStringList *)retval = ((QString *)handle)->split(t_sep, behavior, cs);
}

void QString_split2(QStringH handle, QStringListH retval, PWideChar sep, Qt::SplitBehavior behavior, Qt::CaseSensitivity cs)
{
	*(QStringList *)retval = ((QString *)handle)->split(*(QChar *)sep, behavior, cs);
}

void QString_split4(QStringH handle, QStringListH retval, const QRegularExpressionH sep, Qt::SplitBehavior behavior)
{
	*(QStringList *)retval = ((QString *)handle)->split(*(const QRegularExpression*)sep, behavior);
}

void QString_normalized(QStringH handle, PWideString retval, QString::NormalizationForm mode, QChar::UnicodeVersion version)
{
	QString t_retval;
	t_retval = ((QString *)handle)->normalized(mode, version);
	copyQStringToPWideString(t_retval, retval);
}

void QString_repeated(QStringH handle, PWideString retval, int times)
{
	QString t_retval;
	t_retval = ((QString *)handle)->repeated(times);
	copyQStringToPWideString(t_retval, retval);
}

const ushort* QString_utf16(QStringH handle)
{
	return (const ushort*) ((QString *)handle)->utf16();
}

void QString_toLatin1(QStringH handle, QByteArrayH retval)
{
	*(QByteArray *)retval = ((QString *)handle)->toLatin1();
}

void QString_toUtf8(QStringH handle, QByteArrayH retval)
{
	*(QByteArray *)retval = ((QString *)handle)->toUtf8();
}

void QString_toLocal8Bit(QStringH handle, QByteArrayH retval)
{
	*(QByteArray *)retval = ((QString *)handle)->toLocal8Bit();
}

void QString_fromLatin1(PWideString retval, const char* str, int size)
{
	QString t_retval;
	t_retval = QString::fromLatin1(str, size);
	copyQStringToPWideString(t_retval, retval);
}

void QString_fromUtf8(PWideString retval, const char* str, int size)
{
	QString t_retval;
	t_retval = QString::fromUtf8(str, size);
	copyQStringToPWideString(t_retval, retval);
}

void QString_fromLocal8Bit(PWideString retval, const char* str, int size)
{
	QString t_retval;
	t_retval = QString::fromLocal8Bit(str, size);
	copyQStringToPWideString(t_retval, retval);
}

void QString_fromLatin12(PWideString retval, const QByteArrayH str)
{
	QString t_retval;
	t_retval = QString::fromLatin1(*(const QByteArray*)str);
	copyQStringToPWideString(t_retval, retval);
}

void QString_fromUtf82(PWideString retval, const QByteArrayH str)
{
	QString t_retval;
	t_retval = QString::fromUtf8(*(const QByteArray*)str);
	copyQStringToPWideString(t_retval, retval);
}

void QString_fromLocal8Bit2(PWideString retval, const QByteArrayH str)
{
	QString t_retval;
	t_retval = QString::fromLocal8Bit(*(const QByteArray*)str);
	copyQStringToPWideString(t_retval, retval);
}

void QString_fromUtf16(PWideString retval, const ushort* AnonParam1, int size)
{
	QString t_retval;
	t_retval = QString::fromUtf16(AnonParam1, size);
	copyQStringToPWideString(t_retval, retval);
}

void QString_fromUcs4(PWideString retval, const uint* AnonParam1, int size)
{
	QString t_retval;
	t_retval = QString::fromUcs4(AnonParam1, size);
	copyQStringToPWideString(t_retval, retval);
}

void QString_fromRawData(PWideString retval, const QCharH AnonParam1, int size)
{
	QString t_retval;
	t_retval = QString::fromRawData((const QChar*)AnonParam1, size);
	copyQStringToPWideString(t_retval, retval);
}

int QString_toWCharArray(QStringH handle, wchar_t* array)
{
	return (int) ((QString *)handle)->toWCharArray(array);
}

void QString_fromWCharArray(PWideString retval, const wchar_t* string, int size)
{
	QString t_retval;
	t_retval = QString::fromWCharArray(string, size);
	copyQStringToPWideString(t_retval, retval);
}

void QString_setRawData(QStringH handle, PWideString retval, const QCharH unicode, int size)
{
	QString t_retval;
	t_retval = ((QString *)handle)->setRawData((const QChar*)unicode, size);
	copyQStringToPWideString(t_retval, retval);
}

void QString_setUnicode(QStringH handle, PWideString retval, const QCharH unicode, int size)
{
	QString t_retval;
	t_retval = ((QString *)handle)->setUnicode((const QChar*)unicode, size);
	copyQStringToPWideString(t_retval, retval);
}

void QString_setUtf16(QStringH handle, PWideString retval, const ushort* utf16, int size)
{
	QString t_retval;
	t_retval = ((QString *)handle)->setUtf16(utf16, size);
	copyQStringToPWideString(t_retval, retval);
}

int QString_compare(QStringH handle, PWideString s, Qt::CaseSensitivity cs)
{
	QString t_s;
	copyPWideStringToQString(s, t_s);
	return (int) ((QString *)handle)->compare(t_s, cs);
}

int QString_compare2(PWideString s1, PWideString s2, Qt::CaseSensitivity cs)
{
	QString t_s1;
	QString t_s2;
	copyPWideStringToQString(s1, t_s1);
	copyPWideStringToQString(s2, t_s2);
	return (int) QString::compare(t_s1, t_s2, cs);
}

int QString_localeAwareCompare(QStringH handle, PWideString s)
{
	QString t_s;
	copyPWideStringToQString(s, t_s);
	return (int) ((QString *)handle)->localeAwareCompare(t_s);
}

int QString_localeAwareCompare2(PWideString s1, PWideString s2)
{
	QString t_s1;
	QString t_s2;
	copyPWideStringToQString(s1, t_s1);
	copyPWideStringToQString(s2, t_s2);
	return (int) QString::localeAwareCompare(t_s1, t_s2);
}

short QString_toShort(QStringH handle, bool* ok, int base)
{
	return (short) ((QString *)handle)->toShort(ok, base);
}

ushort QString_toUShort(QStringH handle, bool* ok, int base)
{
	return (ushort) ((QString *)handle)->toUShort(ok, base);
}

int QString_toInt(QStringH handle, bool* ok, int base)
{
	return (int) ((QString *)handle)->toInt(ok, base);
}

uint QString_toUInt(QStringH handle, bool* ok, int base)
{
	return (uint) ((QString *)handle)->toUInt(ok, base);
}

long QString_toLong(QStringH handle, bool* ok, int base)
{
	return (long) ((QString *)handle)->toLong(ok, base);
}

ulong QString_toULong(QStringH handle, bool* ok, int base)
{
	return (ulong) ((QString *)handle)->toULong(ok, base);
}

qlonglong QString_toLongLong(QStringH handle, bool* ok, int base)
{
	return (qlonglong) ((QString *)handle)->toLongLong(ok, base);
}

qulonglong QString_toULongLong(QStringH handle, bool* ok, int base)
{
	return (qulonglong) ((QString *)handle)->toULongLong(ok, base);
}

float QString_toFloat(QStringH handle, bool* ok)
{
	return (float) ((QString *)handle)->toFloat(ok);
}

double QString_toDouble(QStringH handle, bool* ok)
{
	return (double) ((QString *)handle)->toDouble(ok);
}

void QString_setNum(QStringH handle, PWideString retval, short AnonParam1, int base)
{
	QString t_retval;
	t_retval = ((QString *)handle)->setNum(AnonParam1, base);
	copyQStringToPWideString(t_retval, retval);
}

void QString_setNum2(QStringH handle, PWideString retval, ushort AnonParam1, int base)
{
	QString t_retval;
	t_retval = ((QString *)handle)->setNum(AnonParam1, base);
	copyQStringToPWideString(t_retval, retval);
}

void QString_setNum3(QStringH handle, PWideString retval, int AnonParam1, int base)
{
	QString t_retval;
	t_retval = ((QString *)handle)->setNum(AnonParam1, base);
	copyQStringToPWideString(t_retval, retval);
}

void QString_setNum4(QStringH handle, PWideString retval, uint AnonParam1, int base)
{
	QString t_retval;
	t_retval = ((QString *)handle)->setNum(AnonParam1, base);
	copyQStringToPWideString(t_retval, retval);
}

void QString_setNum7(QStringH handle, PWideString retval, qlonglong AnonParam1, int base)
{
	QString t_retval;
	t_retval = ((QString *)handle)->setNum(AnonParam1, base);
	copyQStringToPWideString(t_retval, retval);
}

void QString_setNum8(QStringH handle, PWideString retval, qulonglong AnonParam1, int base)
{
	QString t_retval;
	t_retval = ((QString *)handle)->setNum(AnonParam1, base);
	copyQStringToPWideString(t_retval, retval);
}

void QString_setNum9(QStringH handle, PWideString retval, float AnonParam1, char f, int prec)
{
	QString t_retval;
	t_retval = ((QString *)handle)->setNum(AnonParam1, f, prec);
	copyQStringToPWideString(t_retval, retval);
}

void QString_setNum10(QStringH handle, PWideString retval, double AnonParam1, char f, int prec)
{
	QString t_retval;
	t_retval = ((QString *)handle)->setNum(AnonParam1, f, prec);
	copyQStringToPWideString(t_retval, retval);
}

void QString_number(PWideString retval, int AnonParam1, int base)
{
	QString t_retval;
	t_retval = QString::number(AnonParam1, base);
	copyQStringToPWideString(t_retval, retval);
}

void QString_number2(PWideString retval, uint AnonParam1, int base)
{
	QString t_retval;
	t_retval = QString::number(AnonParam1, base);
	copyQStringToPWideString(t_retval, retval);
}

void QString_number5(PWideString retval, qlonglong AnonParam1, int base)
{
	QString t_retval;
	t_retval = QString::number(AnonParam1, base);
	copyQStringToPWideString(t_retval, retval);
}

void QString_number6(PWideString retval, qulonglong AnonParam1, int base)
{
	QString t_retval;
	t_retval = QString::number(AnonParam1, base);
	copyQStringToPWideString(t_retval, retval);
}

void QString_number7(PWideString retval, double AnonParam1, char f, int prec)
{
	QString t_retval;
	t_retval = QString::number(AnonParam1, f, prec);
	copyQStringToPWideString(t_retval, retval);
}

QStringH QString_Create6(const char* ch)
{
	return (QStringH) new QString(ch);
}

QStringH QString_Create7(const QByteArrayH a)
{
	return (QStringH) new QString(*(const QByteArray*)a);
}

void QString_prepend3(QStringH handle, PWideString retval, const char* s)
{
	QString t_retval;
	t_retval = ((QString *)handle)->prepend(s);
	copyQStringToPWideString(t_retval, retval);
}

void QString_prepend4(QStringH handle, PWideString retval, const QByteArrayH s)
{
	QString t_retval;
	t_retval = ((QString *)handle)->prepend(*(const QByteArray*)s);
	copyQStringToPWideString(t_retval, retval);
}

void QString_append4(QStringH handle, PWideString retval, const char* s)
{
	QString t_retval;
	t_retval = ((QString *)handle)->append(s);
	copyQStringToPWideString(t_retval, retval);
}

void QString_append5(QStringH handle, PWideString retval, const QByteArrayH s)
{
	QString t_retval;
	t_retval = ((QString *)handle)->append(*(const QByteArray*)s);
	copyQStringToPWideString(t_retval, retval);
}

void QString_push_back(QStringH handle, PWideChar c)
{
	((QString *)handle)->push_back(*(QChar *)c);
}

void QString_push_back2(QStringH handle, PWideString s)
{
	QString t_s;
	copyPWideStringToQString(s, t_s);
	((QString *)handle)->push_back(t_s);
}

void QString_push_front(QStringH handle, PWideChar c)
{
	((QString *)handle)->push_front(*(QChar *)c);
}

void QString_push_front2(QStringH handle, PWideString s)
{
	QString t_s;
	copyPWideStringToQString(s, t_s);
	((QString *)handle)->push_front(t_s);
}

bool QString_isNull(QStringH handle)
{
	return (bool) ((QString *)handle)->isNull();
}

bool QString_isRightToLeft(QStringH handle)
{
	return (bool) ((QString *)handle)->isRightToLeft();
}

QStringH QString_Create8(int size, Qt::Initialization AnonParam2)
{
	return (QStringH) new QString(size, AnonParam2);
}

void QString_chopped(QStringH handle, int len, PWideString retval)
{
	QString t_retval;
	t_retval = ((QString *)handle)->chopped((qsizetype) len);
	copyQStringToPWideString(t_retval, retval);

}

void QString_first(QStringH handle, int n, PWideString retval)
{
	QString t_retval;
	t_retval = ((QString *)handle)->first((qsizetype) n);
	copyQStringToPWideString(t_retval, retval);
}

bool QString_isValidUtf16(QStringH handle)
{
  return (bool) ((QString *)handle)->isValidUtf16();
}

void QString_last(QStringH handle, int n, PWideString retval)
{
	QString t_retval;
	t_retval = ((QString *)handle)->last((qsizetype) n);
	copyQStringToPWideString(t_retval, retval);
}

QAnyStringViewH QAnyStringView_Create()
{
  return (QAnyStringViewH) new QAnyStringView();
}

QAnyStringViewH QAnyStringView_Create2(PWideString str)
{
	QString t_AnonParam1;
	copyPWideStringToQString(str, t_AnonParam1);
	return (QAnyStringViewH) new QAnyStringView(t_AnonParam1);
}

QAnyStringViewH QAnyStringView_Create3(const QByteArrayH str)
{
	return (QAnyStringViewH) new QAnyStringView(*(const QByteArray*)str);
}

QAnyStringViewH QAnyStringView_Create4(const char* str)
{
	return (QAnyStringViewH) new QAnyStringView(str);
}

void QAnyStringView_Destroy(QAnyStringViewH handle)
{
	delete (QAnyStringView *)handle;
}

qsizetype QAnyStringView_length(QAnyStringViewH handle)
{
  return (qsizetype) ((QAnyStringView *)handle)->length();
}

qsizetype QAnyStringView_sizeBytes(QAnyStringViewH handle)
{
  return (qsizetype) ((QAnyStringView *)handle)->size_bytes();
}

bool QAnyStringView_isEmpty(QAnyStringViewH handle)
{
  return (bool) ((QAnyStringView *)handle)->isEmpty();
}

bool QAnyStringView_isNull(QAnyStringViewH handle)
{
  return (bool) ((QAnyStringView *)handle)->isNull();
}

void QAnyStringView_toString(QAnyStringViewH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QAnyStringView *)handle)->toString();
	copyQStringToPWideString(t_retval, retval);
}

void QAnyStringView_toString2(QAnyStringViewH handle, QStringH retval)
{
  *(QString *)retval = ((QAnyStringView *)handle)->toString();
}

QStringViewH QStringView_Create()
{
  return (QStringViewH) new QStringView();
}

QStringViewH QStringView_Create2(PWideString str)
{
	QString t_AnonParam1;
	copyPWideStringToQString(str, t_AnonParam1);
	return (QStringViewH) new QStringView(t_AnonParam1);
}

/*
QStringViewH QStringView_Create3(const char* str)
{
	return (QStringViewH) new QStringView(str);
}
*/

void QStringView_Destroy(QStringViewH handle)
{
	delete (QStringView *)handle;
}

qsizetype QStringView_length(QStringViewH handle)
{
  return (qsizetype) ((QStringView *)handle)->length();
}

bool QStringView_isEmpty(QStringViewH handle)
{
  return (bool) ((QStringView *)handle)->isEmpty();
}

bool QStringView_isNull(QStringViewH handle)
{
  return (bool) ((QStringView *)handle)->isNull();
}

bool QStringView_isValidUtf16(QStringViewH handle)
{
  return (bool) ((QStringView *)handle)->isValidUtf16();
}

void QStringView_toString(QStringViewH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QStringView *)handle)->toString();
	copyQStringToPWideString(t_retval, retval);
}

void QStringView_toString2(QUtf8StringViewH handle, QStringH retval)
{
  *(QString *)retval = ((QStringView *)handle)->toString();
}

void QStringView_toUtf8(QStringViewH handle, QByteArrayH retval)
{
  *(QByteArray *)retval = ((QStringView *)handle)->toUtf8();
}

QUtf8StringViewH QUtf8StringView_Create()
{
  return (QUtf8StringViewH) new QUtf8StringView();
}

QUtf8StringViewH QUtf8StringView_Create2(const char* str)
{
	return (QUtf8StringViewH) new QUtf8StringView(str);
}

void QUtf8StringView_Destroy(QUtf8StringViewH handle)
{
  delete (QUtf8StringView *)handle;
}

qsizetype QUtf8StringView_length(QUtf8StringViewH handle)
{
 return (qsizetype) ((QUtf8StringView *)handle)->length();
}

bool QUtf8StringView_isEmpty(QUtf8StringViewH handle)
{
 return (bool) ((QUtf8StringView *)handle)->isEmpty();
}

bool QUtf8StringView_isNull(QUtf8StringViewH handle)
{
 return (bool) ((QUtf8StringView *)handle)->isNull();
}

/* Qt 6.3
bool QUtf8StringView_isValidUtf8(QUtf8StringViewH handle)
{
 return (bool) ((QUtf8StringView *)handle)->isValidUtf8();
}
*/

void QUtf8StringView_toString(QUtf8StringViewH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QUtf8StringView *)handle)->toString();
	copyQStringToPWideString(t_retval, retval);
}

void QUtf8StringView_toString2(QUtf8StringViewH handle, QStringH retval)
{
  *(QString *)retval = ((QUtf8StringView *)handle)->toString();
}

