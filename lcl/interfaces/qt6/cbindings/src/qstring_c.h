//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QSTRING_C_H
#define QSTRING_C_H

#include <QtCore>
#include "pascalbind.h"

C_EXPORT QStringH QString_Create();
C_EXPORT void QString_Destroy(QStringH handle);
C_EXPORT QStringH QString_Create2(const QCharH unicode, int size);
C_EXPORT QStringH QString_Create3(PWideChar c);
C_EXPORT QStringH QString_Create5(PWideString AnonParam1);
C_EXPORT QStringH QString_Create6(const char* ch);
C_EXPORT QStringH QString_Create7(const QByteArrayH a);
C_EXPORT QStringH QString_Create8(int size, Qt::Initialization AnonParam2);
C_EXPORT void QString_swap(QStringH handle, PWideString other);
C_EXPORT int QString_size(QStringH handle);
C_EXPORT int QString_count(QStringH handle);
C_EXPORT int QString_length(QStringH handle);
C_EXPORT bool QString_isEmpty(QStringH handle);
C_EXPORT void QString_resize(QStringH handle, int size);
C_EXPORT void QString_fill(QStringH handle, PWideString retval, PWideChar c, int size);
C_EXPORT void QString_truncate(QStringH handle, int pos);
C_EXPORT void QString_chop(QStringH handle, int n);
C_EXPORT int QString_capacity(QStringH handle);
C_EXPORT void QString_reserve(QStringH handle, int size);
C_EXPORT void QString_squeeze(QStringH handle);
C_EXPORT const QCharH QString_unicode(QStringH handle);
C_EXPORT QCharH QString_data(QStringH handle);
C_EXPORT const QCharH QString_constData(QStringH handle);
C_EXPORT void QString_detach(QStringH handle);
C_EXPORT bool QString_isDetached(QStringH handle);
C_EXPORT bool QString_isSharedWith(QStringH handle, PWideString other);
C_EXPORT void QString_clear(QStringH handle);
C_EXPORT void QString_at(QStringH handle, PWideChar retval, int i);
C_EXPORT void QString_arg(QStringH handle, PWideString retval, qlonglong a, int fieldwidth, int base, PWideChar fillChar);
C_EXPORT void QString_arg2(QStringH handle, PWideString retval, qulonglong a, int fieldwidth, int base, PWideChar fillChar);
C_EXPORT void QString_arg4(QStringH handle, PWideString retval, ulong a, int fieldwidth, int base, PWideChar fillChar);
C_EXPORT void QString_arg7(QStringH handle, PWideString retval, short a, int fieldWidth, int base, PWideChar fillChar);
C_EXPORT void QString_arg8(QStringH handle, PWideString retval, ushort a, int fieldWidth, int base, PWideChar fillChar);
C_EXPORT void QString_arg9(QStringH handle, PWideString retval, double a, int fieldWidth, char fmt, int prec, PWideChar fillChar);
C_EXPORT void QString_arg10(QStringH handle, PWideString retval, char a, int fieldWidth, PWideChar fillChar);
C_EXPORT void QString_arg11(QStringH handle, PWideString retval, PWideChar a, int fieldWidth, PWideChar fillChar);
C_EXPORT void QString_arg12(QStringH handle, PWideString retval, PWideString a, int fieldWidth, PWideChar fillChar);
C_EXPORT void QString_arg13(QStringH handle, PWideString retval, PWideString a1, PWideString a2);
C_EXPORT void QString_arg14(QStringH handle, PWideString retval, PWideString a1, PWideString a2, PWideString a3);
C_EXPORT void QString_arg15(QStringH handle, PWideString retval, PWideString a1, PWideString a2, PWideString a3, PWideString a4);
C_EXPORT void QString_arg16(QStringH handle, PWideString retval, PWideString a1, PWideString a2, PWideString a3, PWideString a4, PWideString a5);
C_EXPORT void QString_arg17(QStringH handle, PWideString retval, PWideString a1, PWideString a2, PWideString a3, PWideString a4, PWideString a5, PWideString a6);
C_EXPORT void QString_arg18(QStringH handle, PWideString retval, PWideString a1, PWideString a2, PWideString a3, PWideString a4, PWideString a5, PWideString a6, PWideString a7);
C_EXPORT void QString_arg19(QStringH handle, PWideString retval, PWideString a1, PWideString a2, PWideString a3, PWideString a4, PWideString a5, PWideString a6, PWideString a7, PWideString a8);
C_EXPORT void QString_arg20(QStringH handle, PWideString retval, PWideString a1, PWideString a2, PWideString a3, PWideString a4, PWideString a5, PWideString a6, PWideString a7, PWideString a8, PWideString a9);
C_EXPORT int QString_indexOf(QStringH handle, PWideChar c, int from, Qt::CaseSensitivity cs);
C_EXPORT int QString_lastIndexOf(QStringH handle, PWideChar c, int from, Qt::CaseSensitivity cs);
C_EXPORT bool QString_contains(QStringH handle, PWideChar c, Qt::CaseSensitivity cs);
C_EXPORT bool QString_contains2(QStringH handle, PWideString s, Qt::CaseSensitivity cs);
C_EXPORT int QString_count2(QStringH handle, PWideChar c, Qt::CaseSensitivity cs);
C_EXPORT int QString_count3(QStringH handle, PWideString s, Qt::CaseSensitivity cs);
C_EXPORT bool QString_contains5(QStringH handle, const QRegularExpressionH re);
C_EXPORT bool QString_contains6(QStringH handle, const QRegularExpressionH re, QRegularExpressionMatchH match);
C_EXPORT int QString_count5(QStringH handle, const QRegularExpressionH re);
C_EXPORT void QString_section(QStringH handle, PWideString retval, PWideChar sep, int start, int end, unsigned int flags);
C_EXPORT void QString_section2(QStringH handle, PWideString retval, PWideString in_sep, int start, int end, unsigned int flags);
C_EXPORT void QString_section4(QStringH handle, PWideString retval, const QRegularExpressionH re, int start, int end, unsigned int flags);
C_EXPORT void QString_left(QStringH handle, PWideString retval, int n);
C_EXPORT void QString_right(QStringH handle, PWideString retval, int n);
C_EXPORT void QString_mid(QStringH handle, PWideString retval, int position, int n);
C_EXPORT bool QString_startsWith(QStringH handle, PWideString s, Qt::CaseSensitivity cs);
C_EXPORT bool QString_startsWith2(QStringH handle, PWideChar c, Qt::CaseSensitivity cs);
C_EXPORT bool QString_endsWith(QStringH handle, PWideString s, Qt::CaseSensitivity cs);
C_EXPORT bool QString_endsWith2(QStringH handle, PWideChar c, Qt::CaseSensitivity cs);
C_EXPORT void QString_leftJustified(QStringH handle, PWideString retval, int width, PWideChar fill, bool trunc);
C_EXPORT void QString_rightJustified(QStringH handle, PWideString retval, int width, PWideChar fill, bool trunc);
C_EXPORT void QString_toLower(QStringH handle, PWideString retval);
C_EXPORT void QString_toUpper(QStringH handle, PWideString retval);
C_EXPORT void QString_toCaseFolded(QStringH handle, PWideString retval);
C_EXPORT void QString_trimmed(QStringH handle, PWideString retval);
C_EXPORT void QString_simplified(QStringH handle, PWideString retval);
C_EXPORT void QString_toHtmlEscaped(QStringH handle, PWideString retval);
C_EXPORT void QString_insert(QStringH handle, PWideString retval, int i, PWideChar c);
C_EXPORT void QString_insert2(QStringH handle, PWideString retval, int i, const QCharH uc, int len);
C_EXPORT void QString_insert3(QStringH handle, PWideString retval, int i, PWideString s);
C_EXPORT void QString_append(QStringH handle, PWideString retval, PWideChar c);
C_EXPORT void QString_append2(QStringH handle, PWideString retval, const QCharH uc, int len);
C_EXPORT void QString_append3(QStringH handle, PWideString retval, PWideString s);
C_EXPORT void QString_prepend(QStringH handle, PWideString retval, PWideChar c);
C_EXPORT void QString_prepend2(QStringH handle, PWideString retval, PWideString s);
C_EXPORT void QString_remove(QStringH handle, PWideString retval, int i, int len);
C_EXPORT void QString_remove2(QStringH handle, PWideString retval, PWideChar c, Qt::CaseSensitivity cs);
C_EXPORT void QString_remove3(QStringH handle, PWideString retval, PWideString s, Qt::CaseSensitivity cs);
C_EXPORT void QString_replace(QStringH handle, PWideString retval, int i, int len, PWideChar after);
C_EXPORT void QString_replace2(QStringH handle, PWideString retval, int i, int len, const QCharH s, int slen);
C_EXPORT void QString_replace3(QStringH handle, PWideString retval, int i, int len, PWideString after);
C_EXPORT void QString_replace4(QStringH handle, PWideString retval, PWideChar before, PWideChar after, Qt::CaseSensitivity cs);
C_EXPORT void QString_replace5(QStringH handle, PWideString retval, const QCharH before, int blen, const QCharH after, int alen, Qt::CaseSensitivity cs);
C_EXPORT void QString_replace6(QStringH handle, PWideString retval, PWideString before, PWideString after, Qt::CaseSensitivity cs);
C_EXPORT void QString_replace7(QStringH handle, PWideString retval, PWideChar c, PWideString after, Qt::CaseSensitivity cs);
C_EXPORT void QString_replace9(QStringH handle, PWideString retval, const QRegularExpressionH re, PWideString after);
C_EXPORT void QString_remove5(QStringH handle, PWideString retval, const QRegularExpressionH re);
C_EXPORT void QString_split(QStringH handle, QStringListH retval, PWideString sep, Qt::SplitBehavior behavior, Qt::CaseSensitivity cs);
C_EXPORT void QString_split2(QStringH handle, QStringListH retval, PWideChar sep, Qt::SplitBehavior behavior, Qt::CaseSensitivity cs);
C_EXPORT void QString_split4(QStringH handle, QStringListH retval, const QRegularExpressionH sep, Qt::SplitBehavior behavior);
C_EXPORT void QString_normalized(QStringH handle, PWideString retval, QString::NormalizationForm mode, QChar::UnicodeVersion version);
C_EXPORT void QString_repeated(QStringH handle, PWideString retval, int times);
C_EXPORT const ushort* QString_utf16(QStringH handle);
C_EXPORT void QString_toLatin1(QStringH handle, QByteArrayH retval);
C_EXPORT void QString_toUtf8(QStringH handle, QByteArrayH retval);
C_EXPORT void QString_toLocal8Bit(QStringH handle, QByteArrayH retval);
C_EXPORT void QString_fromLatin1(PWideString retval, const char* str, int size);
C_EXPORT void QString_fromUtf8(PWideString retval, const char* str, int size);
C_EXPORT void QString_fromLocal8Bit(PWideString retval, const char* str, int size);
C_EXPORT void QString_fromLatin12(PWideString retval, const QByteArrayH str);
C_EXPORT void QString_fromUtf82(PWideString retval, const QByteArrayH str);
C_EXPORT void QString_fromLocal8Bit2(PWideString retval, const QByteArrayH str);
C_EXPORT void QString_fromUtf16(PWideString retval, const ushort* AnonParam1, int size);
C_EXPORT void QString_fromUcs4(PWideString retval, const uint* AnonParam1, int size);
C_EXPORT void QString_fromRawData(PWideString retval, const QCharH AnonParam1, int size);
C_EXPORT int QString_toWCharArray(QStringH handle, wchar_t* array);
C_EXPORT void QString_fromWCharArray(PWideString retval, const wchar_t* string, int size);
C_EXPORT void QString_setRawData(QStringH handle, PWideString retval, const QCharH unicode, int size);
C_EXPORT void QString_setUnicode(QStringH handle, PWideString retval, const QCharH unicode, int size);
C_EXPORT void QString_setUtf16(QStringH handle, PWideString retval, const ushort* utf16, int size);
C_EXPORT int QString_compare(QStringH handle, PWideString s, Qt::CaseSensitivity cs);
C_EXPORT int QString_compare2(PWideString s1, PWideString s2, Qt::CaseSensitivity cs);
C_EXPORT int QString_localeAwareCompare(QStringH handle, PWideString s);
C_EXPORT int QString_localeAwareCompare2(PWideString s1, PWideString s2);
C_EXPORT short QString_toShort(QStringH handle, bool* ok, int base);
C_EXPORT ushort QString_toUShort(QStringH handle, bool* ok, int base);
C_EXPORT int QString_toInt(QStringH handle, bool* ok, int base);
C_EXPORT uint QString_toUInt(QStringH handle, bool* ok, int base);
C_EXPORT long QString_toLong(QStringH handle, bool* ok, int base);
C_EXPORT ulong QString_toULong(QStringH handle, bool* ok, int base);
C_EXPORT qlonglong QString_toLongLong(QStringH handle, bool* ok, int base);
C_EXPORT qulonglong QString_toULongLong(QStringH handle, bool* ok, int base);
C_EXPORT float QString_toFloat(QStringH handle, bool* ok);
C_EXPORT double QString_toDouble(QStringH handle, bool* ok);
C_EXPORT void QString_setNum(QStringH handle, PWideString retval, short AnonParam1, int base);
C_EXPORT void QString_setNum2(QStringH handle, PWideString retval, ushort AnonParam1, int base);
C_EXPORT void QString_setNum3(QStringH handle, PWideString retval, int AnonParam1, int base);
C_EXPORT void QString_setNum4(QStringH handle, PWideString retval, uint AnonParam1, int base);
C_EXPORT void QString_setNum7(QStringH handle, PWideString retval, qlonglong AnonParam1, int base);
C_EXPORT void QString_setNum8(QStringH handle, PWideString retval, qulonglong AnonParam1, int base);
C_EXPORT void QString_setNum9(QStringH handle, PWideString retval, float AnonParam1, char f, int prec);
C_EXPORT void QString_setNum10(QStringH handle, PWideString retval, double AnonParam1, char f, int prec);
C_EXPORT void QString_number(PWideString retval, int AnonParam1, int base);
C_EXPORT void QString_number2(PWideString retval, uint AnonParam1, int base);
C_EXPORT void QString_number5(PWideString retval, qlonglong AnonParam1, int base);
C_EXPORT void QString_number6(PWideString retval, qulonglong AnonParam1, int base);
C_EXPORT void QString_number7(PWideString retval, double AnonParam1, char f, int prec);
C_EXPORT void QString_prepend3(QStringH handle, PWideString retval, const char* s);
C_EXPORT void QString_prepend4(QStringH handle, PWideString retval, const QByteArrayH s);
C_EXPORT void QString_append4(QStringH handle, PWideString retval, const char* s);
C_EXPORT void QString_append5(QStringH handle, PWideString retval, const QByteArrayH s);
C_EXPORT void QString_push_back(QStringH handle, PWideChar c);
C_EXPORT void QString_push_back2(QStringH handle, PWideString s);
C_EXPORT void QString_push_front(QStringH handle, PWideChar c);
C_EXPORT void QString_push_front2(QStringH handle, PWideString s);
C_EXPORT bool QString_isNull(QStringH handle);
C_EXPORT bool QString_isSimpleText(QStringH handle);
C_EXPORT bool QString_isRightToLeft(QStringH handle);
C_EXPORT void QString_chopped(QStringH handle, int len, PWideString retval);
C_EXPORT void QString_first(QStringH handle, int n, PWideString retval);
C_EXPORT bool QString_isValidUtf16(QStringH handle);
C_EXPORT void QString_last(QStringH handle, int n, PWideString retval);

#endif

#ifndef QANYSTRINGVIEW_C_H
#define QANYSTRINGVIEW_C_H
C_EXPORT QAnyStringViewH QAnyStringView_Create();
C_EXPORT QAnyStringViewH QAnyStringView_Create2(PWideString str);
C_EXPORT QAnyStringViewH QAnyStringView_Create3(const QByteArrayH str);
C_EXPORT QAnyStringViewH QAnyStringView_Create4(const char* str);
C_EXPORT void QAnyStringView_Destroy(QAnyStringViewH handle);
C_EXPORT qsizetype QAnyStringView_length(QAnyStringViewH handle);
C_EXPORT qsizetype QAnyStringView_sizeBytes(QAnyStringViewH handle);
C_EXPORT bool QAnyStringView_isEmpty(QAnyStringViewH handle);
C_EXPORT bool QAnyStringView_isNull(QAnyStringViewH handle);
C_EXPORT void QAnyStringView_toString(QAnyStringViewH handle, PWideString retval);
C_EXPORT void QAnyStringView_toString2(QAnyStringViewH handle, QStringH retval);
#endif

#ifndef QSTRINGVIEW_C_H
#define QSTRINGVIEW_C_H
C_EXPORT QStringViewH QStringView_Create();
C_EXPORT QStringViewH QStringView_Create2(PWideString str);
C_EXPORT void QStringView_Destroy(QStringViewH handle);
C_EXPORT qsizetype QStringView_length(QStringViewH handle);
C_EXPORT bool QStringView_isEmpty(QStringViewH handle);
C_EXPORT bool QStringView_isNull(QStringViewH handle);
C_EXPORT bool QStringView_isValidUtf16(QStringViewH handle);
C_EXPORT void QStringView_toString(QStringViewH handle, PWideString retval);
C_EXPORT void QStringView_toString2(QUtf8StringViewH handle, QStringH retval);
C_EXPORT void QStringView_toUtf8(QStringViewH handle, QByteArrayH retval);
#endif

#ifndef QUTF8STRINGVIEW_C_H
#define QUTF8STRINGVIEW_C_H
C_EXPORT QUtf8StringViewH QUtf8StringView_Create();
C_EXPORT QUtf8StringViewH QUtf8StringView_Create2(const char* str);
C_EXPORT void QUtf8StringView_Destroy(QUtf8StringViewH handle);
C_EXPORT qsizetype QUtf8StringView_length(QUtf8StringViewH handle);
C_EXPORT bool QUtf8StringView_isEmpty(QUtf8StringViewH handle);
C_EXPORT bool QUtf8StringView_isNull(QUtf8StringViewH handle);
// Qt 6.3 C_EXPORT bool QUtf8StringView_isValidUtf8(QUtf8StringViewH handle);
C_EXPORT void QUtf8StringView_toString(QUtf8StringViewH handle, PWideString retval);
C_EXPORT void QUtf8StringView_toString2(QUtf8StringViewH handle, QStringH retval);
#endif
