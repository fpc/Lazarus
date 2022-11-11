//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QCHAR_C_H
#define QCHAR_C_H

#include <QtCore>
#include "pascalbind.h"

C_EXPORT QCharH QChar_Create();
C_EXPORT void QChar_Destroy(QCharH handle);
C_EXPORT QCharH QChar_Create2(ushort rc);
C_EXPORT QCharH QChar_Create4(short rc);
C_EXPORT QCharH QChar_Create5(uint rc);
C_EXPORT QCharH QChar_Create6(int rc);
C_EXPORT QCharH QChar_Create7(QChar::SpecialCharacter s);
C_EXPORT QCharH QChar_Create8(char c);
C_EXPORT QChar::Category QChar_category(QCharH handle);
C_EXPORT QChar::Direction QChar_direction(QCharH handle);
C_EXPORT QChar::JoiningType QChar_joining(QCharH handle);
C_EXPORT unsigned char QChar_combiningClass(QCharH handle);
C_EXPORT void QChar_mirroredChar(QCharH handle, PWideChar retval);
C_EXPORT bool QChar_hasMirrored(QCharH handle);
C_EXPORT void QChar_decomposition(QCharH handle, PWideString retval);
C_EXPORT QChar::Decomposition QChar_decompositionTag(QCharH handle);
C_EXPORT int QChar_digitValue(QCharH handle);
C_EXPORT void QChar_toLower(QCharH handle, PWideChar retval);
C_EXPORT void QChar_toUpper(QCharH handle, PWideChar retval);
C_EXPORT void QChar_toTitleCase(QCharH handle, PWideChar retval);
C_EXPORT void QChar_toCaseFolded(QCharH handle, PWideChar retval);
C_EXPORT QChar::Script QChar_script(QCharH handle);
C_EXPORT QChar::UnicodeVersion QChar_unicodeVersion(QCharH handle);
C_EXPORT char QChar_toLatin1(QCharH handle);
C_EXPORT ushort QChar_unicode(QCharH handle);
C_EXPORT void QChar_fromLatin1(PWideChar retval, char c);
C_EXPORT bool QChar_isNull(QCharH handle);
C_EXPORT bool QChar_isPrint(QCharH handle);
C_EXPORT bool QChar_isSpace(QCharH handle);
C_EXPORT bool QChar_isMark(QCharH handle);
C_EXPORT bool QChar_isPunct(QCharH handle);
C_EXPORT bool QChar_isSymbol(QCharH handle);
C_EXPORT bool QChar_isLetter(QCharH handle);
C_EXPORT bool QChar_isNumber(QCharH handle);
C_EXPORT bool QChar_isLetterOrNumber(QCharH handle);
C_EXPORT bool QChar_isDigit(QCharH handle);
C_EXPORT bool QChar_isLower(QCharH handle);
C_EXPORT bool QChar_isUpper(QCharH handle);
C_EXPORT bool QChar_isTitleCase(QCharH handle);
C_EXPORT bool QChar_isNonCharacter(QCharH handle);
C_EXPORT bool QChar_isHighSurrogate(QCharH handle);
C_EXPORT bool QChar_isLowSurrogate(QCharH handle);
C_EXPORT bool QChar_isSurrogate(QCharH handle);
C_EXPORT unsigned char QChar_cell(QCharH handle);
C_EXPORT unsigned char QChar_row(QCharH handle);
C_EXPORT void QChar_setCell(QCharH handle, unsigned char cell);
C_EXPORT void QChar_setRow(QCharH handle, unsigned char row);
C_EXPORT bool QChar_isNonCharacter2(uint ucs4);
C_EXPORT bool QChar_isHighSurrogate2(uint ucs4);
C_EXPORT bool QChar_isLowSurrogate2(uint ucs4);
C_EXPORT bool QChar_isSurrogate2(uint ucs4);
C_EXPORT bool QChar_requiresSurrogates(uint ucs4);
C_EXPORT uint QChar_surrogateToUcs4(ushort high, ushort low);
C_EXPORT uint QChar_surrogateToUcs42(PWideChar high, PWideChar low);
C_EXPORT ushort QChar_highSurrogate(uint ucs4);
C_EXPORT ushort QChar_lowSurrogate(uint ucs4);
C_EXPORT bool QChar_isSpace2(uint ucs4);
C_EXPORT bool QChar_isLetter2(uint ucs4);
C_EXPORT bool QChar_isNumber2(uint ucs4);
C_EXPORT bool QChar_isLetterOrNumber2(uint ucs4);
C_EXPORT bool QChar_isDigit2(uint ucs4);
C_EXPORT bool QChar_isLower2(uint ucs4);
C_EXPORT bool QChar_isUpper2(uint ucs4);
C_EXPORT bool QChar_isTitleCase2(uint ucs4);

#endif
