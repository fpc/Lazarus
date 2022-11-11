//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qchar_c.h"

QCharH QChar_Create()
{
	return (QCharH) new QChar();
}

void QChar_Destroy(QCharH handle)
{
	delete (QChar *)handle;
}

QCharH QChar_Create2(ushort rc)
{
	return (QCharH) new QChar(rc);
}

QCharH QChar_Create4(short rc)
{
	return (QCharH) new QChar(rc);
}

QCharH QChar_Create5(uint rc)
{
	return (QCharH) new QChar(rc);
}

QCharH QChar_Create6(int rc)
{
	return (QCharH) new QChar(rc);
}

QCharH QChar_Create7(QChar::SpecialCharacter s)
{
	return (QCharH) new QChar(s);
}

QCharH QChar_Create8(char c)
{
	return (QCharH) new QChar(c);
}

QChar::Category QChar_category(QCharH handle)
{
	return (QChar::Category) ((QChar *)handle)->category();
}

QChar::Direction QChar_direction(QCharH handle)
{
	return (QChar::Direction) ((QChar *)handle)->direction();
}

QChar::JoiningType QChar_joining(QCharH handle)
{
	return (QChar::JoiningType) ((QChar *)handle)->joiningType();
}

unsigned char QChar_combiningClass(QCharH handle)
{
	return (unsigned char) ((QChar *)handle)->combiningClass();
}

void QChar_mirroredChar(QCharH handle, PWideChar retval)
{
	*(QChar *)retval = ((QChar *)handle)->mirroredChar();
}

bool QChar_hasMirrored(QCharH handle)
{
	return (bool) ((QChar *)handle)->hasMirrored();
}

void QChar_decomposition(QCharH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QChar *)handle)->decomposition();
	copyQStringToPWideString(t_retval, retval);
}

QChar::Decomposition QChar_decompositionTag(QCharH handle)
{
	return (QChar::Decomposition) ((QChar *)handle)->decompositionTag();
}

int QChar_digitValue(QCharH handle)
{
	return (int) ((QChar *)handle)->digitValue();
}

void QChar_toLower(QCharH handle, PWideChar retval)
{
	*(QChar *)retval = ((QChar *)handle)->toLower();
}

void QChar_toUpper(QCharH handle, PWideChar retval)
{
	*(QChar *)retval = ((QChar *)handle)->toUpper();
}

void QChar_toTitleCase(QCharH handle, PWideChar retval)
{
	*(QChar *)retval = ((QChar *)handle)->toTitleCase();
}

void QChar_toCaseFolded(QCharH handle, PWideChar retval)
{
	*(QChar *)retval = ((QChar *)handle)->toCaseFolded();
}

QChar::Script QChar_script(QCharH handle)
{
	return (QChar::Script) ((QChar *)handle)->script();
}

QChar::UnicodeVersion QChar_unicodeVersion(QCharH handle)
{
	return (QChar::UnicodeVersion) ((QChar *)handle)->unicodeVersion();
}

char QChar_toLatin1(QCharH handle)
{
	return (char) ((QChar *)handle)->toLatin1();
}

ushort QChar_unicode(QCharH handle)
{
	return (ushort) ((QChar *)handle)->unicode();
}

void QChar_fromLatin1(PWideChar retval, char c)
{
	*(QChar *)retval = QChar::fromLatin1(c);
}

bool QChar_isNull(QCharH handle)
{
	return (bool) ((QChar *)handle)->isNull();
}

bool QChar_isPrint(QCharH handle)
{
	return (bool) ((QChar *)handle)->isPrint();
}

bool QChar_isSpace(QCharH handle)
{
	return (bool) ((QChar *)handle)->isSpace();
}

bool QChar_isMark(QCharH handle)
{
	return (bool) ((QChar *)handle)->isMark();
}

bool QChar_isPunct(QCharH handle)
{
	return (bool) ((QChar *)handle)->isPunct();
}

bool QChar_isSymbol(QCharH handle)
{
	return (bool) ((QChar *)handle)->isSymbol();
}

bool QChar_isLetter(QCharH handle)
{
	return (bool) ((QChar *)handle)->isLetter();
}

bool QChar_isNumber(QCharH handle)
{
	return (bool) ((QChar *)handle)->isNumber();
}

bool QChar_isLetterOrNumber(QCharH handle)
{
	return (bool) ((QChar *)handle)->isLetterOrNumber();
}

bool QChar_isDigit(QCharH handle)
{
	return (bool) ((QChar *)handle)->isDigit();
}

bool QChar_isLower(QCharH handle)
{
	return (bool) ((QChar *)handle)->isLower();
}

bool QChar_isUpper(QCharH handle)
{
	return (bool) ((QChar *)handle)->isUpper();
}

bool QChar_isTitleCase(QCharH handle)
{
	return (bool) ((QChar *)handle)->isTitleCase();
}

bool QChar_isNonCharacter(QCharH handle)
{
	return (bool) ((QChar *)handle)->isNonCharacter();
}

bool QChar_isHighSurrogate(QCharH handle)
{
	return (bool) ((QChar *)handle)->isHighSurrogate();
}

bool QChar_isLowSurrogate(QCharH handle)
{
	return (bool) ((QChar *)handle)->isLowSurrogate();
}

bool QChar_isSurrogate(QCharH handle)
{
	return (bool) ((QChar *)handle)->isSurrogate();
}

unsigned char QChar_cell(QCharH handle)
{
	return (unsigned char) ((QChar *)handle)->cell();
}

unsigned char QChar_row(QCharH handle)
{
	return (unsigned char) ((QChar *)handle)->row();
}

void QChar_setCell(QCharH handle, unsigned char cell)
{
	((QChar *)handle)->setCell((uchar)cell);
}

void QChar_setRow(QCharH handle, unsigned char row)
{
	((QChar *)handle)->setRow((uchar)row);
}

bool QChar_isNonCharacter2(uint ucs4)
{
	return (bool) QChar::isNonCharacter(ucs4);
}

bool QChar_isHighSurrogate2(uint ucs4)
{
	return (bool) QChar::isHighSurrogate(ucs4);
}

bool QChar_isLowSurrogate2(uint ucs4)
{
	return (bool) QChar::isLowSurrogate(ucs4);
}

bool QChar_isSurrogate2(uint ucs4)
{
	return (bool) QChar::isSurrogate(ucs4);
}

bool QChar_requiresSurrogates(uint ucs4)
{
	return (bool) QChar::requiresSurrogates(ucs4);
}

uint QChar_surrogateToUcs4(ushort high, ushort low)
{
	return (uint) QChar::surrogateToUcs4(high, low);
}

uint QChar_surrogateToUcs42(PWideChar high, PWideChar low)
{
	return (uint) QChar::surrogateToUcs4(*(QChar *)high, *(QChar *)low);
}

ushort QChar_highSurrogate(uint ucs4)
{
	return (ushort) QChar::highSurrogate(ucs4);
}

ushort QChar_lowSurrogate(uint ucs4)
{
	return (ushort) QChar::lowSurrogate(ucs4);
}

bool QChar_isSpace2(uint ucs4)
{
	return (bool) QChar::isSpace(ucs4);
}

bool QChar_isLetter2(uint ucs4)
{
	return (bool) QChar::isLetter(ucs4);
}

bool QChar_isNumber2(uint ucs4)
{
	return (bool) QChar::isNumber(ucs4);
}

bool QChar_isLetterOrNumber2(uint ucs4)
{
	return (bool) QChar::isLetterOrNumber(ucs4);
}

bool QChar_isDigit2(uint ucs4)
{
	return (bool) QChar::isDigit(ucs4);
}

bool QChar_isLower2(uint ucs4)
{
	return (bool) QChar::isLower(ucs4);
}

bool QChar_isUpper2(uint ucs4)
{
	return (bool) QChar::isUpper(ucs4);
}

bool QChar_isTitleCase2(uint ucs4)
{
	return (bool) QChar::isTitleCase(ucs4);
}

