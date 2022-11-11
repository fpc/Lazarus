//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qfont_c.h"

QFontH QFont_Create()
{
	return (QFontH) new QFont();
}

void QFont_Destroy(QFontH handle)
{
	delete (QFont *)handle;
}

QFontH QFont_Create2(PWideString family, int pointSize, int weight, bool italic)
{
	QString t_family;
	copyPWideStringToQString(family, t_family);
	return (QFontH) new QFont(t_family, pointSize, weight, italic);
}

QFontH QFont_Create3(const QFontH AnonParam1, QPaintDeviceH pd)
{
	return (QFontH) new QFont(*(const QFont*)AnonParam1, (QPaintDevice*)pd);
}

QFontH QFont_Create4(const QFontH AnonParam1)
{
	return (QFontH) new QFont(*(const QFont*)AnonParam1);
}

void QFont_swap(QFontH handle, QFontH other)
{
	((QFont *)handle)->swap(*(QFont*)other);
}

void QFont_family(QFontH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QFont *)handle)->family();
	copyQStringToPWideString(t_retval, retval);
}

void QFont_setFamily(QFontH handle, PWideString AnonParam1)
{
	QString t_AnonParam1;
	copyPWideStringToQString(AnonParam1, t_AnonParam1);
	((QFont *)handle)->setFamily(t_AnonParam1);
}

void QFont_styleName(QFontH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QFont *)handle)->styleName();
	copyQStringToPWideString(t_retval, retval);
}

void QFont_setStyleName(QFontH handle, PWideString AnonParam1)
{
	QString t_AnonParam1;
	copyPWideStringToQString(AnonParam1, t_AnonParam1);
	((QFont *)handle)->setStyleName(t_AnonParam1);
}

int QFont_pointSize(QFontH handle)
{
	return (int) ((QFont *)handle)->pointSize();
}

void QFont_setPointSize(QFontH handle, int AnonParam1)
{
	((QFont *)handle)->setPointSize(AnonParam1);
}

qreal QFont_pointSizeF(QFontH handle)
{
	return (qreal) ((QFont *)handle)->pointSizeF();
}

void QFont_setPointSizeF(QFontH handle, qreal AnonParam1)
{
	((QFont *)handle)->setPointSizeF(AnonParam1);
}

int QFont_pixelSize(QFontH handle)
{
	return (int) ((QFont *)handle)->pixelSize();
}

void QFont_setPixelSize(QFontH handle, int AnonParam1)
{
	((QFont *)handle)->setPixelSize(AnonParam1);
}

QFont::Weight QFont_weight(QFontH handle)
{
	return (QFont::Weight) ((QFont *)handle)->weight();
}

void QFont_setWeight(QFontH handle, QFont::Weight AnonParam1)
{
	((QFont *)handle)->setWeight(AnonParam1);
}

bool QFont_bold(QFontH handle)
{
	return (bool) ((QFont *)handle)->bold();
}

void QFont_setBold(QFontH handle, bool AnonParam1)
{
	((QFont *)handle)->setBold(AnonParam1);
}

void QFont_setStyle(QFontH handle, QFont::Style style)
{
	((QFont *)handle)->setStyle(style);
}

QFont::Style QFont_style(QFontH handle)
{
	return (QFont::Style) ((QFont *)handle)->style();
}

bool QFont_italic(QFontH handle)
{
	return (bool) ((QFont *)handle)->italic();
}

void QFont_setItalic(QFontH handle, bool b)
{
	((QFont *)handle)->setItalic(b);
}

bool QFont_underline(QFontH handle)
{
	return (bool) ((QFont *)handle)->underline();
}

void QFont_setUnderline(QFontH handle, bool AnonParam1)
{
	((QFont *)handle)->setUnderline(AnonParam1);
}

bool QFont_overline(QFontH handle)
{
	return (bool) ((QFont *)handle)->overline();
}

void QFont_setOverline(QFontH handle, bool AnonParam1)
{
	((QFont *)handle)->setOverline(AnonParam1);
}

bool QFont_strikeOut(QFontH handle)
{
	return (bool) ((QFont *)handle)->strikeOut();
}

void QFont_setStrikeOut(QFontH handle, bool AnonParam1)
{
	((QFont *)handle)->setStrikeOut(AnonParam1);
}

bool QFont_fixedPitch(QFontH handle)
{
	return (bool) ((QFont *)handle)->fixedPitch();
}

void QFont_setFixedPitch(QFontH handle, bool AnonParam1)
{
	((QFont *)handle)->setFixedPitch(AnonParam1);
}

bool QFont_kerning(QFontH handle)
{
	return (bool) ((QFont *)handle)->kerning();
}

void QFont_setKerning(QFontH handle, bool AnonParam1)
{
	((QFont *)handle)->setKerning(AnonParam1);
}

QFont::StyleHint QFont_styleHint(QFontH handle)
{
	return (QFont::StyleHint) ((QFont *)handle)->styleHint();
}

QFont::StyleStrategy QFont_styleStrategy(QFontH handle)
{
	return (QFont::StyleStrategy) ((QFont *)handle)->styleStrategy();
}

void QFont_setStyleHint(QFontH handle, QFont::StyleHint AnonParam1, QFont::StyleStrategy AnonParam2)
{
	((QFont *)handle)->setStyleHint(AnonParam1, AnonParam2);
}

void QFont_setStyleStrategy(QFontH handle, QFont::StyleStrategy s)
{
	((QFont *)handle)->setStyleStrategy(s);
}

int QFont_stretch(QFontH handle)
{
	return (int) ((QFont *)handle)->stretch();
}

void QFont_setStretch(QFontH handle, int AnonParam1)
{
	((QFont *)handle)->setStretch(AnonParam1);
}

qreal QFont_letterSpacing(QFontH handle)
{
	return (qreal) ((QFont *)handle)->letterSpacing();
}

QFont::SpacingType QFont_letterSpacingType(QFontH handle)
{
	return (QFont::SpacingType) ((QFont *)handle)->letterSpacingType();
}

void QFont_setLetterSpacing(QFontH handle, QFont::SpacingType type, qreal spacing)
{
	((QFont *)handle)->setLetterSpacing(type, spacing);
}

qreal QFont_wordSpacing(QFontH handle)
{
	return (qreal) ((QFont *)handle)->wordSpacing();
}

void QFont_setWordSpacing(QFontH handle, qreal spacing)
{
	((QFont *)handle)->setWordSpacing(spacing);
}

void QFont_setCapitalization(QFontH handle, QFont::Capitalization AnonParam1)
{
	((QFont *)handle)->setCapitalization(AnonParam1);
}

QFont::Capitalization QFont_capitalization(QFontH handle)
{
	return (QFont::Capitalization) ((QFont *)handle)->capitalization();
}

void QFont_setHintingPreference(QFontH handle, QFont::HintingPreference hintingPreference)
{
	((QFont *)handle)->setHintingPreference(hintingPreference);
}

QFont::HintingPreference QFont_hintingPreference(QFontH handle)
{
	return (QFont::HintingPreference) ((QFont *)handle)->hintingPreference();
}

bool QFont_exactMatch(QFontH handle)
{
	return (bool) ((QFont *)handle)->exactMatch();
}

bool QFont_isCopyOf(QFontH handle, const QFontH AnonParam1)
{
	return (bool) ((QFont *)handle)->isCopyOf(*(const QFont*)AnonParam1);
}

void QFont_key(QFontH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QFont *)handle)->key();
	copyQStringToPWideString(t_retval, retval);
}

void QFont_toString(QFontH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QFont *)handle)->toString();
	copyQStringToPWideString(t_retval, retval);
}

bool QFont_fromString(QFontH handle, PWideString AnonParam1)
{
	QString t_AnonParam1;
	copyPWideStringToQString(AnonParam1, t_AnonParam1);
	return (bool) ((QFont *)handle)->fromString(t_AnonParam1);
}

void QFont_substitute(PWideString retval, PWideString AnonParam1)
{
	QString t_retval;
	QString t_AnonParam1;
	copyPWideStringToQString(AnonParam1, t_AnonParam1);
	t_retval = QFont::substitute(t_AnonParam1);
	copyQStringToPWideString(t_retval, retval);
}

void QFont_substitutes(QStringListH retval, PWideString AnonParam1)
{
	QString t_AnonParam1;
	copyPWideStringToQString(AnonParam1, t_AnonParam1);
	*(QStringList *)retval = QFont::substitutes(t_AnonParam1);
}

void QFont_substitutions(QStringListH retval)
{
	*(QStringList *)retval = QFont::substitutions();
}

void QFont_insertSubstitution(PWideString AnonParam1, PWideString AnonParam2)
{
	QString t_AnonParam1;
	QString t_AnonParam2;
	copyPWideStringToQString(AnonParam1, t_AnonParam1);
	copyPWideStringToQString(AnonParam2, t_AnonParam2);
	QFont::insertSubstitution(t_AnonParam1, t_AnonParam2);
}

void QFont_insertSubstitutions(PWideString AnonParam1, const QStringListH AnonParam2)
{
	QString t_AnonParam1;
	copyPWideStringToQString(AnonParam1, t_AnonParam1);
	QFont::insertSubstitutions(t_AnonParam1, *(const QStringList*)AnonParam2);
}

void QFont_removeSubstitutions(PWideString AnonParam1)
{
	QString t_AnonParam1;
	copyPWideStringToQString(AnonParam1, t_AnonParam1);
	QFont::removeSubstitutions(t_AnonParam1);
}

void QFont_initialize()
{
	QFont::initialize();
}

void QFont_cleanup()
{
	QFont::cleanup();
}

void QFont_cacheStatistics()
{
	QFont::cacheStatistics();
}

void QFont_defaultFamily(QFontH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QFont *)handle)->defaultFamily();
	copyQStringToPWideString(t_retval, retval);
}

void QFont_resolve(QFontH handle, QFontH retval, const QFontH AnonParam1)
{
	*(QFont *)retval = ((QFont *)handle)->resolve(*(const QFont*)AnonParam1);
}

void QFont_families(QFontH handle, QStringListH retval)
{
  *(QStringList *)retval = ((QFont *)handle)->families();
}

void QFont_setFamilies(QFontH handle, QStringListH families)
{
  ((QFont *)handle)->setFamilies(*(const QStringList *)families);
}


