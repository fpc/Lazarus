//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QFONT_C_H
#define QFONT_C_H

#include <QtGui>
#include "pascalbind.h"

C_EXPORT QFontH QFont_Create();
C_EXPORT void QFont_Destroy(QFontH handle);
C_EXPORT QFontH QFont_Create2(PWideString family, int pointSize, int weight, bool italic);
C_EXPORT QFontH QFont_Create3(const QFontH AnonParam1, QPaintDeviceH pd);
C_EXPORT QFontH QFont_Create4(const QFontH AnonParam1);
C_EXPORT void QFont_swap(QFontH handle, QFontH other);
C_EXPORT void QFont_family(QFontH handle, PWideString retval);
C_EXPORT void QFont_setFamily(QFontH handle, PWideString AnonParam1);
C_EXPORT void QFont_styleName(QFontH handle, PWideString retval);
C_EXPORT void QFont_setStyleName(QFontH handle, PWideString AnonParam1);
C_EXPORT int QFont_pointSize(QFontH handle);
C_EXPORT void QFont_setPointSize(QFontH handle, int AnonParam1);
C_EXPORT qreal QFont_pointSizeF(QFontH handle);
C_EXPORT void QFont_setPointSizeF(QFontH handle, qreal AnonParam1);
C_EXPORT int QFont_pixelSize(QFontH handle);
C_EXPORT void QFont_setPixelSize(QFontH handle, int AnonParam1);
C_EXPORT QFont::Weight QFont_weight(QFontH handle);
C_EXPORT void QFont_setWeight(QFontH handle, QFont::Weight AnonParam1);
C_EXPORT bool QFont_bold(QFontH handle);
C_EXPORT void QFont_setBold(QFontH handle, bool AnonParam1);
C_EXPORT void QFont_setStyle(QFontH handle, QFont::Style style);
C_EXPORT QFont::Style QFont_style(QFontH handle);
C_EXPORT bool QFont_italic(QFontH handle);
C_EXPORT void QFont_setItalic(QFontH handle, bool b);
C_EXPORT bool QFont_underline(QFontH handle);
C_EXPORT void QFont_setUnderline(QFontH handle, bool AnonParam1);
C_EXPORT bool QFont_overline(QFontH handle);
C_EXPORT void QFont_setOverline(QFontH handle, bool AnonParam1);
C_EXPORT bool QFont_strikeOut(QFontH handle);
C_EXPORT void QFont_setStrikeOut(QFontH handle, bool AnonParam1);
C_EXPORT bool QFont_fixedPitch(QFontH handle);
C_EXPORT void QFont_setFixedPitch(QFontH handle, bool AnonParam1);
C_EXPORT bool QFont_kerning(QFontH handle);
C_EXPORT void QFont_setKerning(QFontH handle, bool AnonParam1);
C_EXPORT QFont::StyleHint QFont_styleHint(QFontH handle);
C_EXPORT QFont::StyleStrategy QFont_styleStrategy(QFontH handle);
C_EXPORT void QFont_setStyleHint(QFontH handle, QFont::StyleHint AnonParam1, QFont::StyleStrategy AnonParam2);
C_EXPORT void QFont_setStyleStrategy(QFontH handle, QFont::StyleStrategy s);
C_EXPORT int QFont_stretch(QFontH handle);
C_EXPORT void QFont_setStretch(QFontH handle, int AnonParam1);
C_EXPORT qreal QFont_letterSpacing(QFontH handle);
C_EXPORT QFont::SpacingType QFont_letterSpacingType(QFontH handle);
C_EXPORT void QFont_setLetterSpacing(QFontH handle, QFont::SpacingType type, qreal spacing);
C_EXPORT qreal QFont_wordSpacing(QFontH handle);
C_EXPORT void QFont_setWordSpacing(QFontH handle, qreal spacing);
C_EXPORT void QFont_setCapitalization(QFontH handle, QFont::Capitalization AnonParam1);
C_EXPORT QFont::Capitalization QFont_capitalization(QFontH handle);
C_EXPORT void QFont_setHintingPreference(QFontH handle, QFont::HintingPreference hintingPreference);
C_EXPORT QFont::HintingPreference QFont_hintingPreference(QFontH handle);
C_EXPORT bool QFont_exactMatch(QFontH handle);
C_EXPORT bool QFont_isCopyOf(QFontH handle, const QFontH AnonParam1);
C_EXPORT void QFont_key(QFontH handle, PWideString retval);
C_EXPORT void QFont_toString(QFontH handle, PWideString retval);
C_EXPORT bool QFont_fromString(QFontH handle, PWideString AnonParam1);
C_EXPORT void QFont_substitute(PWideString retval, PWideString AnonParam1);
C_EXPORT void QFont_substitutes(QStringListH retval, PWideString AnonParam1);
C_EXPORT void QFont_substitutions(QStringListH retval);
C_EXPORT void QFont_insertSubstitution(PWideString AnonParam1, PWideString AnonParam2);
C_EXPORT void QFont_insertSubstitutions(PWideString AnonParam1, const QStringListH AnonParam2);
C_EXPORT void QFont_removeSubstitutions(PWideString AnonParam1);
C_EXPORT void QFont_initialize();
C_EXPORT void QFont_cleanup();
C_EXPORT void QFont_cacheStatistics();
C_EXPORT void QFont_defaultFamily(QFontH handle, PWideString retval);
C_EXPORT void QFont_resolve(QFontH handle, QFontH retval, const QFontH AnonParam1);
C_EXPORT void QFont_families(QFontH handle, QStringListH retval);
C_EXPORT void QFont_setFamilies(QFontH handle, QStringListH families);

#endif
