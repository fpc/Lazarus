//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QFONTINFO_C_H
#define QFONTINFO_C_H

#include <QtGui>
#include "pascalbind.h"

C_EXPORT QFontInfoH QFontInfo_Create(const QFontH AnonParam1);
C_EXPORT void QFontInfo_Destroy(QFontInfoH handle);
C_EXPORT QFontInfoH QFontInfo_Create2(const QFontInfoH AnonParam1);
C_EXPORT void QFontInfo_swap(QFontInfoH handle, QFontInfoH other);
C_EXPORT void QFontInfo_family(QFontInfoH handle, PWideString retval);
C_EXPORT void QFontInfo_styleName(QFontInfoH handle, PWideString retval);
C_EXPORT int QFontInfo_pixelSize(QFontInfoH handle);
C_EXPORT int QFontInfo_pointSize(QFontInfoH handle);
C_EXPORT qreal QFontInfo_pointSizeF(QFontInfoH handle);
C_EXPORT bool QFontInfo_italic(QFontInfoH handle);
C_EXPORT QFont::Style QFontInfo_style(QFontInfoH handle);
C_EXPORT int QFontInfo_weight(QFontInfoH handle);
C_EXPORT bool QFontInfo_bold(QFontInfoH handle);
C_EXPORT bool QFontInfo_underline(QFontInfoH handle);
C_EXPORT bool QFontInfo_overline(QFontInfoH handle);
C_EXPORT bool QFontInfo_strikeOut(QFontInfoH handle);
C_EXPORT bool QFontInfo_fixedPitch(QFontInfoH handle);
C_EXPORT QFont::StyleHint QFontInfo_styleHint(QFontInfoH handle);
C_EXPORT bool QFontInfo_exactMatch(QFontInfoH handle);

#endif
