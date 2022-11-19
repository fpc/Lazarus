//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QFONTDATABASE_C_H
#define QFONTDATABASE_C_H

#include <QtGui>
#include "pascalbind.h"

C_EXPORT void QFontDatabase_writingSystems(PPtrIntArray retval);
C_EXPORT void QFontDatabase_writingSystems2(PPtrIntArray retval, PWideString family);
C_EXPORT void QFontDatabase_families(QStringListH retval, QFontDatabase::WritingSystem writingSystem);
C_EXPORT void QFontDatabase_styles(QStringListH retval, PWideString family);
C_EXPORT void QFontDatabase_pointSizes(PPtrIntArray retval, PWideString family, PWideString style);
C_EXPORT void QFontDatabase_smoothSizes(PPtrIntArray retval, PWideString family, PWideString style);
C_EXPORT void QFontDatabase_styleString(PWideString retval, const QFontH font);
C_EXPORT void QFontDatabase_styleString2(PWideString retval, const QFontInfoH fontInfo);
C_EXPORT void QFontDatabase_font(QFontH retval, PWideString family, PWideString style, int pointSize);
C_EXPORT bool QFontDatabase_isBitmapScalable(PWideString family, PWideString style);
C_EXPORT bool QFontDatabase_isSmoothlyScalable(PWideString family, PWideString style);
C_EXPORT bool QFontDatabase_isScalable(PWideString family, PWideString style);
C_EXPORT bool QFontDatabase_isFixedPitch(PWideString family, PWideString style);
C_EXPORT bool QFontDatabase_italic(PWideString family, PWideString style);
C_EXPORT bool QFontDatabase_bold(PWideString family, PWideString style);
C_EXPORT int QFontDatabase_weight(PWideString family, PWideString style);
C_EXPORT void QFontDatabase_writingSystemName(PWideString retval, QFontDatabase::WritingSystem writingSystem);
C_EXPORT void QFontDatabase_writingSystemSample(PWideString retval, QFontDatabase::WritingSystem writingSystem);
C_EXPORT int QFontDatabase_addApplicationFont(PWideString fileName);
C_EXPORT int QFontDatabase_addApplicationFontFromData(const QByteArrayH fontData);
C_EXPORT void QFontDatabase_applicationFontFamilies(QStringListH retval, int id);
C_EXPORT bool QFontDatabase_removeApplicationFont(int id);
C_EXPORT bool QFontDatabase_removeAllApplicationFonts();
C_EXPORT bool QFontDatabase_isPrivateFamily(PWideString family);
C_EXPORT void QFontDatabase_standardSizes(PPtrIntArray retval);
C_EXPORT void QFontDatabase_systemFont(QFontDatabase::SystemFont font, QFontH retval);

#endif
