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

C_EXPORT void QFontDatabase_standardSizes(PPtrIntArray retval);
C_EXPORT QFontDatabaseH QFontDatabase_Create();
C_EXPORT void QFontDatabase_Destroy(QFontDatabaseH handle);
C_EXPORT void QFontDatabase_writingSystems(QFontDatabaseH handle, PPtrIntArray retval);
C_EXPORT void QFontDatabase_writingSystems2(QFontDatabaseH handle, PPtrIntArray retval, PWideString family);
C_EXPORT void QFontDatabase_families(QFontDatabaseH handle, QStringListH retval, QFontDatabase::WritingSystem writingSystem);
C_EXPORT void QFontDatabase_styles(QFontDatabaseH handle, QStringListH retval, PWideString family);
C_EXPORT void QFontDatabase_pointSizes(QFontDatabaseH handle, PPtrIntArray retval, PWideString family, PWideString style);
C_EXPORT void QFontDatabase_smoothSizes(QFontDatabaseH handle, PPtrIntArray retval, PWideString family, PWideString style);
C_EXPORT void QFontDatabase_styleString(QFontDatabaseH handle, PWideString retval, const QFontH font);
C_EXPORT void QFontDatabase_styleString2(QFontDatabaseH handle, PWideString retval, const QFontInfoH fontInfo);
C_EXPORT void QFontDatabase_font(QFontDatabaseH handle, QFontH retval, PWideString family, PWideString style, int pointSize);
C_EXPORT bool QFontDatabase_isBitmapScalable(QFontDatabaseH handle, PWideString family, PWideString style);
C_EXPORT bool QFontDatabase_isSmoothlyScalable(QFontDatabaseH handle, PWideString family, PWideString style);
C_EXPORT bool QFontDatabase_isScalable(QFontDatabaseH handle, PWideString family, PWideString style);
C_EXPORT bool QFontDatabase_isFixedPitch(QFontDatabaseH handle, PWideString family, PWideString style);
C_EXPORT bool QFontDatabase_italic(QFontDatabaseH handle, PWideString family, PWideString style);
C_EXPORT bool QFontDatabase_bold(QFontDatabaseH handle, PWideString family, PWideString style);
C_EXPORT int QFontDatabase_weight(QFontDatabaseH handle, PWideString family, PWideString style);
C_EXPORT bool QFontDatabase_hasFamily(QFontDatabaseH handle, PWideString family);
C_EXPORT void QFontDatabase_writingSystemName(PWideString retval, QFontDatabase::WritingSystem writingSystem);
C_EXPORT void QFontDatabase_writingSystemSample(PWideString retval, QFontDatabase::WritingSystem writingSystem);
C_EXPORT int QFontDatabase_addApplicationFont(PWideString fileName);
C_EXPORT int QFontDatabase_addApplicationFontFromData(const QByteArrayH fontData);
C_EXPORT void QFontDatabase_applicationFontFamilies(QStringListH retval, int id);
C_EXPORT bool QFontDatabase_removeApplicationFont(int id);
C_EXPORT bool QFontDatabase_removeAllApplicationFonts();

#endif
