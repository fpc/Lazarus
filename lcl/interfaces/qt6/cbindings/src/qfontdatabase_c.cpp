//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qfontdatabase_c.h"

void QFontDatabase_writingSystems(PPtrIntArray retval)
{
	QList<QFontDatabase::WritingSystem> t_retval;
	t_retval = QFontDatabase::writingSystems();
	copyQListTemplateToPtrIntArray(t_retval, retval);
}

void QFontDatabase_writingSystems2(PPtrIntArray retval, PWideString family)
{
	QList<QFontDatabase::WritingSystem> t_retval;
	QString t_family;
	copyPWideStringToQString(family, t_family);
	t_retval = QFontDatabase::writingSystems(t_family);
	copyQListTemplateToPtrIntArray(t_retval, retval);
}

void QFontDatabase_families(QStringListH retval, QFontDatabase::WritingSystem writingSystem)
{
	*(QStringList *)retval = QFontDatabase::families(writingSystem);
}

void QFontDatabase_styles(QStringListH retval, PWideString family)
{
	QString t_family;
	copyPWideStringToQString(family, t_family);
	*(QStringList *)retval = QFontDatabase::styles(t_family);
}

void QFontDatabase_pointSizes(PPtrIntArray retval, PWideString family, PWideString style)
{
	QList<int> t_retval;
	QString t_family;
	QString t_style;
	copyPWideStringToQString(family, t_family);
	copyPWideStringToQString(style, t_style);
	t_retval = QFontDatabase::pointSizes(t_family, t_style);
	copyQListTemplateToPtrIntArray(t_retval, retval);
}

void QFontDatabase_smoothSizes(PPtrIntArray retval, PWideString family, PWideString style)
{
	QList<int> t_retval;
	QString t_family;
	QString t_style;
	copyPWideStringToQString(family, t_family);
	copyPWideStringToQString(style, t_style);
	t_retval = QFontDatabase::smoothSizes(t_family, t_style);
	copyQListTemplateToPtrIntArray(t_retval, retval);
}

void QFontDatabase_styleString(PWideString retval, const QFontH font)
{
	QString t_retval;
	t_retval = QFontDatabase::styleString(*(const QFont*)font);
	copyQStringToPWideString(t_retval, retval);
}

void QFontDatabase_styleString2(PWideString retval, const QFontInfoH fontInfo)
{
	QString t_retval;
	t_retval = QFontDatabase::styleString(*(const QFontInfo*)fontInfo);
	copyQStringToPWideString(t_retval, retval);
}

void QFontDatabase_font(QFontH retval, PWideString family, PWideString style, int pointSize)
{
	QString t_family;
	QString t_style;
	copyPWideStringToQString(family, t_family);
	copyPWideStringToQString(style, t_style);
	*(QFont *)retval = QFontDatabase::font(t_family, t_style, pointSize);
}

bool QFontDatabase_isBitmapScalable(PWideString family, PWideString style)
{
	QString t_family;
	QString t_style;
	copyPWideStringToQString(family, t_family);
	copyPWideStringToQString(style, t_style);
	return (bool) QFontDatabase::isBitmapScalable(t_family, t_style);
}

bool QFontDatabase_isSmoothlyScalable(PWideString family, PWideString style)
{
	QString t_family;
	QString t_style;
	copyPWideStringToQString(family, t_family);
	copyPWideStringToQString(style, t_style);
	return (bool) QFontDatabase::isSmoothlyScalable(t_family, t_style);
}

bool QFontDatabase_isScalable(PWideString family, PWideString style)
{
	QString t_family;
	QString t_style;
	copyPWideStringToQString(family, t_family);
	copyPWideStringToQString(style, t_style);
	return (bool) QFontDatabase::isScalable(t_family, t_style);
}

bool QFontDatabase_isFixedPitch(PWideString family, PWideString style)
{
	QString t_family;
	QString t_style;
	copyPWideStringToQString(family, t_family);
	copyPWideStringToQString(style, t_style);
	return (bool) QFontDatabase::isFixedPitch(t_family, t_style);
}

bool QFontDatabase_italic(PWideString family, PWideString style)
{
	QString t_family;
	QString t_style;
	copyPWideStringToQString(family, t_family);
	copyPWideStringToQString(style, t_style);
	return (bool) QFontDatabase::italic(t_family, t_style);
}

bool QFontDatabase_bold(PWideString family, PWideString style)
{
	QString t_family;
	QString t_style;
	copyPWideStringToQString(family, t_family);
	copyPWideStringToQString(style, t_style);
	return (bool) QFontDatabase::bold(t_family, t_style);
}

int QFontDatabase_weight(PWideString family, PWideString style)
{
	QString t_family;
	QString t_style;
	copyPWideStringToQString(family, t_family);
	copyPWideStringToQString(style, t_style);
	return (int) QFontDatabase::weight(t_family, t_style);
}

void QFontDatabase_writingSystemName(PWideString retval, QFontDatabase::WritingSystem writingSystem)
{
	QString t_retval;
	t_retval = QFontDatabase::writingSystemName(writingSystem);
	copyQStringToPWideString(t_retval, retval);
}

void QFontDatabase_writingSystemSample(PWideString retval, QFontDatabase::WritingSystem writingSystem)
{
	QString t_retval;
	t_retval = QFontDatabase::writingSystemSample(writingSystem);
	copyQStringToPWideString(t_retval, retval);
}

int QFontDatabase_addApplicationFont(PWideString fileName)
{
	QString t_fileName;
	copyPWideStringToQString(fileName, t_fileName);
	return (int) QFontDatabase::addApplicationFont(t_fileName);
}

int QFontDatabase_addApplicationFontFromData(const QByteArrayH fontData)
{
	return (int) QFontDatabase::addApplicationFontFromData(*(const QByteArray*)fontData);
}

void QFontDatabase_applicationFontFamilies(QStringListH retval, int id)
{
	*(QStringList *)retval = QFontDatabase::applicationFontFamilies(id);
}

bool QFontDatabase_removeApplicationFont(int id)
{
	return (bool) QFontDatabase::removeApplicationFont(id);
}

bool QFontDatabase_removeAllApplicationFonts()
{
	return (bool) QFontDatabase::removeAllApplicationFonts();
}

bool QFontDatabase_isPrivateFamily(PWideString family)
{
	QString t_family;
	copyPWideStringToQString(family, t_family);
  return (bool) QFontDatabase::isPrivateFamily(t_family);
}

void QFontDatabase_standardSizes(PPtrIntArray retval)
{
	QList<int> t_retval;
	t_retval = QFontDatabase::standardSizes();
	copyQListTemplateToPtrIntArray(t_retval, retval);
}

void QFontDatabase_systemFont(QFontDatabase::SystemFont font, QFontH retval)
{
	*(QFont *)retval = QFontDatabase::systemFont(font);
}

