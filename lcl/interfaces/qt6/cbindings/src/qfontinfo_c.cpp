//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qfontinfo_c.h"

QFontInfoH QFontInfo_Create(const QFontH AnonParam1)
{
	return (QFontInfoH) new QFontInfo(*(const QFont*)AnonParam1);
}

void QFontInfo_Destroy(QFontInfoH handle)
{
	delete (QFontInfo *)handle;
}

QFontInfoH QFontInfo_Create2(const QFontInfoH AnonParam1)
{
	return (QFontInfoH) new QFontInfo(*(const QFontInfo*)AnonParam1);
}

void QFontInfo_swap(QFontInfoH handle, QFontInfoH other)
{
	((QFontInfo *)handle)->swap(*(QFontInfo*)other);
}

void QFontInfo_family(QFontInfoH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QFontInfo *)handle)->family();
	copyQStringToPWideString(t_retval, retval);
}

void QFontInfo_styleName(QFontInfoH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QFontInfo *)handle)->styleName();
	copyQStringToPWideString(t_retval, retval);
}

int QFontInfo_pixelSize(QFontInfoH handle)
{
	return (int) ((QFontInfo *)handle)->pixelSize();
}

int QFontInfo_pointSize(QFontInfoH handle)
{
	return (int) ((QFontInfo *)handle)->pointSize();
}

qreal QFontInfo_pointSizeF(QFontInfoH handle)
{
	return (qreal) ((QFontInfo *)handle)->pointSizeF();
}

bool QFontInfo_italic(QFontInfoH handle)
{
	return (bool) ((QFontInfo *)handle)->italic();
}

QFont::Style QFontInfo_style(QFontInfoH handle)
{
	return (QFont::Style) ((QFontInfo *)handle)->style();
}

int QFontInfo_weight(QFontInfoH handle)
{
	return (int) ((QFontInfo *)handle)->weight();
}

bool QFontInfo_bold(QFontInfoH handle)
{
	return (bool) ((QFontInfo *)handle)->bold();
}

bool QFontInfo_underline(QFontInfoH handle)
{
	return (bool) ((QFontInfo *)handle)->underline();
}

bool QFontInfo_overline(QFontInfoH handle)
{
	return (bool) ((QFontInfo *)handle)->overline();
}

bool QFontInfo_strikeOut(QFontInfoH handle)
{
	return (bool) ((QFontInfo *)handle)->strikeOut();
}

bool QFontInfo_fixedPitch(QFontInfoH handle)
{
	return (bool) ((QFontInfo *)handle)->fixedPitch();
}

QFont::StyleHint QFontInfo_styleHint(QFontInfoH handle)
{
	return (QFont::StyleHint) ((QFontInfo *)handle)->styleHint();
}

bool QFontInfo_exactMatch(QFontInfoH handle)
{
	return (bool) ((QFontInfo *)handle)->exactMatch();
}

