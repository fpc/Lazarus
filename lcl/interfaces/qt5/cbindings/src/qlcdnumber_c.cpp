//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qlcdnumber_c.h"

QLCDNumberH QLCDNumber_Create(QWidgetH parent)
{
	return (QLCDNumberH) new QLCDNumber((QWidget*)parent);
}

void QLCDNumber_Destroy(QLCDNumberH handle)
{
	delete (QLCDNumber *)handle;
}

QLCDNumberH QLCDNumber_Create2(uint numDigits, QWidgetH parent)
{
	return (QLCDNumberH) new QLCDNumber(numDigits, (QWidget*)parent);
}

bool QLCDNumber_smallDecimalPoint(QLCDNumberH handle)
{
	return (bool) ((QLCDNumber *)handle)->smallDecimalPoint();
}

int QLCDNumber_digitCount(QLCDNumberH handle)
{
	return (int) ((QLCDNumber *)handle)->digitCount();
}

void QLCDNumber_setDigitCount(QLCDNumberH handle, int nDigits)
{
	((QLCDNumber *)handle)->setDigitCount(nDigits);
}

bool QLCDNumber_checkOverflow(QLCDNumberH handle, double num)
{
	return (bool) ((QLCDNumber *)handle)->checkOverflow(num);
}

bool QLCDNumber_checkOverflow2(QLCDNumberH handle, int num)
{
	return (bool) ((QLCDNumber *)handle)->checkOverflow(num);
}

QLCDNumber::Mode QLCDNumber_mode(QLCDNumberH handle)
{
	return (QLCDNumber::Mode) ((QLCDNumber *)handle)->mode();
}

void QLCDNumber_setMode(QLCDNumberH handle, QLCDNumber::Mode AnonParam1)
{
	((QLCDNumber *)handle)->setMode(AnonParam1);
}

QLCDNumber::SegmentStyle QLCDNumber_segmentStyle(QLCDNumberH handle)
{
	return (QLCDNumber::SegmentStyle) ((QLCDNumber *)handle)->segmentStyle();
}

void QLCDNumber_setSegmentStyle(QLCDNumberH handle, QLCDNumber::SegmentStyle AnonParam1)
{
	((QLCDNumber *)handle)->setSegmentStyle(AnonParam1);
}

double QLCDNumber_value(QLCDNumberH handle)
{
	return (double) ((QLCDNumber *)handle)->value();
}

int QLCDNumber_intValue(QLCDNumberH handle)
{
	return (int) ((QLCDNumber *)handle)->intValue();
}

void QLCDNumber_sizeHint(QLCDNumberH handle, PSize retval)
{
	*(QSize *)retval = ((QLCDNumber *)handle)->sizeHint();
}

void QLCDNumber_display(QLCDNumberH handle, PWideString str)
{
	QString t_str;
	copyPWideStringToQString(str, t_str);
	((QLCDNumber *)handle)->display(t_str);
}

void QLCDNumber_display2(QLCDNumberH handle, int num)
{
	((QLCDNumber *)handle)->display(num);
}

void QLCDNumber_display3(QLCDNumberH handle, double num)
{
	((QLCDNumber *)handle)->display(num);
}

void QLCDNumber_setHexMode(QLCDNumberH handle)
{
	((QLCDNumber *)handle)->setHexMode();
}

void QLCDNumber_setDecMode(QLCDNumberH handle)
{
	((QLCDNumber *)handle)->setDecMode();
}

void QLCDNumber_setOctMode(QLCDNumberH handle)
{
	((QLCDNumber *)handle)->setOctMode();
}

void QLCDNumber_setBinMode(QLCDNumberH handle)
{
	((QLCDNumber *)handle)->setBinMode();
}

void QLCDNumber_setSmallDecimalPoint(QLCDNumberH handle, bool AnonParam1)
{
	((QLCDNumber *)handle)->setSmallDecimalPoint(AnonParam1);
}

