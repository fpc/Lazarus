//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QLCDNUMBER_C_H
#define QLCDNUMBER_C_H

#include <QtWidgets>
#include "pascalbind.h"

C_EXPORT QLCDNumberH QLCDNumber_Create(QWidgetH parent);
C_EXPORT void QLCDNumber_Destroy(QLCDNumberH handle);
C_EXPORT QLCDNumberH QLCDNumber_Create2(uint numDigits, QWidgetH parent);
C_EXPORT bool QLCDNumber_smallDecimalPoint(QLCDNumberH handle);
C_EXPORT int QLCDNumber_digitCount(QLCDNumberH handle);
C_EXPORT void QLCDNumber_setDigitCount(QLCDNumberH handle, int nDigits);
C_EXPORT bool QLCDNumber_checkOverflow(QLCDNumberH handle, double num);
C_EXPORT bool QLCDNumber_checkOverflow2(QLCDNumberH handle, int num);
C_EXPORT QLCDNumber::Mode QLCDNumber_mode(QLCDNumberH handle);
C_EXPORT void QLCDNumber_setMode(QLCDNumberH handle, QLCDNumber::Mode AnonParam1);
C_EXPORT QLCDNumber::SegmentStyle QLCDNumber_segmentStyle(QLCDNumberH handle);
C_EXPORT void QLCDNumber_setSegmentStyle(QLCDNumberH handle, QLCDNumber::SegmentStyle AnonParam1);
C_EXPORT double QLCDNumber_value(QLCDNumberH handle);
C_EXPORT int QLCDNumber_intValue(QLCDNumberH handle);
C_EXPORT void QLCDNumber_sizeHint(QLCDNumberH handle, PSize retval);
C_EXPORT void QLCDNumber_display(QLCDNumberH handle, PWideString str);
C_EXPORT void QLCDNumber_display2(QLCDNumberH handle, int num);
C_EXPORT void QLCDNumber_display3(QLCDNumberH handle, double num);
C_EXPORT void QLCDNumber_setHexMode(QLCDNumberH handle);
C_EXPORT void QLCDNumber_setDecMode(QLCDNumberH handle);
C_EXPORT void QLCDNumber_setOctMode(QLCDNumberH handle);
C_EXPORT void QLCDNumber_setBinMode(QLCDNumberH handle);
C_EXPORT void QLCDNumber_setSmallDecimalPoint(QLCDNumberH handle, bool AnonParam1);

#endif
