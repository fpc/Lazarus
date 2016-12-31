//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QSPINBOX_C_H
#define QSPINBOX_C_H

#include <QtWidgets>
#include "pascalbind.h"

C_EXPORT QSpinBoxH QSpinBox_Create(QWidgetH parent);
C_EXPORT void QSpinBox_Destroy(QSpinBoxH handle);
C_EXPORT int QSpinBox_value(QSpinBoxH handle);
C_EXPORT void QSpinBox_prefix(QSpinBoxH handle, PWideString retval);
C_EXPORT void QSpinBox_setPrefix(QSpinBoxH handle, PWideString prefix);
C_EXPORT void QSpinBox_suffix(QSpinBoxH handle, PWideString retval);
C_EXPORT void QSpinBox_setSuffix(QSpinBoxH handle, PWideString suffix);
C_EXPORT void QSpinBox_cleanText(QSpinBoxH handle, PWideString retval);
C_EXPORT int QSpinBox_singleStep(QSpinBoxH handle);
C_EXPORT void QSpinBox_setSingleStep(QSpinBoxH handle, int val);
C_EXPORT int QSpinBox_minimum(QSpinBoxH handle);
C_EXPORT void QSpinBox_setMinimum(QSpinBoxH handle, int min);
C_EXPORT int QSpinBox_maximum(QSpinBoxH handle);
C_EXPORT void QSpinBox_setMaximum(QSpinBoxH handle, int max);
C_EXPORT void QSpinBox_setRange(QSpinBoxH handle, int min, int max);
C_EXPORT void QSpinBox_setValue(QSpinBoxH handle, int val);
C_EXPORT QDoubleSpinBoxH QDoubleSpinBox_Create(QWidgetH parent);
C_EXPORT void QDoubleSpinBox_Destroy(QDoubleSpinBoxH handle);
C_EXPORT double QDoubleSpinBox_value(QDoubleSpinBoxH handle);
C_EXPORT void QDoubleSpinBox_prefix(QDoubleSpinBoxH handle, PWideString retval);
C_EXPORT void QDoubleSpinBox_setPrefix(QDoubleSpinBoxH handle, PWideString prefix);
C_EXPORT void QDoubleSpinBox_suffix(QDoubleSpinBoxH handle, PWideString retval);
C_EXPORT void QDoubleSpinBox_setSuffix(QDoubleSpinBoxH handle, PWideString suffix);
C_EXPORT void QDoubleSpinBox_cleanText(QDoubleSpinBoxH handle, PWideString retval);
C_EXPORT double QDoubleSpinBox_singleStep(QDoubleSpinBoxH handle);
C_EXPORT void QDoubleSpinBox_setSingleStep(QDoubleSpinBoxH handle, double val);
C_EXPORT double QDoubleSpinBox_minimum(QDoubleSpinBoxH handle);
C_EXPORT void QDoubleSpinBox_setMinimum(QDoubleSpinBoxH handle, double min);
C_EXPORT double QDoubleSpinBox_maximum(QDoubleSpinBoxH handle);
C_EXPORT void QDoubleSpinBox_setMaximum(QDoubleSpinBoxH handle, double max);
C_EXPORT void QDoubleSpinBox_setRange(QDoubleSpinBoxH handle, double min, double max);
C_EXPORT int QDoubleSpinBox_decimals(QDoubleSpinBoxH handle);
C_EXPORT void QDoubleSpinBox_setDecimals(QDoubleSpinBoxH handle, int prec);
C_EXPORT QValidator::State QDoubleSpinBox_validate(QDoubleSpinBoxH handle, PWideString input, int* pos);
C_EXPORT double QDoubleSpinBox_valueFromText(QDoubleSpinBoxH handle, PWideString text);
C_EXPORT void QDoubleSpinBox_textFromValue(QDoubleSpinBoxH handle, PWideString retval, double val);
C_EXPORT void QDoubleSpinBox_fixup(QDoubleSpinBoxH handle, PWideString str);
C_EXPORT void QDoubleSpinBox_setValue(QDoubleSpinBoxH handle, double val);

#endif
