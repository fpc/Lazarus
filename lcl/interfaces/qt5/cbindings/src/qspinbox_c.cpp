//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qspinbox_c.h"

QSpinBoxH QSpinBox_Create(QWidgetH parent)
{
	return (QSpinBoxH) new QSpinBox((QWidget*)parent);
}

void QSpinBox_Destroy(QSpinBoxH handle)
{
	delete (QSpinBox *)handle;
}

int QSpinBox_value(QSpinBoxH handle)
{
	return (int) ((QSpinBox *)handle)->value();
}

void QSpinBox_prefix(QSpinBoxH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QSpinBox *)handle)->prefix();
	copyQStringToPWideString(t_retval, retval);
}

void QSpinBox_setPrefix(QSpinBoxH handle, PWideString prefix)
{
	QString t_prefix;
	copyPWideStringToQString(prefix, t_prefix);
	((QSpinBox *)handle)->setPrefix(t_prefix);
}

void QSpinBox_suffix(QSpinBoxH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QSpinBox *)handle)->suffix();
	copyQStringToPWideString(t_retval, retval);
}

void QSpinBox_setSuffix(QSpinBoxH handle, PWideString suffix)
{
	QString t_suffix;
	copyPWideStringToQString(suffix, t_suffix);
	((QSpinBox *)handle)->setSuffix(t_suffix);
}

void QSpinBox_cleanText(QSpinBoxH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QSpinBox *)handle)->cleanText();
	copyQStringToPWideString(t_retval, retval);
}

int QSpinBox_singleStep(QSpinBoxH handle)
{
	return (int) ((QSpinBox *)handle)->singleStep();
}

void QSpinBox_setSingleStep(QSpinBoxH handle, int val)
{
	((QSpinBox *)handle)->setSingleStep(val);
}

int QSpinBox_minimum(QSpinBoxH handle)
{
	return (int) ((QSpinBox *)handle)->minimum();
}

void QSpinBox_setMinimum(QSpinBoxH handle, int min)
{
	((QSpinBox *)handle)->setMinimum(min);
}

int QSpinBox_maximum(QSpinBoxH handle)
{
	return (int) ((QSpinBox *)handle)->maximum();
}

void QSpinBox_setMaximum(QSpinBoxH handle, int max)
{
	((QSpinBox *)handle)->setMaximum(max);
}

void QSpinBox_setRange(QSpinBoxH handle, int min, int max)
{
	((QSpinBox *)handle)->setRange(min, max);
}

void QSpinBox_setValue(QSpinBoxH handle, int val)
{
	((QSpinBox *)handle)->setValue(val);
}

QDoubleSpinBoxH QDoubleSpinBox_Create(QWidgetH parent)
{
	return (QDoubleSpinBoxH) new QDoubleSpinBox((QWidget*)parent);
}

void QDoubleSpinBox_Destroy(QDoubleSpinBoxH handle)
{
	delete (QDoubleSpinBox *)handle;
}

double QDoubleSpinBox_value(QDoubleSpinBoxH handle)
{
	return (double) ((QDoubleSpinBox *)handle)->value();
}

void QDoubleSpinBox_prefix(QDoubleSpinBoxH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QDoubleSpinBox *)handle)->prefix();
	copyQStringToPWideString(t_retval, retval);
}

void QDoubleSpinBox_setPrefix(QDoubleSpinBoxH handle, PWideString prefix)
{
	QString t_prefix;
	copyPWideStringToQString(prefix, t_prefix);
	((QDoubleSpinBox *)handle)->setPrefix(t_prefix);
}

void QDoubleSpinBox_suffix(QDoubleSpinBoxH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QDoubleSpinBox *)handle)->suffix();
	copyQStringToPWideString(t_retval, retval);
}

void QDoubleSpinBox_setSuffix(QDoubleSpinBoxH handle, PWideString suffix)
{
	QString t_suffix;
	copyPWideStringToQString(suffix, t_suffix);
	((QDoubleSpinBox *)handle)->setSuffix(t_suffix);
}

void QDoubleSpinBox_cleanText(QDoubleSpinBoxH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QDoubleSpinBox *)handle)->cleanText();
	copyQStringToPWideString(t_retval, retval);
}

double QDoubleSpinBox_singleStep(QDoubleSpinBoxH handle)
{
	return (double) ((QDoubleSpinBox *)handle)->singleStep();
}

void QDoubleSpinBox_setSingleStep(QDoubleSpinBoxH handle, double val)
{
	((QDoubleSpinBox *)handle)->setSingleStep(val);
}

double QDoubleSpinBox_minimum(QDoubleSpinBoxH handle)
{
	return (double) ((QDoubleSpinBox *)handle)->minimum();
}

void QDoubleSpinBox_setMinimum(QDoubleSpinBoxH handle, double min)
{
	((QDoubleSpinBox *)handle)->setMinimum(min);
}

double QDoubleSpinBox_maximum(QDoubleSpinBoxH handle)
{
	return (double) ((QDoubleSpinBox *)handle)->maximum();
}

void QDoubleSpinBox_setMaximum(QDoubleSpinBoxH handle, double max)
{
	((QDoubleSpinBox *)handle)->setMaximum(max);
}

void QDoubleSpinBox_setRange(QDoubleSpinBoxH handle, double min, double max)
{
	((QDoubleSpinBox *)handle)->setRange(min, max);
}

int QDoubleSpinBox_decimals(QDoubleSpinBoxH handle)
{
	return (int) ((QDoubleSpinBox *)handle)->decimals();
}

void QDoubleSpinBox_setDecimals(QDoubleSpinBoxH handle, int prec)
{
	((QDoubleSpinBox *)handle)->setDecimals(prec);
}

QValidator::State QDoubleSpinBox_validate(QDoubleSpinBoxH handle, PWideString input, int* pos)
{
	QString t_input;
	copyPWideStringToQString(input, t_input);
	QValidator::State t_retval;
	t_retval = (QValidator::State) ((QDoubleSpinBox *)handle)->validate(t_input, *(int*)pos);
	copyQStringToPWideString(t_input, input);
	return t_retval;
}

double QDoubleSpinBox_valueFromText(QDoubleSpinBoxH handle, PWideString text)
{
	QString t_text;
	copyPWideStringToQString(text, t_text);
	return (double) ((QDoubleSpinBox *)handle)->valueFromText(t_text);
}

void QDoubleSpinBox_textFromValue(QDoubleSpinBoxH handle, PWideString retval, double val)
{
	QString t_retval;
	t_retval = ((QDoubleSpinBox *)handle)->textFromValue(val);
	copyQStringToPWideString(t_retval, retval);
}

void QDoubleSpinBox_fixup(QDoubleSpinBoxH handle, PWideString str)
{
	QString t_str;
	copyPWideStringToQString(str, t_str);
	((QDoubleSpinBox *)handle)->fixup(t_str);
	copyQStringToPWideString(t_str, str);
}

void QDoubleSpinBox_setValue(QDoubleSpinBoxH handle, double val)
{
	((QDoubleSpinBox *)handle)->setValue(val);
}

