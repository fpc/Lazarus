//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qvalidator_c.h"

void QValidator_setLocale(QValidatorH handle, const QLocaleH locale)
{
	((QValidator *)handle)->setLocale(*(const QLocale*)locale);
}

void QValidator_locale(QValidatorH handle, QLocaleH retval)
{
	*(QLocale *)retval = ((QValidator *)handle)->locale();
}

QValidator::State QValidator_validate(QValidatorH handle, PWideString AnonParam1, int* AnonParam2)
{
	QString t_AnonParam1;
	copyPWideStringToQString(AnonParam1, t_AnonParam1);
	QValidator::State t_retval;
	t_retval = (QValidator::State) ((QValidator *)handle)->validate(t_AnonParam1, *(int*)AnonParam2);
	copyQStringToPWideString(t_AnonParam1, AnonParam1);
	return t_retval;
}

void QValidator_fixup(QValidatorH handle, PWideString AnonParam1)
{
	QString t_AnonParam1;
	copyPWideStringToQString(AnonParam1, t_AnonParam1);
	((QValidator *)handle)->fixup(t_AnonParam1);
	copyQStringToPWideString(t_AnonParam1, AnonParam1);
}

QIntValidatorH QIntValidator_Create(QObjectH parent)
{
	return (QIntValidatorH) new QIntValidator((QObject*)parent);
}

void QIntValidator_Destroy(QIntValidatorH handle)
{
	delete (QIntValidator *)handle;
}

QIntValidatorH QIntValidator_Create2(int bottom, int top, QObjectH parent)
{
	return (QIntValidatorH) new QIntValidator(bottom, top, (QObject*)parent);
}

QValidator::State QIntValidator_validate(QIntValidatorH handle, PWideString AnonParam1, int* AnonParam2)
{
	QString t_AnonParam1;
	copyPWideStringToQString(AnonParam1, t_AnonParam1);
	QValidator::State t_retval;
	t_retval = (QValidator::State) ((QIntValidator *)handle)->validate(t_AnonParam1, *(int*)AnonParam2);
	copyQStringToPWideString(t_AnonParam1, AnonParam1);
	return t_retval;
}

void QIntValidator_fixup(QIntValidatorH handle, PWideString input)
{
	QString t_input;
	copyPWideStringToQString(input, t_input);
	((QIntValidator *)handle)->fixup(t_input);
	copyQStringToPWideString(t_input, input);
}

void QIntValidator_setBottom(QIntValidatorH handle, int AnonParam1)
{
	((QIntValidator *)handle)->setBottom(AnonParam1);
}

void QIntValidator_setTop(QIntValidatorH handle, int AnonParam1)
{
	((QIntValidator *)handle)->setTop(AnonParam1);
}

void QIntValidator_setRange(QIntValidatorH handle, int bottom, int top)
{
	((QIntValidator *)handle)->setRange(bottom, top);
}

int QIntValidator_bottom(QIntValidatorH handle)
{
	return (int) ((QIntValidator *)handle)->bottom();
}

int QIntValidator_top(QIntValidatorH handle)
{
	return (int) ((QIntValidator *)handle)->top();
}

QDoubleValidatorH QDoubleValidator_Create(QObjectH parent)
{
	return (QDoubleValidatorH) new QDoubleValidator((QObject*)parent);
}

void QDoubleValidator_Destroy(QDoubleValidatorH handle)
{
	delete (QDoubleValidator *)handle;
}

QDoubleValidatorH QDoubleValidator_Create2(double bottom, double top, int decimals, QObjectH parent)
{
	return (QDoubleValidatorH) new QDoubleValidator(bottom, top, decimals, (QObject*)parent);
}

QValidator::State QDoubleValidator_validate(QDoubleValidatorH handle, PWideString AnonParam1, int* AnonParam2)
{
	QString t_AnonParam1;
	copyPWideStringToQString(AnonParam1, t_AnonParam1);
	QValidator::State t_retval;
	t_retval = (QValidator::State) ((QDoubleValidator *)handle)->validate(t_AnonParam1, *(int*)AnonParam2);
	copyQStringToPWideString(t_AnonParam1, AnonParam1);
	return t_retval;
}

void QDoubleValidator_setRange(QDoubleValidatorH handle, double bottom, double top, int decimals)
{
	((QDoubleValidator *)handle)->setRange(bottom, top, decimals);
}

void QDoubleValidator_setBottom(QDoubleValidatorH handle, double AnonParam1)
{
	((QDoubleValidator *)handle)->setBottom(AnonParam1);
}

void QDoubleValidator_setTop(QDoubleValidatorH handle, double AnonParam1)
{
	((QDoubleValidator *)handle)->setTop(AnonParam1);
}

void QDoubleValidator_setDecimals(QDoubleValidatorH handle, int AnonParam1)
{
	((QDoubleValidator *)handle)->setDecimals(AnonParam1);
}

void QDoubleValidator_setNotation(QDoubleValidatorH handle, QDoubleValidator::Notation AnonParam1)
{
	((QDoubleValidator *)handle)->setNotation(AnonParam1);
}

double QDoubleValidator_bottom(QDoubleValidatorH handle)
{
	return (double) ((QDoubleValidator *)handle)->bottom();
}

double QDoubleValidator_top(QDoubleValidatorH handle)
{
	return (double) ((QDoubleValidator *)handle)->top();
}

int QDoubleValidator_decimals(QDoubleValidatorH handle)
{
	return (int) ((QDoubleValidator *)handle)->decimals();
}

QDoubleValidator::Notation QDoubleValidator_notation(QDoubleValidatorH handle)
{
	return (QDoubleValidator::Notation) ((QDoubleValidator *)handle)->notation();
}

QRegularExpressionValidatorH QRegularExpressionValidator_Create(QObjectH parent)
{
	return (QRegularExpressionValidatorH) new QRegularExpressionValidator((QObject*)parent);
}

void QRegularExpressionValidator_Destroy(QRegularExpressionValidatorH handle)
{
	delete (QRegularExpressionValidator *)handle;
}

QRegularExpressionValidatorH QRegularExpressionValidator_Create2(const QRegularExpressionH re, QObjectH parent)
{
	return (QRegularExpressionValidatorH) new QRegularExpressionValidator(*(const QRegularExpression*)re, (QObject*)parent);
}

QValidator::State QRegularExpressionValidator_validate(QRegularExpressionValidatorH handle, PWideString input, int* pos)
{
	QString t_input;
	copyPWideStringToQString(input, t_input);
	QValidator::State t_retval;
	t_retval = (QValidator::State) ((QRegularExpressionValidator *)handle)->validate(t_input, *(int*)pos);
	copyQStringToPWideString(t_input, input);
	return t_retval;
}

void QRegularExpressionValidator_regularExpression(QRegularExpressionValidatorH handle, QRegularExpressionH retval)
{
	*(QRegularExpression *)retval = ((QRegularExpressionValidator *)handle)->regularExpression();
}

void QRegularExpressionValidator_setRegularExpression(QRegularExpressionValidatorH handle, const QRegularExpressionH re)
{
	((QRegularExpressionValidator *)handle)->setRegularExpression(*(const QRegularExpression*)re);
}

