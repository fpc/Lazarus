//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QVALIDATOR_C_H
#define QVALIDATOR_C_H

#include <QtGui>
#include "pascalbind.h"

C_EXPORT void QValidator_setLocale(QValidatorH handle, const QLocaleH locale);
C_EXPORT void QValidator_locale(QValidatorH handle, QLocaleH retval);
C_EXPORT QValidator::State QValidator_validate(QValidatorH handle, PWideString AnonParam1, int* AnonParam2);
C_EXPORT void QValidator_fixup(QValidatorH handle, PWideString AnonParam1);
C_EXPORT QIntValidatorH QIntValidator_Create(QObjectH parent);
C_EXPORT void QIntValidator_Destroy(QIntValidatorH handle);
C_EXPORT QIntValidatorH QIntValidator_Create2(int bottom, int top, QObjectH parent);
C_EXPORT QValidator::State QIntValidator_validate(QIntValidatorH handle, PWideString AnonParam1, int* AnonParam2);
C_EXPORT void QIntValidator_fixup(QIntValidatorH handle, PWideString input);
C_EXPORT void QIntValidator_setBottom(QIntValidatorH handle, int AnonParam1);
C_EXPORT void QIntValidator_setTop(QIntValidatorH handle, int AnonParam1);
C_EXPORT void QIntValidator_setRange(QIntValidatorH handle, int bottom, int top);
C_EXPORT int QIntValidator_bottom(QIntValidatorH handle);
C_EXPORT int QIntValidator_top(QIntValidatorH handle);
C_EXPORT QDoubleValidatorH QDoubleValidator_Create(QObjectH parent);
C_EXPORT void QDoubleValidator_Destroy(QDoubleValidatorH handle);
C_EXPORT QDoubleValidatorH QDoubleValidator_Create2(double bottom, double top, int decimals, QObjectH parent);
C_EXPORT QValidator::State QDoubleValidator_validate(QDoubleValidatorH handle, PWideString AnonParam1, int* AnonParam2);
C_EXPORT void QDoubleValidator_setRange(QDoubleValidatorH handle, double bottom, double top, int decimals);
C_EXPORT void QDoubleValidator_setBottom(QDoubleValidatorH handle, double AnonParam1);
C_EXPORT void QDoubleValidator_setTop(QDoubleValidatorH handle, double AnonParam1);
C_EXPORT void QDoubleValidator_setDecimals(QDoubleValidatorH handle, int AnonParam1);
C_EXPORT void QDoubleValidator_setNotation(QDoubleValidatorH handle, QDoubleValidator::Notation AnonParam1);
C_EXPORT double QDoubleValidator_bottom(QDoubleValidatorH handle);
C_EXPORT double QDoubleValidator_top(QDoubleValidatorH handle);
C_EXPORT int QDoubleValidator_decimals(QDoubleValidatorH handle);
C_EXPORT QDoubleValidator::Notation QDoubleValidator_notation(QDoubleValidatorH handle);

C_EXPORT QRegularExpressionValidatorH QRegularExpressionValidator_Create(QObjectH parent);
C_EXPORT void QRegularExpressionValidator_Destroy(QRegularExpressionValidatorH handle);
C_EXPORT QRegularExpressionValidatorH QRegularExpressionValidator_Create2(const QRegularExpressionH re, QObjectH parent);
C_EXPORT QValidator::State QRegularExpressionValidator_validate(QRegularExpressionValidatorH handle, PWideString input, int* pos);
C_EXPORT void QRegularExpressionValidator_regularExpression(QRegularExpressionValidatorH handle, QRegularExpressionH retval);
C_EXPORT void QRegularExpressionValidator_setRegularExpression(QRegularExpressionValidatorH handle, const QRegularExpressionH re);

#endif
