//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QABSTRACTSPINBOX_C_H
#define QABSTRACTSPINBOX_C_H

#include <QtWidgets>
#include "pascalbind.h"

C_EXPORT QAbstractSpinBoxH QAbstractSpinBox_Create(QWidgetH parent);
C_EXPORT void QAbstractSpinBox_Destroy(QAbstractSpinBoxH handle);
C_EXPORT QAbstractSpinBox::ButtonSymbols QAbstractSpinBox_buttonSymbols(QAbstractSpinBoxH handle);
C_EXPORT void QAbstractSpinBox_setButtonSymbols(QAbstractSpinBoxH handle, QAbstractSpinBox::ButtonSymbols bs);
C_EXPORT void QAbstractSpinBox_setCorrectionMode(QAbstractSpinBoxH handle, QAbstractSpinBox::CorrectionMode cm);
C_EXPORT QAbstractSpinBox::CorrectionMode QAbstractSpinBox_correctionMode(QAbstractSpinBoxH handle);
C_EXPORT bool QAbstractSpinBox_hasAcceptableInput(QAbstractSpinBoxH handle);
C_EXPORT void QAbstractSpinBox_text(QAbstractSpinBoxH handle, PWideString retval);
C_EXPORT void QAbstractSpinBox_specialValueText(QAbstractSpinBoxH handle, PWideString retval);
C_EXPORT void QAbstractSpinBox_setSpecialValueText(QAbstractSpinBoxH handle, PWideString txt);
C_EXPORT bool QAbstractSpinBox_wrapping(QAbstractSpinBoxH handle);
C_EXPORT void QAbstractSpinBox_setWrapping(QAbstractSpinBoxH handle, bool w);
C_EXPORT void QAbstractSpinBox_setReadOnly(QAbstractSpinBoxH handle, bool r);
C_EXPORT bool QAbstractSpinBox_isReadOnly(QAbstractSpinBoxH handle);
C_EXPORT void QAbstractSpinBox_setKeyboardTracking(QAbstractSpinBoxH handle, bool kt);
C_EXPORT bool QAbstractSpinBox_keyboardTracking(QAbstractSpinBoxH handle);
C_EXPORT void QAbstractSpinBox_setAlignment(QAbstractSpinBoxH handle, unsigned int flag);
C_EXPORT unsigned int QAbstractSpinBox_alignment(QAbstractSpinBoxH handle);
C_EXPORT void QAbstractSpinBox_setFrame(QAbstractSpinBoxH handle, bool AnonParam1);
C_EXPORT bool QAbstractSpinBox_hasFrame(QAbstractSpinBoxH handle);
C_EXPORT void QAbstractSpinBox_setAccelerated(QAbstractSpinBoxH handle, bool on);
C_EXPORT bool QAbstractSpinBox_isAccelerated(QAbstractSpinBoxH handle);
C_EXPORT void QAbstractSpinBox_sizeHint(QAbstractSpinBoxH handle, PSize retval);
C_EXPORT void QAbstractSpinBox_minimumSizeHint(QAbstractSpinBoxH handle, PSize retval);
C_EXPORT void QAbstractSpinBox_interpretText(QAbstractSpinBoxH handle);
C_EXPORT bool QAbstractSpinBox_event(QAbstractSpinBoxH handle, QEventH event);
C_EXPORT void QAbstractSpinBox_inputMethodQuery(QAbstractSpinBoxH handle, QVariantH retval, Qt::InputMethodQuery AnonParam1);
C_EXPORT QValidator::State QAbstractSpinBox_validate(QAbstractSpinBoxH handle, PWideString input, int* pos);
C_EXPORT void QAbstractSpinBox_fixup(QAbstractSpinBoxH handle, PWideString input);
C_EXPORT void QAbstractSpinBox_stepBy(QAbstractSpinBoxH handle, int steps);
C_EXPORT void QAbstractSpinBox_stepUp(QAbstractSpinBoxH handle);
C_EXPORT void QAbstractSpinBox_stepDown(QAbstractSpinBoxH handle);
C_EXPORT void QAbstractSpinBox_selectAll(QAbstractSpinBoxH handle);
C_EXPORT void QAbstractSpinBox_clear(QAbstractSpinBoxH handle);

#endif
