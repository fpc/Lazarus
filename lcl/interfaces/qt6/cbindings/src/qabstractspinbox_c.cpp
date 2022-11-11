//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qabstractspinbox_c.h"

QAbstractSpinBoxH QAbstractSpinBox_Create(QWidgetH parent)
{
	return (QAbstractSpinBoxH) new QAbstractSpinBox((QWidget*)parent);
}

void QAbstractSpinBox_Destroy(QAbstractSpinBoxH handle)
{
	delete (QAbstractSpinBox *)handle;
}

QAbstractSpinBox::ButtonSymbols QAbstractSpinBox_buttonSymbols(QAbstractSpinBoxH handle)
{
	return (QAbstractSpinBox::ButtonSymbols) ((QAbstractSpinBox *)handle)->buttonSymbols();
}

void QAbstractSpinBox_setButtonSymbols(QAbstractSpinBoxH handle, QAbstractSpinBox::ButtonSymbols bs)
{
	((QAbstractSpinBox *)handle)->setButtonSymbols(bs);
}

void QAbstractSpinBox_setCorrectionMode(QAbstractSpinBoxH handle, QAbstractSpinBox::CorrectionMode cm)
{
	((QAbstractSpinBox *)handle)->setCorrectionMode(cm);
}

QAbstractSpinBox::CorrectionMode QAbstractSpinBox_correctionMode(QAbstractSpinBoxH handle)
{
	return (QAbstractSpinBox::CorrectionMode) ((QAbstractSpinBox *)handle)->correctionMode();
}

bool QAbstractSpinBox_hasAcceptableInput(QAbstractSpinBoxH handle)
{
	return (bool) ((QAbstractSpinBox *)handle)->hasAcceptableInput();
}

void QAbstractSpinBox_text(QAbstractSpinBoxH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QAbstractSpinBox *)handle)->text();
	copyQStringToPWideString(t_retval, retval);
}

void QAbstractSpinBox_specialValueText(QAbstractSpinBoxH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QAbstractSpinBox *)handle)->specialValueText();
	copyQStringToPWideString(t_retval, retval);
}

void QAbstractSpinBox_setSpecialValueText(QAbstractSpinBoxH handle, PWideString txt)
{
	QString t_txt;
	copyPWideStringToQString(txt, t_txt);
	((QAbstractSpinBox *)handle)->setSpecialValueText(t_txt);
}

bool QAbstractSpinBox_wrapping(QAbstractSpinBoxH handle)
{
	return (bool) ((QAbstractSpinBox *)handle)->wrapping();
}

void QAbstractSpinBox_setWrapping(QAbstractSpinBoxH handle, bool w)
{
	((QAbstractSpinBox *)handle)->setWrapping(w);
}

void QAbstractSpinBox_setReadOnly(QAbstractSpinBoxH handle, bool r)
{
	((QAbstractSpinBox *)handle)->setReadOnly(r);
}

bool QAbstractSpinBox_isReadOnly(QAbstractSpinBoxH handle)
{
	return (bool) ((QAbstractSpinBox *)handle)->isReadOnly();
}

void QAbstractSpinBox_setKeyboardTracking(QAbstractSpinBoxH handle, bool kt)
{
	((QAbstractSpinBox *)handle)->setKeyboardTracking(kt);
}

bool QAbstractSpinBox_keyboardTracking(QAbstractSpinBoxH handle)
{
	return (bool) ((QAbstractSpinBox *)handle)->keyboardTracking();
}

void QAbstractSpinBox_setAlignment(QAbstractSpinBoxH handle, unsigned int flag)
{
	((QAbstractSpinBox *)handle)->setAlignment((Qt::Alignment)flag);
}

unsigned int QAbstractSpinBox_alignment(QAbstractSpinBoxH handle)
{
	return (unsigned int) ((QAbstractSpinBox *)handle)->alignment();
}

void QAbstractSpinBox_setFrame(QAbstractSpinBoxH handle, bool AnonParam1)
{
	((QAbstractSpinBox *)handle)->setFrame(AnonParam1);
}

bool QAbstractSpinBox_hasFrame(QAbstractSpinBoxH handle)
{
	return (bool) ((QAbstractSpinBox *)handle)->hasFrame();
}

void QAbstractSpinBox_setAccelerated(QAbstractSpinBoxH handle, bool on)
{
	((QAbstractSpinBox *)handle)->setAccelerated(on);
}

bool QAbstractSpinBox_isAccelerated(QAbstractSpinBoxH handle)
{
	return (bool) ((QAbstractSpinBox *)handle)->isAccelerated();
}

void QAbstractSpinBox_sizeHint(QAbstractSpinBoxH handle, PSize retval)
{
	*(QSize *)retval = ((QAbstractSpinBox *)handle)->sizeHint();
}

void QAbstractSpinBox_minimumSizeHint(QAbstractSpinBoxH handle, PSize retval)
{
	*(QSize *)retval = ((QAbstractSpinBox *)handle)->minimumSizeHint();
}

void QAbstractSpinBox_interpretText(QAbstractSpinBoxH handle)
{
	((QAbstractSpinBox *)handle)->interpretText();
}

bool QAbstractSpinBox_event(QAbstractSpinBoxH handle, QEventH event)
{
	return (bool) ((QAbstractSpinBox *)handle)->event((QEvent*)event);
}

void QAbstractSpinBox_inputMethodQuery(QAbstractSpinBoxH handle, QVariantH retval, Qt::InputMethodQuery AnonParam1)
{
	*(QVariant *)retval = ((QAbstractSpinBox *)handle)->inputMethodQuery(AnonParam1);
}

QValidator::State QAbstractSpinBox_validate(QAbstractSpinBoxH handle, PWideString input, int* pos)
{
	QString t_input;
	copyPWideStringToQString(input, t_input);
	QValidator::State t_retval;
	t_retval = (QValidator::State) ((QAbstractSpinBox *)handle)->validate(t_input, *(int*)pos);
	copyQStringToPWideString(t_input, input);
	return t_retval;
}

void QAbstractSpinBox_fixup(QAbstractSpinBoxH handle, PWideString input)
{
	QString t_input;
	copyPWideStringToQString(input, t_input);
	((QAbstractSpinBox *)handle)->fixup(t_input);
	copyQStringToPWideString(t_input, input);
}

void QAbstractSpinBox_stepBy(QAbstractSpinBoxH handle, int steps)
{
	((QAbstractSpinBox *)handle)->stepBy(steps);
}

void QAbstractSpinBox_stepUp(QAbstractSpinBoxH handle)
{
	((QAbstractSpinBox *)handle)->stepUp();
}

void QAbstractSpinBox_stepDown(QAbstractSpinBoxH handle)
{
	((QAbstractSpinBox *)handle)->stepDown();
}

void QAbstractSpinBox_selectAll(QAbstractSpinBoxH handle)
{
	((QAbstractSpinBox *)handle)->selectAll();
}

void QAbstractSpinBox_clear(QAbstractSpinBoxH handle)
{
	((QAbstractSpinBox *)handle)->clear();
}

