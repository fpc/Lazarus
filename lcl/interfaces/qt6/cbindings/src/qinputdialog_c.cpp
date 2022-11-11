//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qinputdialog_c.h"

QInputDialogH QInputDialog_Create(QWidgetH parent, unsigned int flags)
{
	return (QInputDialogH) new QInputDialog((QWidget*)parent, (Qt::WindowFlags)flags);
}

void QInputDialog_Destroy(QInputDialogH handle)
{
	delete (QInputDialog *)handle;
}

void QInputDialog_setInputMode(QInputDialogH handle, QInputDialog::InputMode mode)
{
	((QInputDialog *)handle)->setInputMode(mode);
}

QInputDialog::InputMode QInputDialog_inputMode(QInputDialogH handle)
{
	return (QInputDialog::InputMode) ((QInputDialog *)handle)->inputMode();
}

void QInputDialog_setLabelText(QInputDialogH handle, PWideString text)
{
	QString t_text;
	copyPWideStringToQString(text, t_text);
	((QInputDialog *)handle)->setLabelText(t_text);
}

void QInputDialog_labelText(QInputDialogH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QInputDialog *)handle)->labelText();
	copyQStringToPWideString(t_retval, retval);
}

void QInputDialog_setOption(QInputDialogH handle, QInputDialog::InputDialogOption option, bool on)
{
	((QInputDialog *)handle)->setOption(option, on);
}

bool QInputDialog_testOption(QInputDialogH handle, QInputDialog::InputDialogOption option)
{
	return (bool) ((QInputDialog *)handle)->testOption(option);
}

void QInputDialog_setOptions(QInputDialogH handle, unsigned int options)
{
	((QInputDialog *)handle)->setOptions((QInputDialog::InputDialogOptions)options);
}

unsigned int QInputDialog_options(QInputDialogH handle)
{
	return (unsigned int) ((QInputDialog *)handle)->options();
}

void QInputDialog_setTextValue(QInputDialogH handle, PWideString text)
{
	QString t_text;
	copyPWideStringToQString(text, t_text);
	((QInputDialog *)handle)->setTextValue(t_text);
}

void QInputDialog_textValue(QInputDialogH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QInputDialog *)handle)->textValue();
	copyQStringToPWideString(t_retval, retval);
}

void QInputDialog_setTextEchoMode(QInputDialogH handle, QLineEdit::EchoMode mode)
{
	((QInputDialog *)handle)->setTextEchoMode(mode);
}

QLineEdit::EchoMode QInputDialog_textEchoMode(QInputDialogH handle)
{
	return (QLineEdit::EchoMode) ((QInputDialog *)handle)->textEchoMode();
}

void QInputDialog_setComboBoxEditable(QInputDialogH handle, bool editable)
{
	((QInputDialog *)handle)->setComboBoxEditable(editable);
}

bool QInputDialog_isComboBoxEditable(QInputDialogH handle)
{
	return (bool) ((QInputDialog *)handle)->isComboBoxEditable();
}

void QInputDialog_setComboBoxItems(QInputDialogH handle, const QStringListH items)
{
	((QInputDialog *)handle)->setComboBoxItems(*(const QStringList*)items);
}

void QInputDialog_comboBoxItems(QInputDialogH handle, QStringListH retval)
{
	*(QStringList *)retval = ((QInputDialog *)handle)->comboBoxItems();
}

void QInputDialog_setIntValue(QInputDialogH handle, int value)
{
	((QInputDialog *)handle)->setIntValue(value);
}

int QInputDialog_intValue(QInputDialogH handle)
{
	return (int) ((QInputDialog *)handle)->intValue();
}

void QInputDialog_setIntMinimum(QInputDialogH handle, int min)
{
	((QInputDialog *)handle)->setIntMinimum(min);
}

int QInputDialog_intMinimum(QInputDialogH handle)
{
	return (int) ((QInputDialog *)handle)->intMinimum();
}

void QInputDialog_setIntMaximum(QInputDialogH handle, int max)
{
	((QInputDialog *)handle)->setIntMaximum(max);
}

int QInputDialog_intMaximum(QInputDialogH handle)
{
	return (int) ((QInputDialog *)handle)->intMaximum();
}

void QInputDialog_setIntRange(QInputDialogH handle, int min, int max)
{
	((QInputDialog *)handle)->setIntRange(min, max);
}

void QInputDialog_setIntStep(QInputDialogH handle, int step)
{
	((QInputDialog *)handle)->setIntStep(step);
}

int QInputDialog_intStep(QInputDialogH handle)
{
	return (int) ((QInputDialog *)handle)->intStep();
}

void QInputDialog_setDoubleValue(QInputDialogH handle, double value)
{
	((QInputDialog *)handle)->setDoubleValue(value);
}

double QInputDialog_doubleValue(QInputDialogH handle)
{
	return (double) ((QInputDialog *)handle)->doubleValue();
}

void QInputDialog_setDoubleMinimum(QInputDialogH handle, double min)
{
	((QInputDialog *)handle)->setDoubleMinimum(min);
}

double QInputDialog_doubleMinimum(QInputDialogH handle)
{
	return (double) ((QInputDialog *)handle)->doubleMinimum();
}

void QInputDialog_setDoubleMaximum(QInputDialogH handle, double max)
{
	((QInputDialog *)handle)->setDoubleMaximum(max);
}

double QInputDialog_doubleMaximum(QInputDialogH handle)
{
	return (double) ((QInputDialog *)handle)->doubleMaximum();
}

void QInputDialog_setDoubleRange(QInputDialogH handle, double min, double max)
{
	((QInputDialog *)handle)->setDoubleRange(min, max);
}

void QInputDialog_setDoubleDecimals(QInputDialogH handle, int decimals)
{
	((QInputDialog *)handle)->setDoubleDecimals(decimals);
}

int QInputDialog_doubleDecimals(QInputDialogH handle)
{
	return (int) ((QInputDialog *)handle)->doubleDecimals();
}

void QInputDialog_setDoubleStep(QInputDialogH handle, double step)
{
	((QInputDialog *)handle)->setDoubleStep(step);
}

double QInputDialog_doubleStep(QInputDialogH handle)
{
	return (double) ((QInputDialog *)handle)->doubleStep();
}


void QInputDialog_setOkButtonText(QInputDialogH handle, PWideString text)
{
	QString t_text;
	copyPWideStringToQString(text, t_text);
	((QInputDialog *)handle)->setOkButtonText(t_text);
}

void QInputDialog_okButtonText(QInputDialogH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QInputDialog *)handle)->okButtonText();
	copyQStringToPWideString(t_retval, retval);
}

void QInputDialog_setCancelButtonText(QInputDialogH handle, PWideString text)
{
	QString t_text;
	copyPWideStringToQString(text, t_text);
	((QInputDialog *)handle)->setCancelButtonText(t_text);
}

void QInputDialog_cancelButtonText(QInputDialogH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QInputDialog *)handle)->cancelButtonText();
	copyQStringToPWideString(t_retval, retval);
}

void QInputDialog_open(QInputDialogH handle, QObjectH receiver, const char* member)
{
	((QInputDialog *)handle)->open((QObject*)receiver, member);
}

void QInputDialog_minimumSizeHint(QInputDialogH handle, PSize retval)
{
	*(QSize *)retval = ((QInputDialog *)handle)->minimumSizeHint();
}

void QInputDialog_sizeHint(QInputDialogH handle, PSize retval)
{
	*(QSize *)retval = ((QInputDialog *)handle)->sizeHint();
}

void QInputDialog_setVisible(QInputDialogH handle, bool visible)
{
	((QInputDialog *)handle)->setVisible(visible);
}

void QInputDialog_getText(PWideString retval, QWidgetH parent, PWideString title, PWideString label, QLineEdit::EchoMode echo, PWideString text, bool* ok, unsigned int flags, unsigned int inputMethodHints)
{
	QString t_retval;
	QString t_title;
	QString t_label;
	QString t_text;
	copyPWideStringToQString(title, t_title);
	copyPWideStringToQString(label, t_label);
	copyPWideStringToQString(text, t_text);
	t_retval = QInputDialog::getText((QWidget*)parent, t_title, t_label, echo, t_text, ok, (Qt::WindowFlags)flags, (Qt::InputMethodHints)inputMethodHints);
	copyQStringToPWideString(t_retval, retval);
}

void QInputDialog_getItem(PWideString retval, QWidgetH parent, PWideString title, PWideString label, const QStringListH items, int current, bool editable, bool* ok, unsigned int flags, unsigned int inputMethodHints)
{
	QString t_retval;
	QString t_title;
	QString t_label;
	copyPWideStringToQString(title, t_title);
	copyPWideStringToQString(label, t_label);
	t_retval = QInputDialog::getItem((QWidget*)parent, t_title, t_label, *(const QStringList*)items, current, editable, ok, (Qt::WindowFlags)flags, (Qt::InputMethodHints)inputMethodHints);
	copyQStringToPWideString(t_retval, retval);
}

int QInputDialog_getInt(QWidgetH parent, PWideString title, PWideString label, int value, int minValue, int maxValue, int step, bool* ok, unsigned int flags)
{
	QString t_title;
	QString t_label;
	copyPWideStringToQString(title, t_title);
	copyPWideStringToQString(label, t_label);
	return (int) QInputDialog::getInt((QWidget*)parent, t_title, t_label, value, minValue, maxValue, step, ok, (Qt::WindowFlags)flags);
}

double QInputDialog_getDouble(QWidgetH parent, PWideString title, PWideString label, double value, double minValue, double maxValue, int decimals, bool* ok, unsigned int flags)
{
	QString t_title;
	QString t_label;
	copyPWideStringToQString(title, t_title);
	copyPWideStringToQString(label, t_label);
	return (double) QInputDialog::getDouble((QWidget*)parent, t_title, t_label, value, minValue, maxValue, decimals, ok, (Qt::WindowFlags)flags);
}

void QInputDialog_done(QInputDialogH handle, int result)
{
	((QInputDialog *)handle)->done(result);
}

