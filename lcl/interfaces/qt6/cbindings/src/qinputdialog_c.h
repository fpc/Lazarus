//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QINPUTDIALOG_C_H
#define QINPUTDIALOG_C_H

#include <QtWidgets>
#include "pascalbind.h"

C_EXPORT QInputDialogH QInputDialog_Create(QWidgetH parent, unsigned int flags);
C_EXPORT void QInputDialog_Destroy(QInputDialogH handle);
C_EXPORT void QInputDialog_setInputMode(QInputDialogH handle, QInputDialog::InputMode mode);
C_EXPORT QInputDialog::InputMode QInputDialog_inputMode(QInputDialogH handle);
C_EXPORT void QInputDialog_setLabelText(QInputDialogH handle, PWideString text);
C_EXPORT void QInputDialog_labelText(QInputDialogH handle, PWideString retval);
C_EXPORT void QInputDialog_setOption(QInputDialogH handle, QInputDialog::InputDialogOption option, bool on);
C_EXPORT bool QInputDialog_testOption(QInputDialogH handle, QInputDialog::InputDialogOption option);
C_EXPORT void QInputDialog_setOptions(QInputDialogH handle, unsigned int options);
C_EXPORT unsigned int QInputDialog_options(QInputDialogH handle);
C_EXPORT void QInputDialog_setTextValue(QInputDialogH handle, PWideString text);
C_EXPORT void QInputDialog_textValue(QInputDialogH handle, PWideString retval);
C_EXPORT void QInputDialog_setTextEchoMode(QInputDialogH handle, QLineEdit::EchoMode mode);
C_EXPORT QLineEdit::EchoMode QInputDialog_textEchoMode(QInputDialogH handle);
C_EXPORT void QInputDialog_setComboBoxEditable(QInputDialogH handle, bool editable);
C_EXPORT bool QInputDialog_isComboBoxEditable(QInputDialogH handle);
C_EXPORT void QInputDialog_setComboBoxItems(QInputDialogH handle, const QStringListH items);
C_EXPORT void QInputDialog_comboBoxItems(QInputDialogH handle, QStringListH retval);
C_EXPORT void QInputDialog_setIntValue(QInputDialogH handle, int value);
C_EXPORT int QInputDialog_intValue(QInputDialogH handle);
C_EXPORT void QInputDialog_setIntMinimum(QInputDialogH handle, int min);
C_EXPORT int QInputDialog_intMinimum(QInputDialogH handle);
C_EXPORT void QInputDialog_setIntMaximum(QInputDialogH handle, int max);
C_EXPORT int QInputDialog_intMaximum(QInputDialogH handle);
C_EXPORT void QInputDialog_setIntRange(QInputDialogH handle, int min, int max);
C_EXPORT void QInputDialog_setIntStep(QInputDialogH handle, int step);
C_EXPORT int QInputDialog_intStep(QInputDialogH handle);
C_EXPORT void QInputDialog_setDoubleValue(QInputDialogH handle, double value);
C_EXPORT double QInputDialog_doubleValue(QInputDialogH handle);
C_EXPORT void QInputDialog_setDoubleMinimum(QInputDialogH handle, double min);
C_EXPORT double QInputDialog_doubleMinimum(QInputDialogH handle);
C_EXPORT void QInputDialog_setDoubleMaximum(QInputDialogH handle, double max);
C_EXPORT double QInputDialog_doubleMaximum(QInputDialogH handle);
C_EXPORT void QInputDialog_setDoubleRange(QInputDialogH handle, double min, double max);
C_EXPORT void QInputDialog_setDoubleDecimals(QInputDialogH handle, int decimals);
C_EXPORT int QInputDialog_doubleDecimals(QInputDialogH handle);
C_EXPORT void QInputDialog_setDoubleStep(QInputDialogH handle, double step);
C_EXPORT double QInputDialog_doubleStep(QInputDialogH handle);
C_EXPORT void QInputDialog_setOkButtonText(QInputDialogH handle, PWideString text);
C_EXPORT void QInputDialog_okButtonText(QInputDialogH handle, PWideString retval);
C_EXPORT void QInputDialog_setCancelButtonText(QInputDialogH handle, PWideString text);
C_EXPORT void QInputDialog_cancelButtonText(QInputDialogH handle, PWideString retval);
C_EXPORT void QInputDialog_open(QInputDialogH handle, QObjectH receiver, const char* member);
C_EXPORT void QInputDialog_minimumSizeHint(QInputDialogH handle, PSize retval);
C_EXPORT void QInputDialog_sizeHint(QInputDialogH handle, PSize retval);
C_EXPORT void QInputDialog_setVisible(QInputDialogH handle, bool visible);
C_EXPORT void QInputDialog_getText(PWideString retval, QWidgetH parent, PWideString title, PWideString label, QLineEdit::EchoMode echo, PWideString text, bool* ok, unsigned int flags, unsigned int inputMethodHints);
C_EXPORT void QInputDialog_getItem(PWideString retval, QWidgetH parent, PWideString title, PWideString label, const QStringListH items, int current, bool editable, bool* ok, unsigned int flags, unsigned int inputMethodHints);
C_EXPORT int QInputDialog_getInt(QWidgetH parent, PWideString title, PWideString label, int value, int minValue, int maxValue, int step, bool* ok, unsigned int flags);
C_EXPORT double QInputDialog_getDouble(QWidgetH parent, PWideString title, PWideString label, double value, double minValue, double maxValue, int decimals, bool* ok, unsigned int flags);
C_EXPORT void QInputDialog_done(QInputDialogH handle, int result);

#endif
