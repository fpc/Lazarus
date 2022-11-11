//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QPROGRESSDIALOG_C_H
#define QPROGRESSDIALOG_C_H

#include <QtWidgets>
#include "pascalbind.h"

C_EXPORT QProgressDialogH QProgressDialog_Create(QWidgetH parent, unsigned int flags);
C_EXPORT void QProgressDialog_Destroy(QProgressDialogH handle);
C_EXPORT QProgressDialogH QProgressDialog_Create2(PWideString labelText, PWideString cancelButtonText, int minimum, int maximum, QWidgetH parent, unsigned int flags);
C_EXPORT void QProgressDialog_setLabel(QProgressDialogH handle, QLabelH label);
C_EXPORT void QProgressDialog_setCancelButton(QProgressDialogH handle, QPushButtonH button);
C_EXPORT void QProgressDialog_setBar(QProgressDialogH handle, QProgressBarH bar);
C_EXPORT bool QProgressDialog_wasCanceled(QProgressDialogH handle);
C_EXPORT int QProgressDialog_minimum(QProgressDialogH handle);
C_EXPORT int QProgressDialog_maximum(QProgressDialogH handle);
C_EXPORT int QProgressDialog_value(QProgressDialogH handle);
C_EXPORT void QProgressDialog_sizeHint(QProgressDialogH handle, PSize retval);
C_EXPORT void QProgressDialog_labelText(QProgressDialogH handle, PWideString retval);
C_EXPORT int QProgressDialog_minimumDuration(QProgressDialogH handle);
C_EXPORT void QProgressDialog_setAutoReset(QProgressDialogH handle, bool reset);
C_EXPORT bool QProgressDialog_autoReset(QProgressDialogH handle);
C_EXPORT void QProgressDialog_setAutoClose(QProgressDialogH handle, bool close);
C_EXPORT bool QProgressDialog_autoClose(QProgressDialogH handle);
C_EXPORT void QProgressDialog_open(QProgressDialogH handle, QObjectH receiver, const char* member);
C_EXPORT void QProgressDialog_cancel(QProgressDialogH handle);
C_EXPORT void QProgressDialog_reset(QProgressDialogH handle);
C_EXPORT void QProgressDialog_setMaximum(QProgressDialogH handle, int maximum);
C_EXPORT void QProgressDialog_setMinimum(QProgressDialogH handle, int minimum);
C_EXPORT void QProgressDialog_setRange(QProgressDialogH handle, int minimum, int maximum);
C_EXPORT void QProgressDialog_setValue(QProgressDialogH handle, int progress);
C_EXPORT void QProgressDialog_setLabelText(QProgressDialogH handle, PWideString text);
C_EXPORT void QProgressDialog_setCancelButtonText(QProgressDialogH handle, PWideString text);
C_EXPORT void QProgressDialog_setMinimumDuration(QProgressDialogH handle, int ms);

#endif
