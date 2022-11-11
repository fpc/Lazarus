//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QABSTRACTPRINTDIALOG_C_H
#define QABSTRACTPRINTDIALOG_C_H

#include <QtPrintSupport>
#include "pascalbind.h"

C_EXPORT int QAbstractPrintDialog_exec(QAbstractPrintDialogH handle);
C_EXPORT void QAbstractPrintDialog_setPrintRange(QAbstractPrintDialogH handle, QAbstractPrintDialog::PrintRange range);
C_EXPORT QAbstractPrintDialog::PrintRange QAbstractPrintDialog_printRange(QAbstractPrintDialogH handle);
C_EXPORT void QAbstractPrintDialog_setMinMax(QAbstractPrintDialogH handle, int min, int max);
C_EXPORT int QAbstractPrintDialog_minPage(QAbstractPrintDialogH handle);
C_EXPORT int QAbstractPrintDialog_maxPage(QAbstractPrintDialogH handle);
C_EXPORT void QAbstractPrintDialog_setFromTo(QAbstractPrintDialogH handle, int fromPage, int toPage);
C_EXPORT int QAbstractPrintDialog_fromPage(QAbstractPrintDialogH handle);
C_EXPORT int QAbstractPrintDialog_toPage(QAbstractPrintDialogH handle);
C_EXPORT QPrinterH QAbstractPrintDialog_printer(QAbstractPrintDialogH handle);

#endif
