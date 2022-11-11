//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QPRINTDIALOG_C_H
#define QPRINTDIALOG_C_H

#include <QtPrintSupport>
#include "pascalbind.h"

C_EXPORT QPrintDialogH QPrintDialog_Create(QPrinterH printer, QWidgetH parent);
C_EXPORT void QPrintDialog_Destroy(QPrintDialogH handle);
C_EXPORT QPrintDialogH QPrintDialog_Create2(QWidgetH parent);
C_EXPORT int QPrintDialog_exec(QPrintDialogH handle);
#if defined BINUX
C_EXPORT void QPrintDialog_accept(QPrintDialogH handle);
#endif
C_EXPORT void QPrintDialog_done(QPrintDialogH handle, int result);
C_EXPORT void QPrintDialog_setOption(QPrintDialogH handle, QAbstractPrintDialog::PrintDialogOption option, bool on);
C_EXPORT bool QPrintDialog_testOption(QPrintDialogH handle, QAbstractPrintDialog::PrintDialogOption option);
C_EXPORT void QPrintDialog_setOptions(QPrintDialogH handle, unsigned int options);
C_EXPORT unsigned int QPrintDialog_options(QPrintDialogH handle);
C_EXPORT void QPrintDialog_setVisible(QPrintDialogH handle, bool visible);
C_EXPORT void QPrintDialog_open(QPrintDialogH handle, QObjectH receiver, const char* member);

#endif
