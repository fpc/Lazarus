//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QPAGESETUPDIALOG_C_H
#define QPAGESETUPDIALOG_C_H

#include <QtPrintSupport>
#include "pascalbind.h"

C_EXPORT QPageSetupDialogH QPageSetupDialog_Create(QPrinterH printer, QWidgetH parent);
C_EXPORT void QPageSetupDialog_Destroy(QPageSetupDialogH handle);
C_EXPORT QPageSetupDialogH QPageSetupDialog_Create2(QWidgetH parent);
C_EXPORT int QPageSetupDialog_exec(QPageSetupDialogH handle);
C_EXPORT void QPageSetupDialog_open(QPageSetupDialogH handle, QObjectH receiver, const char* member);
C_EXPORT void QPageSetupDialog_done(QPageSetupDialogH handle, int result);
C_EXPORT QPrinterH QPageSetupDialog_printer(QPageSetupDialogH handle);
#if defined MSWINDOWS || DARWIN
C_EXPORT void QPageSetupDialog_setVisible(QPageSetupDialogH handle, bool visible);
#endif

#endif
