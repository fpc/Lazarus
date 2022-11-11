//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QPRINTPREVIEWDIALOG_C_H
#define QPRINTPREVIEWDIALOG_C_H

#include <QtPrintSupport>
#include "pascalbind.h"

C_EXPORT QPrintPreviewDialogH QPrintPreviewDialog_Create(QWidgetH parent, unsigned int flags);
C_EXPORT void QPrintPreviewDialog_Destroy(QPrintPreviewDialogH handle);
C_EXPORT QPrintPreviewDialogH QPrintPreviewDialog_Create2(QPrinterH printer, QWidgetH parent, unsigned int flags);
C_EXPORT void QPrintPreviewDialog_open(QPrintPreviewDialogH handle, QObjectH receiver, const char* member);
C_EXPORT QPrinterH QPrintPreviewDialog_printer(QPrintPreviewDialogH handle);
C_EXPORT void QPrintPreviewDialog_setVisible(QPrintPreviewDialogH handle, bool visible);
C_EXPORT void QPrintPreviewDialog_done(QPrintPreviewDialogH handle, int result);

#endif
