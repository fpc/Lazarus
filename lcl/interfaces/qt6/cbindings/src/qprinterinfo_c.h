//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QPRINTERINFO_C_H
#define QPRINTERINFO_C_H

#include <QtPrintSupport>
#include "pascalbind.h"

C_EXPORT QPrinterInfoH QPrinterInfo_Create();
C_EXPORT void QPrinterInfo_Destroy(QPrinterInfoH handle);
C_EXPORT QPrinterInfoH QPrinterInfo_Create2(const QPrinterInfoH other);
C_EXPORT QPrinterInfoH QPrinterInfo_Create3(const QPrinterH printer);
C_EXPORT void QPrinterInfo_printerName(QPrinterInfoH handle, PWideString retval);
C_EXPORT void QPrinterInfo_description(QPrinterInfoH handle, PWideString retval);
C_EXPORT void QPrinterInfo_location(QPrinterInfoH handle, PWideString retval);
C_EXPORT void QPrinterInfo_makeAndModel(QPrinterInfoH handle, PWideString retval);
C_EXPORT bool QPrinterInfo_isNull(QPrinterInfoH handle);
C_EXPORT bool QPrinterInfo_isDefault(QPrinterInfoH handle);
C_EXPORT void QPrinterInfo_supportedPageSizes(QPrinterInfoH handle, PPtrIntArray retval);

C_EXPORT QPrinter::ColorMode QPrinterInfo_defaultColorMode(QPrinterInfoH handle);
C_EXPORT QPrinter::DuplexMode QPrinterInfo_defaultDuplexMode(QPrinterInfoH handle);
C_EXPORT void QPrinterInfo_defaultPageSize(QPrinterInfoH handle, QPageSizeH retval);
C_EXPORT bool QPrinterInfo_isRemote(QPrinterInfoH handle);
C_EXPORT void QPrinterInfo_maximumPhysicalPageSize(QPrinterInfoH handle, QPageSizeH retval);
C_EXPORT void QPrinterInfo_minimumPhysicalPageSize(QPrinterInfoH handle, QPageSizeH retval);
C_EXPORT QPrinter::PrinterState QPrinterInfo_state(QPrinterInfoH handle);
C_EXPORT void QPrinterInfo_supportedColorModes(QPrinterInfoH handle, PPtrIntArray retval);
C_EXPORT void QPrinterInfo_supportedDuplexModes(QPrinterInfoH handle, PPtrIntArray retval);
C_EXPORT void QPrinterInfo_supportedResolutions(QPrinterInfoH handle, PPtrIntArray retval);

C_EXPORT void QPrinterInfo_defaultPrinterName(PWideString retval);


C_EXPORT void QPrinterInfo_availablePrinters(PPtrIntArray retval);
C_EXPORT void QPrinterInfo_availablePrinterNames(QStringListH retval);
C_EXPORT void QPrinterInfo_defaultPrinter(QPrinterInfoH retval);
C_EXPORT void QPrinterInfo_printerInfo(QPrinterInfoH retval, PWideString printerName);

#endif
