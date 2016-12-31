//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QPRINTER_C_H
#define QPRINTER_C_H

#include <QtPrintSupport>
#include "pascalbind.h"

C_EXPORT QPrinterH QPrinter_Create(QPrinter::PrinterMode mode);
C_EXPORT void QPrinter_Destroy(QPrinterH handle);
C_EXPORT QPrinterH QPrinter_Create2(const QPrinterInfoH printer, QPrinter::PrinterMode mode);
C_EXPORT int QPrinter_devType(QPrinterH handle);
C_EXPORT void QPrinter_setOutputFormat(QPrinterH handle, QPrinter::OutputFormat format);
C_EXPORT QPrinter::OutputFormat QPrinter_outputFormat(QPrinterH handle);
C_EXPORT void QPrinter_setPrinterName(QPrinterH handle, PWideString AnonParam1);
C_EXPORT void QPrinter_printerName(QPrinterH handle, PWideString retval);
C_EXPORT bool QPrinter_isValid(QPrinterH handle);
C_EXPORT void QPrinter_setOutputFileName(QPrinterH handle, PWideString AnonParam1);
C_EXPORT void QPrinter_outputFileName(QPrinterH handle, PWideString retval);
C_EXPORT void QPrinter_setPrintProgram(QPrinterH handle, PWideString AnonParam1);
C_EXPORT void QPrinter_printProgram(QPrinterH handle, PWideString retval);
C_EXPORT void QPrinter_setDocName(QPrinterH handle, PWideString AnonParam1);
C_EXPORT void QPrinter_docName(QPrinterH handle, PWideString retval);
C_EXPORT void QPrinter_setCreator(QPrinterH handle, PWideString AnonParam1);
C_EXPORT void QPrinter_creator(QPrinterH handle, PWideString retval);
C_EXPORT void QPrinter_setOrientation(QPrinterH handle, QPrinter::Orientation AnonParam1);
C_EXPORT QPrinter::Orientation QPrinter_orientation(QPrinterH handle);
C_EXPORT void QPrinter_setPageSizeMM(QPrinterH handle, const QSizeFH size);
C_EXPORT void QPrinter_setPaperSize(QPrinterH handle, QPagedPaintDevice::PageSize AnonParam1);
C_EXPORT QPagedPaintDevice::PageSize QPrinter_paperSize(QPrinterH handle);
C_EXPORT void QPrinter_setPaperSize2(QPrinterH handle, const QSizeFH paperSize, QPrinter::Unit unit);
C_EXPORT void QPrinter_paperSize2(QPrinterH handle, QSizeFH retval, QPrinter::Unit unit);
C_EXPORT void QPrinter_setPaperName(QPrinterH handle, PWideString paperName);
C_EXPORT void QPrinter_paperName(QPrinterH handle, PWideString retval);
C_EXPORT void QPrinter_setPageOrder(QPrinterH handle, QPrinter::PageOrder AnonParam1);
C_EXPORT QPrinter::PageOrder QPrinter_pageOrder(QPrinterH handle);
C_EXPORT void QPrinter_setResolution(QPrinterH handle, int AnonParam1);
C_EXPORT int QPrinter_resolution(QPrinterH handle);
C_EXPORT void QPrinter_setColorMode(QPrinterH handle, QPrinter::ColorMode AnonParam1);
C_EXPORT QPrinter::ColorMode QPrinter_colorMode(QPrinterH handle);
C_EXPORT void QPrinter_setCollateCopies(QPrinterH handle, bool collate);
C_EXPORT bool QPrinter_collateCopies(QPrinterH handle);
C_EXPORT void QPrinter_setFullPage(QPrinterH handle, bool AnonParam1);
C_EXPORT bool QPrinter_fullPage(QPrinterH handle);
C_EXPORT void QPrinter_setNumCopies(QPrinterH handle, int AnonParam1);
C_EXPORT int QPrinter_numCopies(QPrinterH handle);
C_EXPORT int QPrinter_actualNumCopies(QPrinterH handle);
C_EXPORT void QPrinter_setCopyCount(QPrinterH handle, int AnonParam1);
C_EXPORT int QPrinter_copyCount(QPrinterH handle);
C_EXPORT bool QPrinter_supportsMultipleCopies(QPrinterH handle);
C_EXPORT void QPrinter_setPaperSource(QPrinterH handle, QPrinter::PaperSource AnonParam1);
C_EXPORT QPrinter::PaperSource QPrinter_paperSource(QPrinterH handle);
C_EXPORT void QPrinter_setDuplex(QPrinterH handle, QPrinter::DuplexMode duplex);
C_EXPORT QPrinter::DuplexMode QPrinter_duplex(QPrinterH handle);
C_EXPORT void QPrinter_supportedResolutions(QPrinterH handle, PPtrIntArray retval);
C_EXPORT void QPrinter_setFontEmbeddingEnabled(QPrinterH handle, bool enable);
C_EXPORT bool QPrinter_fontEmbeddingEnabled(QPrinterH handle);
C_EXPORT void QPrinter_setDoubleSidedPrinting(QPrinterH handle, bool enable);
C_EXPORT bool QPrinter_doubleSidedPrinting(QPrinterH handle);
C_EXPORT void QPrinter_paperRect(QPrinterH handle, PRect retval);
C_EXPORT void QPrinter_pageRect(QPrinterH handle, PRect retval);
C_EXPORT void QPrinter_paperRect2(QPrinterH handle, QRectFH retval, QPrinter::Unit AnonParam1);
C_EXPORT void QPrinter_pageRect2(QPrinterH handle, QRectFH retval, QPrinter::Unit AnonParam1);
#if defined BINUX || DARWIN
C_EXPORT void QPrinter_printerSelectionOption(QPrinterH handle, PWideString retval);
C_EXPORT void QPrinter_setPrinterSelectionOption(QPrinterH handle, PWideString AnonParam1);
#endif
C_EXPORT bool QPrinter_newPage(QPrinterH handle);
C_EXPORT bool QPrinter_abort(QPrinterH handle);
C_EXPORT QPrinter::PrinterState QPrinter_printerState(QPrinterH handle);
C_EXPORT QPaintEngineH QPrinter_paintEngine(QPrinterH handle);
C_EXPORT QPrintEngineH QPrinter_printEngine(QPrinterH handle);
C_EXPORT void QPrinter_setFromTo(QPrinterH handle, int fromPage, int toPage);
C_EXPORT int QPrinter_fromPage(QPrinterH handle);
C_EXPORT int QPrinter_toPage(QPrinterH handle);
C_EXPORT void QPrinter_setPrintRange(QPrinterH handle, QPrinter::PrintRange range);
C_EXPORT QPrinter::PrintRange QPrinter_printRange(QPrinterH handle);
C_EXPORT void QPrinter_setPageMargins(QPrinterH handle, qreal left, qreal top, qreal right, qreal bottom, QPrinter::Unit unit);
C_EXPORT void QPrinter_getPageMargins(QPrinterH handle, qreal* left, qreal* top, qreal* right, qreal* bottom, QPrinter::Unit unit);
#if defined MSWINDOWS
C_EXPORT void QPrinter_setWinPageSize(QPrinterH handle, int winPageSize);
C_EXPORT int QPrinter_winPageSize(QPrinterH handle);
#endif

#endif
