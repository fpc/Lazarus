//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qprinter_c.h"

QPrinterH QPrinter_Create(QPrinter::PrinterMode mode)
{
	return (QPrinterH) new QPrinter(mode);
}

void QPrinter_Destroy(QPrinterH handle)
{
	delete (QPrinter *)handle;
}

QPrinterH QPrinter_Create2(const QPrinterInfoH printer, QPrinter::PrinterMode mode)
{
	return (QPrinterH) new QPrinter(*(const QPrinterInfo*)printer, mode);
}

int QPrinter_devType(QPrinterH handle)
{
	return (int) ((QPrinter *)handle)->devType();
}

void QPrinter_setOutputFormat(QPrinterH handle, QPrinter::OutputFormat format)
{
	((QPrinter *)handle)->setOutputFormat(format);
}

QPrinter::OutputFormat QPrinter_outputFormat(QPrinterH handle)
{
	return (QPrinter::OutputFormat) ((QPrinter *)handle)->outputFormat();
}

void QPrinter_setPrinterName(QPrinterH handle, PWideString AnonParam1)
{
	QString t_AnonParam1;
	copyPWideStringToQString(AnonParam1, t_AnonParam1);
	((QPrinter *)handle)->setPrinterName(t_AnonParam1);
}

void QPrinter_printerName(QPrinterH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QPrinter *)handle)->printerName();
	copyQStringToPWideString(t_retval, retval);
}

bool QPrinter_isValid(QPrinterH handle)
{
	return (bool) ((QPrinter *)handle)->isValid();
}

void QPrinter_setOutputFileName(QPrinterH handle, PWideString AnonParam1)
{
	QString t_AnonParam1;
	copyPWideStringToQString(AnonParam1, t_AnonParam1);
	((QPrinter *)handle)->setOutputFileName(t_AnonParam1);
}

void QPrinter_outputFileName(QPrinterH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QPrinter *)handle)->outputFileName();
	copyQStringToPWideString(t_retval, retval);
}

void QPrinter_setPrintProgram(QPrinterH handle, PWideString AnonParam1)
{
	QString t_AnonParam1;
	copyPWideStringToQString(AnonParam1, t_AnonParam1);
	((QPrinter *)handle)->setPrintProgram(t_AnonParam1);
}

void QPrinter_printProgram(QPrinterH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QPrinter *)handle)->printProgram();
	copyQStringToPWideString(t_retval, retval);
}

void QPrinter_setDocName(QPrinterH handle, PWideString AnonParam1)
{
	QString t_AnonParam1;
	copyPWideStringToQString(AnonParam1, t_AnonParam1);
	((QPrinter *)handle)->setDocName(t_AnonParam1);
}

void QPrinter_docName(QPrinterH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QPrinter *)handle)->docName();
	copyQStringToPWideString(t_retval, retval);
}

void QPrinter_setCreator(QPrinterH handle, PWideString AnonParam1)
{
	QString t_AnonParam1;
	copyPWideStringToQString(AnonParam1, t_AnonParam1);
	((QPrinter *)handle)->setCreator(t_AnonParam1);
}

void QPrinter_creator(QPrinterH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QPrinter *)handle)->creator();
	copyQStringToPWideString(t_retval, retval);
}

void QPrinter_setOrientation(QPrinterH handle, QPrinter::Orientation AnonParam1)
{
	((QPrinter *)handle)->setOrientation(AnonParam1);
}

QPrinter::Orientation QPrinter_orientation(QPrinterH handle)
{
	return (QPrinter::Orientation) ((QPrinter *)handle)->orientation();
}

void QPrinter_setPageSizeMM(QPrinterH handle, const QSizeFH size)
{
	((QPrinter *)handle)->setPageSizeMM(*(const QSizeF*)size);
}

void QPrinter_setPaperSize(QPrinterH handle, QPagedPaintDevice::PageSize AnonParam1)
{
	((QPrinter *)handle)->setPaperSize(AnonParam1);
}

QPagedPaintDevice::PageSize QPrinter_paperSize(QPrinterH handle)
{
	return (QPagedPaintDevice::PageSize) ((QPrinter *)handle)->paperSize();
}

void QPrinter_setPaperSize2(QPrinterH handle, const QSizeFH paperSize, QPrinter::Unit unit)
{
	((QPrinter *)handle)->setPaperSize(*(const QSizeF*)paperSize, unit);
}

void QPrinter_paperSize2(QPrinterH handle, QSizeFH retval, QPrinter::Unit unit)
{
	*(QSizeF *)retval = ((QPrinter *)handle)->paperSize(unit);
}

void QPrinter_setPaperName(QPrinterH handle, PWideString paperName)
{
	QString t_paperName;
	copyPWideStringToQString(paperName, t_paperName);
	((QPrinter *)handle)->setPaperName(t_paperName);
}

void QPrinter_paperName(QPrinterH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QPrinter *)handle)->paperName();
	copyQStringToPWideString(t_retval, retval);
}

void QPrinter_setPageOrder(QPrinterH handle, QPrinter::PageOrder AnonParam1)
{
	((QPrinter *)handle)->setPageOrder(AnonParam1);
}

QPrinter::PageOrder QPrinter_pageOrder(QPrinterH handle)
{
	return (QPrinter::PageOrder) ((QPrinter *)handle)->pageOrder();
}

void QPrinter_setResolution(QPrinterH handle, int AnonParam1)
{
	((QPrinter *)handle)->setResolution(AnonParam1);
}

int QPrinter_resolution(QPrinterH handle)
{
	return (int) ((QPrinter *)handle)->resolution();
}

void QPrinter_setColorMode(QPrinterH handle, QPrinter::ColorMode AnonParam1)
{
	((QPrinter *)handle)->setColorMode(AnonParam1);
}

QPrinter::ColorMode QPrinter_colorMode(QPrinterH handle)
{
	return (QPrinter::ColorMode) ((QPrinter *)handle)->colorMode();
}

void QPrinter_setCollateCopies(QPrinterH handle, bool collate)
{
	((QPrinter *)handle)->setCollateCopies(collate);
}

bool QPrinter_collateCopies(QPrinterH handle)
{
	return (bool) ((QPrinter *)handle)->collateCopies();
}

void QPrinter_setFullPage(QPrinterH handle, bool AnonParam1)
{
	((QPrinter *)handle)->setFullPage(AnonParam1);
}

bool QPrinter_fullPage(QPrinterH handle)
{
	return (bool) ((QPrinter *)handle)->fullPage();
}

void QPrinter_setNumCopies(QPrinterH handle, int AnonParam1)
{
	((QPrinter *)handle)->setNumCopies(AnonParam1);
}

int QPrinter_numCopies(QPrinterH handle)
{
	return (int) ((QPrinter *)handle)->numCopies();
}

int QPrinter_actualNumCopies(QPrinterH handle)
{
	return (int) ((QPrinter *)handle)->actualNumCopies();
}

void QPrinter_setCopyCount(QPrinterH handle, int AnonParam1)
{
	((QPrinter *)handle)->setCopyCount(AnonParam1);
}

int QPrinter_copyCount(QPrinterH handle)
{
	return (int) ((QPrinter *)handle)->copyCount();
}

bool QPrinter_supportsMultipleCopies(QPrinterH handle)
{
	return (bool) ((QPrinter *)handle)->supportsMultipleCopies();
}

void QPrinter_setPaperSource(QPrinterH handle, QPrinter::PaperSource AnonParam1)
{
	((QPrinter *)handle)->setPaperSource(AnonParam1);
}

QPrinter::PaperSource QPrinter_paperSource(QPrinterH handle)
{
	return (QPrinter::PaperSource) ((QPrinter *)handle)->paperSource();
}

void QPrinter_setDuplex(QPrinterH handle, QPrinter::DuplexMode duplex)
{
	((QPrinter *)handle)->setDuplex(duplex);
}

QPrinter::DuplexMode QPrinter_duplex(QPrinterH handle)
{
	return (QPrinter::DuplexMode) ((QPrinter *)handle)->duplex();
}

void QPrinter_supportedResolutions(QPrinterH handle, PPtrIntArray retval)
{
	QList<int> t_retval;
	t_retval = ((QPrinter *)handle)->supportedResolutions();
	copyQListTemplateToPtrIntArray(t_retval, retval);
}

void QPrinter_setFontEmbeddingEnabled(QPrinterH handle, bool enable)
{
	((QPrinter *)handle)->setFontEmbeddingEnabled(enable);
}

bool QPrinter_fontEmbeddingEnabled(QPrinterH handle)
{
	return (bool) ((QPrinter *)handle)->fontEmbeddingEnabled();
}

void QPrinter_setDoubleSidedPrinting(QPrinterH handle, bool enable)
{
	((QPrinter *)handle)->setDoubleSidedPrinting(enable);
}

bool QPrinter_doubleSidedPrinting(QPrinterH handle)
{
	return (bool) ((QPrinter *)handle)->doubleSidedPrinting();
}

void QPrinter_paperRect(QPrinterH handle, PRect retval)
{
	QRect t_retval;
	t_retval = ((QPrinter *)handle)->paperRect();
	copyQRectToPRect(t_retval, retval);
}

void QPrinter_pageRect(QPrinterH handle, PRect retval)
{
	QRect t_retval;
	t_retval = ((QPrinter *)handle)->pageRect();
	copyQRectToPRect(t_retval, retval);
}

void QPrinter_paperRect2(QPrinterH handle, QRectFH retval, QPrinter::Unit AnonParam1)
{
	*(QRectF *)retval = ((QPrinter *)handle)->paperRect(AnonParam1);
}

void QPrinter_pageRect2(QPrinterH handle, QRectFH retval, QPrinter::Unit AnonParam1)
{
	*(QRectF *)retval = ((QPrinter *)handle)->pageRect(AnonParam1);
}

#if defined BINUX || DARWIN
void QPrinter_printerSelectionOption(QPrinterH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QPrinter *)handle)->printerSelectionOption();
	copyQStringToPWideString(t_retval, retval);
}

void QPrinter_setPrinterSelectionOption(QPrinterH handle, PWideString AnonParam1)
{
	QString t_AnonParam1;
	copyPWideStringToQString(AnonParam1, t_AnonParam1);
	((QPrinter *)handle)->setPrinterSelectionOption(t_AnonParam1);
}

#endif
bool QPrinter_newPage(QPrinterH handle)
{
	return (bool) ((QPrinter *)handle)->newPage();
}

bool QPrinter_abort(QPrinterH handle)
{
	return (bool) ((QPrinter *)handle)->abort();
}

QPrinter::PrinterState QPrinter_printerState(QPrinterH handle)
{
	return (QPrinter::PrinterState) ((QPrinter *)handle)->printerState();
}

QPaintEngineH QPrinter_paintEngine(QPrinterH handle)
{
	return (QPaintEngineH) ((QPrinter *)handle)->paintEngine();
}

QPrintEngineH QPrinter_printEngine(QPrinterH handle)
{
	return (QPrintEngineH) ((QPrinter *)handle)->printEngine();
}

void QPrinter_setFromTo(QPrinterH handle, int fromPage, int toPage)
{
	((QPrinter *)handle)->setFromTo(fromPage, toPage);
}

int QPrinter_fromPage(QPrinterH handle)
{
	return (int) ((QPrinter *)handle)->fromPage();
}

int QPrinter_toPage(QPrinterH handle)
{
	return (int) ((QPrinter *)handle)->toPage();
}

void QPrinter_setPrintRange(QPrinterH handle, QPrinter::PrintRange range)
{
	((QPrinter *)handle)->setPrintRange(range);
}

QPrinter::PrintRange QPrinter_printRange(QPrinterH handle)
{
	return (QPrinter::PrintRange) ((QPrinter *)handle)->printRange();
}

void QPrinter_setPageMargins(QPrinterH handle, qreal left, qreal top, qreal right, qreal bottom, QPrinter::Unit unit)
{
	((QPrinter *)handle)->setPageMargins(left, top, right, bottom, unit);
}

void QPrinter_getPageMargins(QPrinterH handle, qreal* left, qreal* top, qreal* right, qreal* bottom, QPrinter::Unit unit)
{
	((QPrinter *)handle)->getPageMargins(left, top, right, bottom, unit);
}

#if defined MSWINDOWS
void QPrinter_setWinPageSize(QPrinterH handle, int winPageSize)
{
	((QPrinter *)handle)->setWinPageSize(winPageSize);
}

int QPrinter_winPageSize(QPrinterH handle)
{
	return (int) ((QPrinter *)handle)->winPageSize();
}

#endif
