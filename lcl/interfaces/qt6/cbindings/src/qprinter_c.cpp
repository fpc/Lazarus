//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
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

void QPrinter_setOrientation(QPrinterH handle, QPageLayout::Orientation AnonParam1)
{
	((QPrinter *)handle)->setPageOrientation(AnonParam1);
}

QPageLayout::Orientation QPrinter_orientation(QPrinterH handle)
{
	return (QPageLayout::Orientation) ((QPrinter *)handle)->pageLayout().orientation();
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

#if defined BINUX
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

void QPrinter_setPageMargins(QPrinterH handle, qreal left, qreal top, qreal right, qreal bottom, QPageLayout::Unit unit)
{
	((QPrinter *)handle)->setPageMargins(QMarginsF(left, top , right ,bottom), unit);
}

void QPrinter_getPageMargins(QPrinterH handle, qreal* left, qreal* top, qreal* right, qreal* bottom, QPageLayout::Unit unit)
{
	QMarginsF m = ((QPrinter *)handle)->pageLayout().margins(unit);
  *left = m.left();
  *top = m.top();
  *right = m.right();
  *bottom = m.bottom();
}

QPagedPaintDevice::PdfVersion QPrinter_pdfVersion(QPrinterH handle)
{
  return (QPagedPaintDevice::PdfVersion)((QPrinter *)handle)->pdfVersion();
}

void QPrinter_setPdfVersion(QPrinterH handle, QPagedPaintDevice::PdfVersion version)
{
 ((QPrinter *)handle)->setPdfVersion((QPagedPaintDevice::PdfVersion) version);
}

void QPrinter_pageRect(QPrinterH handle, QPrinter::Unit unit, QRectFH retval)
{
  *(QRectF *)retval = ((QPrinter *)handle)->pageRect((QPrinter::Unit) unit);
}

void QPrinter_pageRect2(QPrinterH handle, QPrinter::Unit unit, PRect retval)
{
	QRect t_retval;
  t_retval = ((QPrinter *)handle)->pageRect((QPrinter::Unit) unit).toRect();
	copyQRectToPRect(t_retval, retval);
}

void QPrinter_paperRect(QPrinterH handle, QPrinter::Unit unit, QRectFH retval)
{
  *(QRectF *)retval = ((QPrinter *)handle)->paperRect((QPrinter::Unit) unit);
}

void QPrinter_paperRect2(QPrinterH handle, QPrinter::Unit unit, PRect retval)
{
	QRect t_retval;
  t_retval = ((QPrinter *)handle)->paperRect((QPrinter::Unit) unit).toRect();
	copyQRectToPRect(t_retval, retval);
}

void QPrinter_pageLayout(QPrinterH handle, QPageLayoutH retval)
{
  *(QPageLayout *)retval = ((QPrinter *)handle)->pageLayout();
}

void QPrinter_pageSize(QPrinterH handle, QPageSizeH retval)
{
  *(QPageSize *)retval = ((QPrinter *)handle)->pageLayout().pageSize();
}

void QPrinter_setPageLayout(QPrinterH handle, const QPageLayoutH pageLayout)
{
  ((QPrinter *)handle)->setPageLayout(*(const QPageLayout*) pageLayout);
}

void QPrinter_setPageSize(QPrinterH handle, const QPageSizeH pageSize)
{
  ((QPrinter *)handle)->setPageSize(*(const QPageSize*) pageSize);
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
