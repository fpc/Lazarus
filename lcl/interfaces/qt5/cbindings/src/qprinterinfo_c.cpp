//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qprinterinfo_c.h"

QPrinterInfoH QPrinterInfo_Create()
{
	return (QPrinterInfoH) new QPrinterInfo();
}

void QPrinterInfo_Destroy(QPrinterInfoH handle)
{
	delete (QPrinterInfo *)handle;
}

QPrinterInfoH QPrinterInfo_Create2(const QPrinterInfoH other)
{
	return (QPrinterInfoH) new QPrinterInfo(*(const QPrinterInfo*)other);
}

QPrinterInfoH QPrinterInfo_Create3(const QPrinterH printer)
{
	return (QPrinterInfoH) new QPrinterInfo(*(const QPrinter*)printer);
}

void QPrinterInfo_printerName(QPrinterInfoH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QPrinterInfo *)handle)->printerName();
	copyQStringToPWideString(t_retval, retval);
}

void QPrinterInfo_description(QPrinterInfoH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QPrinterInfo *)handle)->description();
	copyQStringToPWideString(t_retval, retval);
}

void QPrinterInfo_location(QPrinterInfoH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QPrinterInfo *)handle)->location();
	copyQStringToPWideString(t_retval, retval);
}

void QPrinterInfo_makeAndModel(QPrinterInfoH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QPrinterInfo *)handle)->makeAndModel();
	copyQStringToPWideString(t_retval, retval);
}

bool QPrinterInfo_isNull(QPrinterInfoH handle)
{
	return (bool) ((QPrinterInfo *)handle)->isNull();
}

bool QPrinterInfo_isDefault(QPrinterInfoH handle)
{
	return (bool) ((QPrinterInfo *)handle)->isDefault();
}

void QPrinterInfo_supportedPaperSizes(QPrinterInfoH handle, PPtrIntArray retval)
{
	QList<QPrinter::PaperSize> t_retval;
	t_retval = ((QPrinterInfo *)handle)->supportedPaperSizes();
	copyQListTemplateToPtrIntArray(t_retval, retval);
}

void QPrinterInfo_availablePrinters(PPtrIntArray retval)
{
	QList<QPrinterInfo> t_retval;
	t_retval = QPrinterInfo::availablePrinters();
	copyQListTemplateToPtrIntArrayWithNew(t_retval, retval);
}

void QPrinterInfo_availablePrinterNames(QStringListH retval)
{
  *(QStringList *)retval = QPrinterInfo::availablePrinterNames();
}

void QPrinterInfo_defaultPrinter(QPrinterInfoH retval)
{
	*(QPrinterInfo *)retval = QPrinterInfo::defaultPrinter();
}

void QPrinterInfo_printerInfo(QPrinterInfoH retval, PWideString printerName)
{
	QString t_printerName;
	copyPWideStringToQString(printerName, t_printerName);
	*(QPrinterInfo *)retval = QPrinterInfo::printerInfo(t_printerName);
}

