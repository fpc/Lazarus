//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qabstractprintdialog_c.h"

int QAbstractPrintDialog_exec(QAbstractPrintDialogH handle)
{
	return (int) ((QAbstractPrintDialog *)handle)->exec();
}

void QAbstractPrintDialog_setPrintRange(QAbstractPrintDialogH handle, QAbstractPrintDialog::PrintRange range)
{
	((QAbstractPrintDialog *)handle)->setPrintRange(range);
}

QAbstractPrintDialog::PrintRange QAbstractPrintDialog_printRange(QAbstractPrintDialogH handle)
{
	return (QAbstractPrintDialog::PrintRange) ((QAbstractPrintDialog *)handle)->printRange();
}

void QAbstractPrintDialog_setMinMax(QAbstractPrintDialogH handle, int min, int max)
{
	((QAbstractPrintDialog *)handle)->setMinMax(min, max);
}

int QAbstractPrintDialog_minPage(QAbstractPrintDialogH handle)
{
	return (int) ((QAbstractPrintDialog *)handle)->minPage();
}

int QAbstractPrintDialog_maxPage(QAbstractPrintDialogH handle)
{
	return (int) ((QAbstractPrintDialog *)handle)->maxPage();
}

void QAbstractPrintDialog_setFromTo(QAbstractPrintDialogH handle, int fromPage, int toPage)
{
	((QAbstractPrintDialog *)handle)->setFromTo(fromPage, toPage);
}

int QAbstractPrintDialog_fromPage(QAbstractPrintDialogH handle)
{
	return (int) ((QAbstractPrintDialog *)handle)->fromPage();
}

int QAbstractPrintDialog_toPage(QAbstractPrintDialogH handle)
{
	return (int) ((QAbstractPrintDialog *)handle)->toPage();
}

QPrinterH QAbstractPrintDialog_printer(QAbstractPrintDialogH handle)
{
	return (QPrinterH) ((QAbstractPrintDialog *)handle)->printer();
}

