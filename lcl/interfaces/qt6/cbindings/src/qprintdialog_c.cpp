//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qprintdialog_c.h"

QPrintDialogH QPrintDialog_Create(QPrinterH printer, QWidgetH parent)
{
	return (QPrintDialogH) new QPrintDialog((QPrinter*)printer, (QWidget*)parent);
}

void QPrintDialog_Destroy(QPrintDialogH handle)
{
	delete (QPrintDialog *)handle;
}

QPrintDialogH QPrintDialog_Create2(QWidgetH parent)
{
	return (QPrintDialogH) new QPrintDialog((QWidget*)parent);
}

int QPrintDialog_exec(QPrintDialogH handle)
{
	return (int) ((QPrintDialog *)handle)->exec();
}

#if defined BINUX
void QPrintDialog_accept(QPrintDialogH handle)
{
	((QPrintDialog *)handle)->accept();
}

#endif
void QPrintDialog_done(QPrintDialogH handle, int result)
{
	((QPrintDialog *)handle)->done(result);
}

void QPrintDialog_setOption(QPrintDialogH handle, QAbstractPrintDialog::PrintDialogOption option, bool on)
{
	((QPrintDialog *)handle)->setOption(option, on);
}

bool QPrintDialog_testOption(QPrintDialogH handle, QAbstractPrintDialog::PrintDialogOption option)
{
	return (bool) ((QPrintDialog *)handle)->testOption(option);
}

void QPrintDialog_setOptions(QPrintDialogH handle, unsigned int options)
{
	((QPrintDialog *)handle)->setOptions((QAbstractPrintDialog::PrintDialogOptions)options);
}

unsigned int QPrintDialog_options(QPrintDialogH handle)
{
	return (unsigned int) ((QPrintDialog *)handle)->options();
}

void QPrintDialog_setVisible(QPrintDialogH handle, bool visible)
{
	((QPrintDialog *)handle)->setVisible(visible);
}

void QPrintDialog_open(QPrintDialogH handle, QObjectH receiver, const char* member)
{
	((QPrintDialog *)handle)->open((QObject*)receiver, member);
}

