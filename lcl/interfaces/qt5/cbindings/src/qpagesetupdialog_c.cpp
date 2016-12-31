//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qpagesetupdialog_c.h"

QPageSetupDialogH QPageSetupDialog_Create(QPrinterH printer, QWidgetH parent)
{
	return (QPageSetupDialogH) new QPageSetupDialog((QPrinter*)printer, (QWidget*)parent);
}

void QPageSetupDialog_Destroy(QPageSetupDialogH handle)
{
	delete (QPageSetupDialog *)handle;
}

QPageSetupDialogH QPageSetupDialog_Create2(QWidgetH parent)
{
	return (QPageSetupDialogH) new QPageSetupDialog((QWidget*)parent);
}

int QPageSetupDialog_exec(QPageSetupDialogH handle)
{
	return (int) ((QPageSetupDialog *)handle)->exec();
}

void QPageSetupDialog_open(QPageSetupDialogH handle, QObjectH receiver, const char* member)
{
	((QPageSetupDialog *)handle)->open((QObject*)receiver, member);
}

void QPageSetupDialog_done(QPageSetupDialogH handle, int result)
{
	((QPageSetupDialog *)handle)->done(result);
}

QPrinterH QPageSetupDialog_printer(QPageSetupDialogH handle)
{
	return (QPrinterH) ((QPageSetupDialog *)handle)->printer();
}

#if defined MSWINDOWS || DARWIN
void QPageSetupDialog_setVisible(QPageSetupDialogH handle, bool visible)
{
	((QPageSetupDialog *)handle)->setVisible(visible);
}

#endif
