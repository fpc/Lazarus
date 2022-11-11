//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qprintpreviewdialog_c.h"

QPrintPreviewDialogH QPrintPreviewDialog_Create(QWidgetH parent, unsigned int flags)
{
	return (QPrintPreviewDialogH) new QPrintPreviewDialog((QWidget*)parent, (Qt::WindowFlags)flags);
}

void QPrintPreviewDialog_Destroy(QPrintPreviewDialogH handle)
{
	delete (QPrintPreviewDialog *)handle;
}

QPrintPreviewDialogH QPrintPreviewDialog_Create2(QPrinterH printer, QWidgetH parent, unsigned int flags)
{
	return (QPrintPreviewDialogH) new QPrintPreviewDialog((QPrinter*)printer, (QWidget*)parent, (Qt::WindowFlags)flags);
}

void QPrintPreviewDialog_open(QPrintPreviewDialogH handle, QObjectH receiver, const char* member)
{
	((QPrintPreviewDialog *)handle)->open((QObject*)receiver, member);
}

QPrinterH QPrintPreviewDialog_printer(QPrintPreviewDialogH handle)
{
	return (QPrinterH) ((QPrintPreviewDialog *)handle)->printer();
}

void QPrintPreviewDialog_setVisible(QPrintPreviewDialogH handle, bool visible)
{
	((QPrintPreviewDialog *)handle)->setVisible(visible);
}

void QPrintPreviewDialog_done(QPrintPreviewDialogH handle, int result)
{
	((QPrintPreviewDialog *)handle)->done(result);
}

