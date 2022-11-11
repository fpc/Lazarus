//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qabstractprintdialog_hook_c.h"

QAbstractPrintDialog_hookH QAbstractPrintDialog_hook_Create(QObjectH handle)
{
	return (QAbstractPrintDialog_hookH) new QAbstractPrintDialog_hook((QObject*)handle);
}

void QAbstractPrintDialog_hook_Destroy(QAbstractPrintDialog_hookH handle)
{
	delete (QAbstractPrintDialog_hook *)handle;
}

