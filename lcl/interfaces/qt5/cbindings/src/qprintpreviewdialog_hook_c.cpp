//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qprintpreviewdialog_hook_c.h"

QPrintPreviewDialog_hookH QPrintPreviewDialog_hook_Create(QObjectH handle)
{
	return (QPrintPreviewDialog_hookH) new QPrintPreviewDialog_hook((QObject*)handle);
}

void QPrintPreviewDialog_hook_Destroy(QPrintPreviewDialog_hookH handle)
{
	delete (QPrintPreviewDialog_hook *)handle;
}

void QPrintPreviewDialog_hook_hook_paintRequested(QPrintPreviewDialog_hookH handle, QHookH hook)
{
	((QPrintPreviewDialog_hook *)handle)->hook_paintRequested(hook);
}

