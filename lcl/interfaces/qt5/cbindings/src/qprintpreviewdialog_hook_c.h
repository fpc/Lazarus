//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QPRINTPREVIEWDIALOG_HOOK_C_H
#define QPRINTPREVIEWDIALOG_HOOK_C_H

#include "qprintpreviewdialog_hook.h"

C_EXPORT QPrintPreviewDialog_hookH QPrintPreviewDialog_hook_Create(QObjectH handle);
C_EXPORT void QPrintPreviewDialog_hook_Destroy(QPrintPreviewDialog_hookH handle);
C_EXPORT void QPrintPreviewDialog_hook_hook_paintRequested(QPrintPreviewDialog_hookH handle, QHookH hook);

#endif
