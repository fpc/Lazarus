//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QPRINTPREVIEWWIDGET_HOOK_C_H
#define QPRINTPREVIEWWIDGET_HOOK_C_H

#include "qprintpreviewwidget_hook.h"

C_EXPORT QPrintPreviewWidget_hookH QPrintPreviewWidget_hook_Create(QObjectH handle);
C_EXPORT void QPrintPreviewWidget_hook_Destroy(QPrintPreviewWidget_hookH handle);
C_EXPORT void QPrintPreviewWidget_hook_hook_paintRequested(QPrintPreviewWidget_hookH handle, QHookH hook);
C_EXPORT void QPrintPreviewWidget_hook_hook_previewChanged(QPrintPreviewWidget_hookH handle, QHookH hook);

#endif
