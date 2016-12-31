//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QLCLWEBVIEW_C_H
#define QLCLWEBVIEW_C_H

#include "qlclwebview.h"
#include "pascalbind.h"

C_EXPORT QLCLWebViewH QLCLWebView_Create(QWidgetH parent);
C_EXPORT void QLCLWebView_Destroy(QLCLWebViewH handle);
C_EXPORT void QLCLWebView_override_createWindow(QLCLWebViewH handle, const QOverrideHook hook);

#endif
