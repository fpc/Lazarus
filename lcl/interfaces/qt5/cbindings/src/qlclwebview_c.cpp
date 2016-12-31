//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qlclwebview_c.h"

QLCLWebViewH QLCLWebView_Create(QWidgetH parent)
{
	return (QLCLWebViewH) new QLCLWebView((QWidget*)parent);
}

void QLCLWebView_Destroy(QLCLWebViewH handle)
{
	delete (QLCLWebView *)handle;
}

void QLCLWebView_override_createWindow(QLCLWebViewH handle, const QOverrideHook hook)
{
	((QLCLWebView *)handle)->override_createWindow(hook);
}

