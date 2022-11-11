//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qlclwebpage_c.h"

QLCLWebPageH QLCLWebPage_Create(QObjectH parent)
{
	return (QLCLWebPageH) new QLCLWebPage((QObject*)parent);
}

void QLCLWebPage_Destroy(QLCLWebPageH handle)
{
	delete (QLCLWebPage *)handle;
}

void QLCLWebPage_override_userAgentForUrl(QLCLWebPageH handle, const QOverrideHook hook)
{
	((QLCLWebPage *)handle)->override_userAgentForUrl(hook);
}

void QLCLWebPage_DefaultUserAgentForUrl(QLCLWebPageH handle, PWideString retval, const QUrlH url)
{
	QString t_retval;
	t_retval = ((QLCLWebPage *)handle)->DefaultUserAgentForUrl(*(const QUrl*)url);
	copyQStringToPWideString(t_retval, retval);
}

