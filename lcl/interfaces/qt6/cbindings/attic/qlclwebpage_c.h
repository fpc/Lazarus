//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QLCLWEBPAGE_C_H
#define QLCLWEBPAGE_C_H

#include "qlclwebpage.h"
#include "pascalbind.h"

C_EXPORT QLCLWebPageH QLCLWebPage_Create(QObjectH parent);
C_EXPORT void QLCLWebPage_Destroy(QLCLWebPageH handle);
C_EXPORT void QLCLWebPage_override_userAgentForUrl(QLCLWebPageH handle, const QOverrideHook hook);
C_EXPORT void QLCLWebPage_DefaultUserAgentForUrl(QLCLWebPageH handle, PWideString retval, const QUrlH url);

#endif
