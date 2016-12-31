//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QWEBSECURITYORIGIN_HOOK_C_H
#define QWEBSECURITYORIGIN_HOOK_C_H

#include "qwebsecurityorigin_hook.h"

C_EXPORT QWebSecurityOrigin_hookH QWebSecurityOrigin_hook_Create(QObjectH handle);
C_EXPORT void QWebSecurityOrigin_hook_Destroy(QWebSecurityOrigin_hookH handle);

#endif
