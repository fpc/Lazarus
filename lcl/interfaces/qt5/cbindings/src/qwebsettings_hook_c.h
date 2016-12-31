//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QWEBSETTINGS_HOOK_C_H
#define QWEBSETTINGS_HOOK_C_H

#include "qwebsettings_hook.h"

C_EXPORT QWebSettings_hookH QWebSettings_hook_Create(QObjectH handle);
C_EXPORT void QWebSettings_hook_Destroy(QWebSettings_hookH handle);

#endif
