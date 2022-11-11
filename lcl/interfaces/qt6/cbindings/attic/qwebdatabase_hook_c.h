//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QWEBDATABASE_HOOK_C_H
#define QWEBDATABASE_HOOK_C_H

#include "qwebdatabase_hook.h"

C_EXPORT QWebDatabase_hookH QWebDatabase_hook_Create(QObjectH handle);
C_EXPORT void QWebDatabase_hook_Destroy(QWebDatabase_hookH handle);

#endif
