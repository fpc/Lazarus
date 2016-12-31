//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QWEBHISTORYINTERFACE_HOOK_C_H
#define QWEBHISTORYINTERFACE_HOOK_C_H

#include "qwebhistoryinterface_hook.h"

C_EXPORT QWebHistoryInterface_hookH QWebHistoryInterface_hook_Create(QObjectH handle);
C_EXPORT void QWebHistoryInterface_hook_Destroy(QWebHistoryInterface_hookH handle);

#endif
