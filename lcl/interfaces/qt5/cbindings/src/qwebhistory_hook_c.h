//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QWEBHISTORY_HOOK_C_H
#define QWEBHISTORY_HOOK_C_H

#include "qwebhistory_hook.h"

C_EXPORT QWebHistoryItem_hookH QWebHistoryItem_hook_Create(QObjectH handle);
C_EXPORT void QWebHistoryItem_hook_Destroy(QWebHistoryItem_hookH handle);
C_EXPORT QWebHistory_hookH QWebHistory_hook_Create(QObjectH handle);
C_EXPORT void QWebHistory_hook_Destroy(QWebHistory_hookH handle);

#endif
