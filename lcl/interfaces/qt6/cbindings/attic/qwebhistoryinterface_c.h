//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QWEBHISTORYINTERFACE_C_H
#define QWEBHISTORYINTERFACE_C_H

#include <QtWebKitWidgets>
#include "pascalbind.h"

C_EXPORT void QWebHistoryInterface_setDefaultInterface(QWebHistoryInterfaceH defaultInterface);
C_EXPORT QWebHistoryInterfaceH QWebHistoryInterface_defaultInterface();
C_EXPORT bool QWebHistoryInterface_historyContains(QWebHistoryInterfaceH handle, PWideString url);
C_EXPORT void QWebHistoryInterface_addHistoryEntry(QWebHistoryInterfaceH handle, PWideString url);

#endif
