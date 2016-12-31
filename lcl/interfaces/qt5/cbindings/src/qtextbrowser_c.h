//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QTEXTBROWSER_C_H
#define QTEXTBROWSER_C_H

#include <QtWidgets>
#include "pascalbind.h"

C_EXPORT QTextBrowserH QTextBrowser_Create(QWidgetH parent);
C_EXPORT void QTextBrowser_Destroy(QTextBrowserH handle);
C_EXPORT void QTextBrowser_source(QTextBrowserH handle, QUrlH retval);
C_EXPORT void QTextBrowser_searchPaths(QTextBrowserH handle, QStringListH retval);
C_EXPORT void QTextBrowser_setSearchPaths(QTextBrowserH handle, const QStringListH paths);
C_EXPORT void QTextBrowser_loadResource(QTextBrowserH handle, QVariantH retval, int type, const QUrlH name);
C_EXPORT bool QTextBrowser_isBackwardAvailable(QTextBrowserH handle);
C_EXPORT bool QTextBrowser_isForwardAvailable(QTextBrowserH handle);
C_EXPORT void QTextBrowser_clearHistory(QTextBrowserH handle);
C_EXPORT void QTextBrowser_historyTitle(QTextBrowserH handle, PWideString retval, int AnonParam1);
C_EXPORT void QTextBrowser_historyUrl(QTextBrowserH handle, QUrlH retval, int AnonParam1);
C_EXPORT int QTextBrowser_backwardHistoryCount(QTextBrowserH handle);
C_EXPORT int QTextBrowser_forwardHistoryCount(QTextBrowserH handle);
C_EXPORT bool QTextBrowser_openExternalLinks(QTextBrowserH handle);
C_EXPORT void QTextBrowser_setOpenExternalLinks(QTextBrowserH handle, bool open);
C_EXPORT bool QTextBrowser_openLinks(QTextBrowserH handle);
C_EXPORT void QTextBrowser_setOpenLinks(QTextBrowserH handle, bool open);
C_EXPORT void QTextBrowser_setSource(QTextBrowserH handle, const QUrlH name);
C_EXPORT void QTextBrowser_backward(QTextBrowserH handle);
C_EXPORT void QTextBrowser_forward(QTextBrowserH handle);
C_EXPORT void QTextBrowser_home(QTextBrowserH handle);
C_EXPORT void QTextBrowser_reload(QTextBrowserH handle);

#endif
