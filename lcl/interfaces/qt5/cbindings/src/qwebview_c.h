//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QWEBVIEW_C_H
#define QWEBVIEW_C_H

#include <QtWebKitWidgets>
#include "pascalbind.h"

C_EXPORT QWebViewH QWebView_Create(QWidgetH parent);
C_EXPORT void QWebView_Destroy(QWebViewH handle);
C_EXPORT QWebPageH QWebView_page(QWebViewH handle);
C_EXPORT void QWebView_setPage(QWebViewH handle, QWebPageH page);
C_EXPORT void QWebView_load(QWebViewH handle, const QUrlH url);
C_EXPORT void QWebView_load2(QWebViewH handle, const QNetworkRequestH request, QNetworkAccessManager::Operation operation, const QByteArrayH body);
C_EXPORT void QWebView_setHtml(QWebViewH handle, PWideString html, const QUrlH baseUrl);
C_EXPORT void QWebView_setContent(QWebViewH handle, const QByteArrayH data, PWideString mimeType, const QUrlH baseUrl);
C_EXPORT QWebHistoryH QWebView_history(QWebViewH handle);
C_EXPORT QWebSettingsH QWebView_settings(QWebViewH handle);
C_EXPORT void QWebView_title(QWebViewH handle, PWideString retval);
C_EXPORT void QWebView_setUrl(QWebViewH handle, const QUrlH url);
C_EXPORT void QWebView_url(QWebViewH handle, QUrlH retval);
C_EXPORT void QWebView_icon(QWebViewH handle, QIconH retval);
C_EXPORT bool QWebView_hasSelection(QWebViewH handle);
C_EXPORT void QWebView_selectedText(QWebViewH handle, PWideString retval);
C_EXPORT void QWebView_selectedHtml(QWebViewH handle, PWideString retval);
C_EXPORT QActionH QWebView_pageAction(QWebViewH handle, QWebPage::WebAction action);
C_EXPORT void QWebView_triggerPageAction(QWebViewH handle, QWebPage::WebAction action, bool checked);
C_EXPORT bool QWebView_isModified(QWebViewH handle);
C_EXPORT void QWebView_inputMethodQuery(QWebViewH handle, QVariantH retval, Qt::InputMethodQuery property);
C_EXPORT void QWebView_sizeHint(QWebViewH handle, PSize retval);
C_EXPORT qreal QWebView_zoomFactor(QWebViewH handle);
C_EXPORT void QWebView_setZoomFactor(QWebViewH handle, qreal factor);
C_EXPORT void QWebView_setTextSizeMultiplier(QWebViewH handle, qreal factor);
C_EXPORT qreal QWebView_textSizeMultiplier(QWebViewH handle);
C_EXPORT unsigned int QWebView_renderHints(QWebViewH handle);
C_EXPORT void QWebView_setRenderHints(QWebViewH handle, unsigned int hints);
C_EXPORT void QWebView_setRenderHint(QWebViewH handle, QPainter::RenderHint hint, bool enabled);
C_EXPORT bool QWebView_findText(QWebViewH handle, PWideString subString, unsigned int options);
C_EXPORT bool QWebView_event(QWebViewH handle, QEventH AnonParam1);
C_EXPORT void QWebView_stop(QWebViewH handle);
C_EXPORT void QWebView_back(QWebViewH handle);
C_EXPORT void QWebView_forward(QWebViewH handle);
C_EXPORT void QWebView_reload(QWebViewH handle);
C_EXPORT void QWebView_print(QWebViewH handle, QPrinterH AnonParam1);

#endif
