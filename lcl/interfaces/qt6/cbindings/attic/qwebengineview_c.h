//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QWEBENGINEVIEW_C_H
#define QWEBENGINEVIEW_C_H

#include <QtWebEngineWidgets>
#include "pascalbind.h"

C_EXPORT QWebEngineViewH QWebEngineView_Create(QWidgetH parent);
C_EXPORT void QWebEngineView_Destroy(QWebEngineViewH handle);
C_EXPORT QWebEnginePageH QWebEngineView_page(QWebEngineViewH handle);
C_EXPORT void QWebEngineView_setPage(QWebEngineViewH handle, QWebEnginePageH page);
C_EXPORT void QWebEngineView_load(QWebEngineViewH handle, const QUrlH url);
/* C_EXPORT void QWebEngineView_load2(QWebEngineViewH handle, const QNetworkRequestH request, QNetworkAccessManager::Operation operation, const QByteArrayH body); */
C_EXPORT void QWebEngineView_setHtml(QWebEngineViewH handle, PWideString html, const QUrlH baseUrl);
C_EXPORT void QWebEngineView_setContent(QWebEngineViewH handle, const QByteArrayH data, PWideString mimeType, const QUrlH baseUrl);
C_EXPORT QWebEngineHistoryH QWebEngineView_history(QWebEngineViewH handle);
C_EXPORT QWebEngineSettingsH QWebEngineView_settings(QWebEngineViewH handle);
C_EXPORT void QWebEngineView_title(QWebEngineViewH handle, PWideString retval);
C_EXPORT void QWebEngineView_setUrl(QWebEngineViewH handle, const QUrlH url);
C_EXPORT void QWebEngineView_url(QWebEngineViewH handle, QUrlH retval);
C_EXPORT void QWebEngineView_icon(QWebEngineViewH handle, QIconH retval);
C_EXPORT bool QWebEngineView_hasSelection(QWebEngineViewH handle);
C_EXPORT void QWebEngineView_selectedText(QWebEngineViewH handle, PWideString retval);
/* C_EXPORT void QWebEngineView_selectedHtml(QWebEngineViewH handle, PWideString retval); */
C_EXPORT QActionH QWebEngineView_pageAction(QWebEngineViewH handle, QWebEnginePage::WebAction action);
C_EXPORT void QWebEngineView_triggerPageAction(QWebEngineViewH handle, QWebEnginePage::WebAction action, bool checked);
/* C_EXPORT bool QWebEngineView_isModified(QWebEngineViewH handle); */
/* C_EXPORT void QWebEngineView_inputMethodQuery(QWebEngineViewH handle, QVariantH retval, Qt::InputMethodQuery property); */
C_EXPORT void QWebEngineView_sizeHint(QWebEngineViewH handle, PSize retval);
C_EXPORT qreal QWebEngineView_zoomFactor(QWebEngineViewH handle);
C_EXPORT void QWebEngineView_setZoomFactor(QWebEngineViewH handle, qreal factor);
/* C_EXPORT void QWebEngineView_setTextSizeMultiplier(QWebEngineViewH handle, qreal factor);
C_EXPORT qreal QWebEngineView_textSizeMultiplier(QWebEngineViewH handle); 
C_EXPORT unsigned int QWebEngineView_renderHints(QWebEngineViewH handle);
C_EXPORT void QWebEngineView_setRenderHints(QWebEngineViewH handle, unsigned int hints);
C_EXPORT void QWebEngineView_setRenderHint(QWebEngineViewH handle, QPainter::RenderHint hint, bool enabled);*/
C_EXPORT bool QWebEngineView_findText(QWebEngineViewH handle, PWideString subString, unsigned int options);
C_EXPORT bool QWebEngineView_event(QWebEngineViewH handle, QEventH AnonParam1);
C_EXPORT void QWebEngineView_stop(QWebEngineViewH handle);
C_EXPORT void QWebEngineView_back(QWebEngineViewH handle);
C_EXPORT void QWebEngineView_forward(QWebEngineViewH handle);
C_EXPORT void QWebEngineView_reload(QWebEngineViewH handle);
/* C_EXPORT void QWebEngineView_print(QWebEngineViewH handle, QPrinterH AnonParam1); */

#endif
