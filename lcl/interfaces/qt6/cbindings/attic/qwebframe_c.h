//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QWEBFRAME_C_H
#define QWEBFRAME_C_H

#include <QtWebKitWidgets>
#include "pascalbind.h"

C_EXPORT QWebHitTestResultH QWebHitTestResult_Create();
C_EXPORT void QWebHitTestResult_Destroy(QWebHitTestResultH handle);
C_EXPORT QWebHitTestResultH QWebHitTestResult_Create2(const QWebHitTestResultH other);
C_EXPORT bool QWebHitTestResult_isNull(QWebHitTestResultH handle);
C_EXPORT void QWebHitTestResult_pos(QWebHitTestResultH handle, PQtPoint retval);
C_EXPORT void QWebHitTestResult_boundingRect(QWebHitTestResultH handle, PRect retval);
C_EXPORT void QWebHitTestResult_enclosingBlockElement(QWebHitTestResultH handle, QWebElementH retval);
C_EXPORT void QWebHitTestResult_title(QWebHitTestResultH handle, PWideString retval);
C_EXPORT void QWebHitTestResult_linkText(QWebHitTestResultH handle, PWideString retval);
C_EXPORT void QWebHitTestResult_linkUrl(QWebHitTestResultH handle, QUrlH retval);
C_EXPORT void QWebHitTestResult_linkTitle(QWebHitTestResultH handle, QUrlH retval);
C_EXPORT QWebFrameH QWebHitTestResult_linkTargetFrame(QWebHitTestResultH handle);
C_EXPORT void QWebHitTestResult_linkElement(QWebHitTestResultH handle, QWebElementH retval);
C_EXPORT void QWebHitTestResult_alternateText(QWebHitTestResultH handle, PWideString retval);
C_EXPORT void QWebHitTestResult_imageUrl(QWebHitTestResultH handle, QUrlH retval);
C_EXPORT void QWebHitTestResult_pixmap(QWebHitTestResultH handle, QPixmapH retval);
C_EXPORT bool QWebHitTestResult_isContentEditable(QWebHitTestResultH handle);
C_EXPORT bool QWebHitTestResult_isContentSelected(QWebHitTestResultH handle);
C_EXPORT void QWebHitTestResult_element(QWebHitTestResultH handle, QWebElementH retval);
C_EXPORT QWebFrameH QWebHitTestResult_frame(QWebHitTestResultH handle);
C_EXPORT QWebPageH QWebFrame_page(QWebFrameH handle);
C_EXPORT void QWebFrame_load(QWebFrameH handle, const QUrlH url);
C_EXPORT void QWebFrame_load2(QWebFrameH handle, const QNetworkRequestH request, QNetworkAccessManager::Operation operation, const QByteArrayH body);
C_EXPORT void QWebFrame_setHtml(QWebFrameH handle, PWideString html, const QUrlH baseUrl);
C_EXPORT void QWebFrame_setContent(QWebFrameH handle, const QByteArrayH data, PWideString mimeType, const QUrlH baseUrl);
C_EXPORT void QWebFrame_addToJavaScriptWindowObject(QWebFrameH handle, PWideString name, QObjectH object, QWebFrame::ValueOwnership ownership);
C_EXPORT void QWebFrame_toHtml(QWebFrameH handle, PWideString retval);
C_EXPORT void QWebFrame_toPlainText(QWebFrameH handle, PWideString retval);
C_EXPORT void QWebFrame_title(QWebFrameH handle, PWideString retval);
C_EXPORT void QWebFrame_setUrl(QWebFrameH handle, const QUrlH url);
C_EXPORT void QWebFrame_url(QWebFrameH handle, QUrlH retval);
C_EXPORT void QWebFrame_requestedUrl(QWebFrameH handle, QUrlH retval);
C_EXPORT void QWebFrame_baseUrl(QWebFrameH handle, QUrlH retval);
C_EXPORT void QWebFrame_icon(QWebFrameH handle, QIconH retval);
C_EXPORT void QWebFrame_frameName(QWebFrameH handle, PWideString retval);
C_EXPORT QWebFrameH QWebFrame_parentFrame(QWebFrameH handle);
C_EXPORT void QWebFrame_childFrames(QWebFrameH handle, PPtrIntArray retval);
C_EXPORT Qt::ScrollBarPolicy QWebFrame_scrollBarPolicy(QWebFrameH handle, Qt::Orientation orientation);
C_EXPORT void QWebFrame_setScrollBarPolicy(QWebFrameH handle, Qt::Orientation orientation, Qt::ScrollBarPolicy policy);
C_EXPORT void QWebFrame_setScrollBarValue(QWebFrameH handle, Qt::Orientation orientation, int value);
C_EXPORT int QWebFrame_scrollBarValue(QWebFrameH handle, Qt::Orientation orientation);
C_EXPORT int QWebFrame_scrollBarMinimum(QWebFrameH handle, Qt::Orientation orientation);
C_EXPORT int QWebFrame_scrollBarMaximum(QWebFrameH handle, Qt::Orientation orientation);
C_EXPORT void QWebFrame_scrollBarGeometry(QWebFrameH handle, PRect retval, Qt::Orientation orientation);
C_EXPORT void QWebFrame_scroll(QWebFrameH handle, int AnonParam1, int AnonParam2);
C_EXPORT void QWebFrame_scrollPosition(QWebFrameH handle, PQtPoint retval);
C_EXPORT void QWebFrame_setScrollPosition(QWebFrameH handle, const QPointH pos);
C_EXPORT void QWebFrame_scrollToAnchor(QWebFrameH handle, PWideString anchor);
C_EXPORT void QWebFrame_render(QWebFrameH handle, QPainterH AnonParam1, const QRegionH clip);
C_EXPORT void QWebFrame_render2(QWebFrameH handle, QPainterH AnonParam1, unsigned int layer, const QRegionH clip);
C_EXPORT void QWebFrame_setTextSizeMultiplier(QWebFrameH handle, qreal factor);
C_EXPORT qreal QWebFrame_textSizeMultiplier(QWebFrameH handle);
C_EXPORT qreal QWebFrame_zoomFactor(QWebFrameH handle);
C_EXPORT void QWebFrame_setZoomFactor(QWebFrameH handle, qreal factor);
C_EXPORT bool QWebFrame_hasFocus(QWebFrameH handle);
C_EXPORT void QWebFrame_setFocus(QWebFrameH handle);
C_EXPORT void QWebFrame_pos(QWebFrameH handle, PQtPoint retval);
C_EXPORT void QWebFrame_geometry(QWebFrameH handle, PRect retval);
C_EXPORT void QWebFrame_contentsSize(QWebFrameH handle, PSize retval);
C_EXPORT void QWebFrame_documentElement(QWebFrameH handle, QWebElementH retval);
C_EXPORT void QWebFrame_findAllElements(QWebFrameH handle, QWebElementCollectionH retval, PWideString selectorQuery);
C_EXPORT void QWebFrame_findFirstElement(QWebFrameH handle, QWebElementH retval, PWideString selectorQuery);
C_EXPORT void QWebFrame_hitTestContent(QWebFrameH handle, QWebHitTestResultH retval, const QPointH pos);
C_EXPORT bool QWebFrame_event(QWebFrameH handle, QEventH AnonParam1);
C_EXPORT void QWebFrame_evaluateJavaScript(QWebFrameH handle, QVariantH retval, PWideString scriptSource);
C_EXPORT void QWebFrame_print(QWebFrameH handle, QPrinterH printer);

#endif
