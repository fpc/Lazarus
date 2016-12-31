//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qwebframe_c.h"

QWebHitTestResultH QWebHitTestResult_Create()
{
	return (QWebHitTestResultH) new QWebHitTestResult();
}

void QWebHitTestResult_Destroy(QWebHitTestResultH handle)
{
	delete (QWebHitTestResult *)handle;
}

QWebHitTestResultH QWebHitTestResult_Create2(const QWebHitTestResultH other)
{
	return (QWebHitTestResultH) new QWebHitTestResult(*(const QWebHitTestResult*)other);
}

bool QWebHitTestResult_isNull(QWebHitTestResultH handle)
{
	return (bool) ((QWebHitTestResult *)handle)->isNull();
}

void QWebHitTestResult_pos(QWebHitTestResultH handle, PQtPoint retval)
{
	*(QPoint *)retval = ((QWebHitTestResult *)handle)->pos();
}

void QWebHitTestResult_boundingRect(QWebHitTestResultH handle, PRect retval)
{
	QRect t_retval;
	t_retval = ((QWebHitTestResult *)handle)->boundingRect();
	copyQRectToPRect(t_retval, retval);
}

void QWebHitTestResult_enclosingBlockElement(QWebHitTestResultH handle, QWebElementH retval)
{
	*(QWebElement *)retval = ((QWebHitTestResult *)handle)->enclosingBlockElement();
}

void QWebHitTestResult_title(QWebHitTestResultH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QWebHitTestResult *)handle)->title();
	copyQStringToPWideString(t_retval, retval);
}

void QWebHitTestResult_linkText(QWebHitTestResultH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QWebHitTestResult *)handle)->linkText();
	copyQStringToPWideString(t_retval, retval);
}

void QWebHitTestResult_linkUrl(QWebHitTestResultH handle, QUrlH retval)
{
	*(QUrl *)retval = ((QWebHitTestResult *)handle)->linkUrl();
}

void QWebHitTestResult_linkTitle(QWebHitTestResultH handle, QUrlH retval)
{
	*(QUrl *)retval = ((QWebHitTestResult *)handle)->linkTitle();
}

QWebFrameH QWebHitTestResult_linkTargetFrame(QWebHitTestResultH handle)
{
	return (QWebFrameH) ((QWebHitTestResult *)handle)->linkTargetFrame();
}

void QWebHitTestResult_linkElement(QWebHitTestResultH handle, QWebElementH retval)
{
	*(QWebElement *)retval = ((QWebHitTestResult *)handle)->linkElement();
}

void QWebHitTestResult_alternateText(QWebHitTestResultH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QWebHitTestResult *)handle)->alternateText();
	copyQStringToPWideString(t_retval, retval);
}

void QWebHitTestResult_imageUrl(QWebHitTestResultH handle, QUrlH retval)
{
	*(QUrl *)retval = ((QWebHitTestResult *)handle)->imageUrl();
}

void QWebHitTestResult_pixmap(QWebHitTestResultH handle, QPixmapH retval)
{
	*(QPixmap *)retval = ((QWebHitTestResult *)handle)->pixmap();
}

bool QWebHitTestResult_isContentEditable(QWebHitTestResultH handle)
{
	return (bool) ((QWebHitTestResult *)handle)->isContentEditable();
}

bool QWebHitTestResult_isContentSelected(QWebHitTestResultH handle)
{
	return (bool) ((QWebHitTestResult *)handle)->isContentSelected();
}

void QWebHitTestResult_element(QWebHitTestResultH handle, QWebElementH retval)
{
	*(QWebElement *)retval = ((QWebHitTestResult *)handle)->element();
}

QWebFrameH QWebHitTestResult_frame(QWebHitTestResultH handle)
{
	return (QWebFrameH) ((QWebHitTestResult *)handle)->frame();
}

QWebPageH QWebFrame_page(QWebFrameH handle)
{
	return (QWebPageH) ((QWebFrame *)handle)->page();
}

void QWebFrame_load(QWebFrameH handle, const QUrlH url)
{
	((QWebFrame *)handle)->load(*(const QUrl*)url);
}

void QWebFrame_load2(QWebFrameH handle, const QNetworkRequestH request, QNetworkAccessManager::Operation operation, const QByteArrayH body)
{
	((QWebFrame *)handle)->load(*(const QNetworkRequest*)request, operation, *(const QByteArray*)body);
}

void QWebFrame_setHtml(QWebFrameH handle, PWideString html, const QUrlH baseUrl)
{
	QString t_html;
	copyPWideStringToQString(html, t_html);
	((QWebFrame *)handle)->setHtml(t_html, *(const QUrl*)baseUrl);
}

void QWebFrame_setContent(QWebFrameH handle, const QByteArrayH data, PWideString mimeType, const QUrlH baseUrl)
{
	QString t_mimeType;
	copyPWideStringToQString(mimeType, t_mimeType);
	((QWebFrame *)handle)->setContent(*(const QByteArray*)data, t_mimeType, *(const QUrl*)baseUrl);
}

void QWebFrame_addToJavaScriptWindowObject(QWebFrameH handle, PWideString name, QObjectH object, QWebFrame::ValueOwnership ownership)
{
	QString t_name;
	copyPWideStringToQString(name, t_name);
	((QWebFrame *)handle)->addToJavaScriptWindowObject(t_name, (QObject*)object, ownership);
}

void QWebFrame_toHtml(QWebFrameH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QWebFrame *)handle)->toHtml();
	copyQStringToPWideString(t_retval, retval);
}

void QWebFrame_toPlainText(QWebFrameH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QWebFrame *)handle)->toPlainText();
	copyQStringToPWideString(t_retval, retval);
}

void QWebFrame_title(QWebFrameH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QWebFrame *)handle)->title();
	copyQStringToPWideString(t_retval, retval);
}

void QWebFrame_setUrl(QWebFrameH handle, const QUrlH url)
{
	((QWebFrame *)handle)->setUrl(*(const QUrl*)url);
}

void QWebFrame_url(QWebFrameH handle, QUrlH retval)
{
	*(QUrl *)retval = ((QWebFrame *)handle)->url();
}

void QWebFrame_requestedUrl(QWebFrameH handle, QUrlH retval)
{
	*(QUrl *)retval = ((QWebFrame *)handle)->requestedUrl();
}

void QWebFrame_baseUrl(QWebFrameH handle, QUrlH retval)
{
	*(QUrl *)retval = ((QWebFrame *)handle)->baseUrl();
}

void QWebFrame_icon(QWebFrameH handle, QIconH retval)
{
	*(QIcon *)retval = ((QWebFrame *)handle)->icon();
}

void QWebFrame_frameName(QWebFrameH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QWebFrame *)handle)->frameName();
	copyQStringToPWideString(t_retval, retval);
}

QWebFrameH QWebFrame_parentFrame(QWebFrameH handle)
{
	return (QWebFrameH) ((QWebFrame *)handle)->parentFrame();
}

void QWebFrame_childFrames(QWebFrameH handle, PPtrIntArray retval)
{
	QList<QWebFrame*> t_retval;
	t_retval = ((QWebFrame *)handle)->childFrames();
	copyQListTemplateToPtrIntArray(t_retval, retval);
}

Qt::ScrollBarPolicy QWebFrame_scrollBarPolicy(QWebFrameH handle, Qt::Orientation orientation)
{
	return (Qt::ScrollBarPolicy) ((QWebFrame *)handle)->scrollBarPolicy(orientation);
}

void QWebFrame_setScrollBarPolicy(QWebFrameH handle, Qt::Orientation orientation, Qt::ScrollBarPolicy policy)
{
	((QWebFrame *)handle)->setScrollBarPolicy(orientation, policy);
}

void QWebFrame_setScrollBarValue(QWebFrameH handle, Qt::Orientation orientation, int value)
{
	((QWebFrame *)handle)->setScrollBarValue(orientation, value);
}

int QWebFrame_scrollBarValue(QWebFrameH handle, Qt::Orientation orientation)
{
	return (int) ((QWebFrame *)handle)->scrollBarValue(orientation);
}

int QWebFrame_scrollBarMinimum(QWebFrameH handle, Qt::Orientation orientation)
{
	return (int) ((QWebFrame *)handle)->scrollBarMinimum(orientation);
}

int QWebFrame_scrollBarMaximum(QWebFrameH handle, Qt::Orientation orientation)
{
	return (int) ((QWebFrame *)handle)->scrollBarMaximum(orientation);
}

void QWebFrame_scrollBarGeometry(QWebFrameH handle, PRect retval, Qt::Orientation orientation)
{
	QRect t_retval;
	t_retval = ((QWebFrame *)handle)->scrollBarGeometry(orientation);
	copyQRectToPRect(t_retval, retval);
}

void QWebFrame_scroll(QWebFrameH handle, int AnonParam1, int AnonParam2)
{
	((QWebFrame *)handle)->scroll(AnonParam1, AnonParam2);
}

void QWebFrame_scrollPosition(QWebFrameH handle, PQtPoint retval)
{
	*(QPoint *)retval = ((QWebFrame *)handle)->scrollPosition();
}

void QWebFrame_setScrollPosition(QWebFrameH handle, const QPointH pos)
{
	((QWebFrame *)handle)->setScrollPosition(*(const QPoint*)pos);
}

void QWebFrame_scrollToAnchor(QWebFrameH handle, PWideString anchor)
{
	QString t_anchor;
	copyPWideStringToQString(anchor, t_anchor);
	((QWebFrame *)handle)->scrollToAnchor(t_anchor);
}

void QWebFrame_render(QWebFrameH handle, QPainterH AnonParam1, const QRegionH clip)
{
	((QWebFrame *)handle)->render((QPainter*)AnonParam1, *(const QRegion*)clip);
}

void QWebFrame_render2(QWebFrameH handle, QPainterH AnonParam1, unsigned int layer, const QRegionH clip)
{
	((QWebFrame *)handle)->render((QPainter*)AnonParam1, (QWebFrame::RenderLayers)layer, *(const QRegion*)clip);
}

void QWebFrame_setTextSizeMultiplier(QWebFrameH handle, qreal factor)
{
	((QWebFrame *)handle)->setTextSizeMultiplier(factor);
}

qreal QWebFrame_textSizeMultiplier(QWebFrameH handle)
{
	return (qreal) ((QWebFrame *)handle)->textSizeMultiplier();
}

qreal QWebFrame_zoomFactor(QWebFrameH handle)
{
	return (qreal) ((QWebFrame *)handle)->zoomFactor();
}

void QWebFrame_setZoomFactor(QWebFrameH handle, qreal factor)
{
	((QWebFrame *)handle)->setZoomFactor(factor);
}

bool QWebFrame_hasFocus(QWebFrameH handle)
{
	return (bool) ((QWebFrame *)handle)->hasFocus();
}

void QWebFrame_setFocus(QWebFrameH handle)
{
	((QWebFrame *)handle)->setFocus();
}

void QWebFrame_pos(QWebFrameH handle, PQtPoint retval)
{
	*(QPoint *)retval = ((QWebFrame *)handle)->pos();
}

void QWebFrame_geometry(QWebFrameH handle, PRect retval)
{
	QRect t_retval;
	t_retval = ((QWebFrame *)handle)->geometry();
	copyQRectToPRect(t_retval, retval);
}

void QWebFrame_contentsSize(QWebFrameH handle, PSize retval)
{
	*(QSize *)retval = ((QWebFrame *)handle)->contentsSize();
}

void QWebFrame_documentElement(QWebFrameH handle, QWebElementH retval)
{
	*(QWebElement *)retval = ((QWebFrame *)handle)->documentElement();
}

void QWebFrame_findAllElements(QWebFrameH handle, QWebElementCollectionH retval, PWideString selectorQuery)
{
	QString t_selectorQuery;
	copyPWideStringToQString(selectorQuery, t_selectorQuery);
	*(QWebElementCollection *)retval = ((QWebFrame *)handle)->findAllElements(t_selectorQuery);
}

void QWebFrame_findFirstElement(QWebFrameH handle, QWebElementH retval, PWideString selectorQuery)
{
	QString t_selectorQuery;
	copyPWideStringToQString(selectorQuery, t_selectorQuery);
	*(QWebElement *)retval = ((QWebFrame *)handle)->findFirstElement(t_selectorQuery);
}

void QWebFrame_hitTestContent(QWebFrameH handle, QWebHitTestResultH retval, const QPointH pos)
{
	*(QWebHitTestResult *)retval = ((QWebFrame *)handle)->hitTestContent(*(const QPoint*)pos);
}

bool QWebFrame_event(QWebFrameH handle, QEventH AnonParam1)
{
	return (bool) ((QWebFrame *)handle)->event((QEvent*)AnonParam1);
}

void QWebFrame_evaluateJavaScript(QWebFrameH handle, QVariantH retval, PWideString scriptSource)
{
	QString t_scriptSource;
	copyPWideStringToQString(scriptSource, t_scriptSource);
	*(QVariant *)retval = ((QWebFrame *)handle)->evaluateJavaScript(t_scriptSource);
}

void QWebFrame_print(QWebFrameH handle, QPrinterH printer)
{
	((QWebFrame *)handle)->print((QPrinter*)printer);
}

