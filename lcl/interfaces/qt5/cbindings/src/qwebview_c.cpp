//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qwebview_c.h"

QWebViewH QWebView_Create(QWidgetH parent)
{
	return (QWebViewH) new QWebView((QWidget*)parent);
}

void QWebView_Destroy(QWebViewH handle)
{
	delete (QWebView *)handle;
}

QWebPageH QWebView_page(QWebViewH handle)
{
	return (QWebPageH) ((QWebView *)handle)->page();
}

void QWebView_setPage(QWebViewH handle, QWebPageH page)
{
	((QWebView *)handle)->setPage((QWebPage*)page);
}

void QWebView_load(QWebViewH handle, const QUrlH url)
{
	((QWebView *)handle)->load(*(const QUrl*)url);
}

void QWebView_load2(QWebViewH handle, const QNetworkRequestH request, QNetworkAccessManager::Operation operation, const QByteArrayH body)
{
	((QWebView *)handle)->load(*(const QNetworkRequest*)request, operation, *(const QByteArray*)body);
}

void QWebView_setHtml(QWebViewH handle, PWideString html, const QUrlH baseUrl)
{
	QString t_html;
	copyPWideStringToQString(html, t_html);
	((QWebView *)handle)->setHtml(t_html, *(const QUrl*)baseUrl);
}

void QWebView_setContent(QWebViewH handle, const QByteArrayH data, PWideString mimeType, const QUrlH baseUrl)
{
	QString t_mimeType;
	copyPWideStringToQString(mimeType, t_mimeType);
	((QWebView *)handle)->setContent(*(const QByteArray*)data, t_mimeType, *(const QUrl*)baseUrl);
}

QWebHistoryH QWebView_history(QWebViewH handle)
{
	return (QWebHistoryH) ((QWebView *)handle)->history();
}

QWebSettingsH QWebView_settings(QWebViewH handle)
{
	return (QWebSettingsH) ((QWebView *)handle)->settings();
}

void QWebView_title(QWebViewH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QWebView *)handle)->title();
	copyQStringToPWideString(t_retval, retval);
}

void QWebView_setUrl(QWebViewH handle, const QUrlH url)
{
	((QWebView *)handle)->setUrl(*(const QUrl*)url);
}

void QWebView_url(QWebViewH handle, QUrlH retval)
{
	*(QUrl *)retval = ((QWebView *)handle)->url();
}

void QWebView_icon(QWebViewH handle, QIconH retval)
{
	*(QIcon *)retval = ((QWebView *)handle)->icon();
}

bool QWebView_hasSelection(QWebViewH handle)
{
	return (bool) ((QWebView *)handle)->hasSelection();
}

void QWebView_selectedText(QWebViewH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QWebView *)handle)->selectedText();
	copyQStringToPWideString(t_retval, retval);
}

void QWebView_selectedHtml(QWebViewH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QWebView *)handle)->selectedHtml();
	copyQStringToPWideString(t_retval, retval);
}

QActionH QWebView_pageAction(QWebViewH handle, QWebPage::WebAction action)
{
	return (QActionH) ((QWebView *)handle)->pageAction(action);
}

void QWebView_triggerPageAction(QWebViewH handle, QWebPage::WebAction action, bool checked)
{
	((QWebView *)handle)->triggerPageAction(action, checked);
}

bool QWebView_isModified(QWebViewH handle)
{
	return (bool) ((QWebView *)handle)->isModified();
}

void QWebView_inputMethodQuery(QWebViewH handle, QVariantH retval, Qt::InputMethodQuery property)
{
	*(QVariant *)retval = ((QWebView *)handle)->inputMethodQuery(property);
}

void QWebView_sizeHint(QWebViewH handle, PSize retval)
{
	*(QSize *)retval = ((QWebView *)handle)->sizeHint();
}

qreal QWebView_zoomFactor(QWebViewH handle)
{
	return (qreal) ((QWebView *)handle)->zoomFactor();
}

void QWebView_setZoomFactor(QWebViewH handle, qreal factor)
{
	((QWebView *)handle)->setZoomFactor(factor);
}

void QWebView_setTextSizeMultiplier(QWebViewH handle, qreal factor)
{
	((QWebView *)handle)->setTextSizeMultiplier(factor);
}

qreal QWebView_textSizeMultiplier(QWebViewH handle)
{
	return (qreal) ((QWebView *)handle)->textSizeMultiplier();
}

unsigned int QWebView_renderHints(QWebViewH handle)
{
	return (unsigned int) ((QWebView *)handle)->renderHints();
}

void QWebView_setRenderHints(QWebViewH handle, unsigned int hints)
{
	((QWebView *)handle)->setRenderHints((QPainter::RenderHints)hints);
}

void QWebView_setRenderHint(QWebViewH handle, QPainter::RenderHint hint, bool enabled)
{
	((QWebView *)handle)->setRenderHint(hint, enabled);
}

bool QWebView_findText(QWebViewH handle, PWideString subString, unsigned int options)
{
	QString t_subString;
	copyPWideStringToQString(subString, t_subString);
	return (bool) ((QWebView *)handle)->findText(t_subString, (QWebPage::FindFlags)options);
}

bool QWebView_event(QWebViewH handle, QEventH AnonParam1)
{
	return (bool) ((QWebView *)handle)->event((QEvent*)AnonParam1);
}

void QWebView_stop(QWebViewH handle)
{
	((QWebView *)handle)->stop();
}

void QWebView_back(QWebViewH handle)
{
	((QWebView *)handle)->back();
}

void QWebView_forward(QWebViewH handle)
{
	((QWebView *)handle)->forward();
}

void QWebView_reload(QWebViewH handle)
{
	((QWebView *)handle)->reload();
}

void QWebView_print(QWebViewH handle, QPrinterH AnonParam1)
{
	((QWebView *)handle)->print((QPrinter*)AnonParam1);
}

