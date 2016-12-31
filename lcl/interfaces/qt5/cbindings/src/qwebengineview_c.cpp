//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qwebengineview_c.h"

QWebEngineViewH QWebEngineView_Create(QWidgetH parent)
{
	return (QWebEngineViewH) new QWebEngineView((QWidget*)parent);
}

void QWebEngineView_Destroy(QWebEngineViewH handle)
{
	delete (QWebEngineView *)handle;
}

QWebEnginePageH QWebEngineView_page(QWebEngineViewH handle)
{
	return (QWebEnginePageH) ((QWebEngineView *)handle)->page();
}

void QWebEngineView_setPage(QWebEngineViewH handle, QWebEnginePageH page)
{
	((QWebEngineView *)handle)->setPage((QWebEnginePage*)page);
}

void QWebEngineView_load(QWebEngineViewH handle, const QUrlH url)
{
	((QWebEngineView *)handle)->load(*(const QUrl*)url);
}

/* Old webkit */
/* void QWebEngineView_load2(QWebEngineViewH handle, const QNetworkRequestH request, QNetworkAccessManager::Operation operation, const QByteArrayH body)
{
	((QWebEngineView *)handle)->load(*(const QNetworkRequest*)request, operation, *(const QByteArray*)body);
}*/

void QWebEngineView_setHtml(QWebEngineViewH handle, PWideString html, const QUrlH baseUrl)
{
	QString t_html;
	copyPWideStringToQString(html, t_html);
	((QWebEngineView *)handle)->setHtml(t_html, *(const QUrl*)baseUrl);
}

void QWebEngineView_setContent(QWebEngineViewH handle, const QByteArrayH data, PWideString mimeType, const QUrlH baseUrl)
{
	QString t_mimeType;
	copyPWideStringToQString(mimeType, t_mimeType);
	((QWebEngineView *)handle)->setContent(*(const QByteArray*)data, t_mimeType, *(const QUrl*)baseUrl);
}

QWebEngineHistoryH QWebEngineView_history(QWebEngineViewH handle)
{
	return (QWebEngineHistoryH) ((QWebEngineView *)handle)->history();
}

QWebEngineSettingsH QWebEngineView_settings(QWebEngineViewH handle)
{
	return (QWebEngineSettingsH) ((QWebEngineView *)handle)->settings();
}

void QWebEngineView_title(QWebEngineViewH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QWebEngineView *)handle)->title();
	copyQStringToPWideString(t_retval, retval);
}

void QWebEngineView_setUrl(QWebEngineViewH handle, const QUrlH url)
{
	((QWebEngineView *)handle)->setUrl(*(const QUrl*)url);
}

void QWebEngineView_url(QWebEngineViewH handle, QUrlH retval)
{
	*(QUrl *)retval = ((QWebEngineView *)handle)->url();
}

void QWebEngineView_icon(QWebEngineViewH handle, QIconH retval)
{
	*(QIcon *)retval = ((QWebEngineView *)handle)->icon();
}

bool QWebEngineView_hasSelection(QWebEngineViewH handle)
{
	return (bool) ((QWebEngineView *)handle)->hasSelection();
}

void QWebEngineView_selectedText(QWebEngineViewH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QWebEngineView *)handle)->selectedText();
	copyQStringToPWideString(t_retval, retval);
}

/* webkit
void QWebEngineView_selectedHtml(QWebEngineViewH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QWebEngineView *)handle)->selectedHtml();
	copyQStringToPWideString(t_retval, retval);
}*/

QActionH QWebEngineView_pageAction(QWebEngineViewH handle, QWebEnginePage::WebAction action)
{
	return (QActionH) ((QWebEngineView *)handle)->pageAction(action);
}

void QWebEngineView_triggerPageAction(QWebEngineViewH handle, QWebEnginePage::WebAction action, bool checked)
{
	((QWebEngineView *)handle)->triggerPageAction(action, checked);
}

/* webkit
bool QWebEngineView_isModified(QWebEngineViewH handle)
{
	return (bool) ((QWebEngineView *)handle)->isModified();
}


void QWebEngineView_inputMethodQuery(QWebEngineViewH handle, QVariantH retval, Qt::InputMethodQuery property)
{
	*(QVariant *)retval = ((QWebEngineView *)handle)->inputMethodQuery(property);
}
*/

void QWebEngineView_sizeHint(QWebEngineViewH handle, PSize retval)
{
	*(QSize *)retval = ((QWebEngineView *)handle)->sizeHint();
}

qreal QWebEngineView_zoomFactor(QWebEngineViewH handle)
{
	return (qreal) ((QWebEngineView *)handle)->zoomFactor();
}

void QWebEngineView_setZoomFactor(QWebEngineViewH handle, qreal factor)
{
	((QWebEngineView *)handle)->setZoomFactor(factor);
}

/* webkit
void QWebEngineView_setTextSizeMultiplier(QWebEngineViewH handle, qreal factor)
{
	((QWebEngineView *)handle)->setTextSizeMultiplier(factor);
}

qreal QWebEngineView_textSizeMultiplier(QWebEngineViewH handle)
{
	return (qreal) ((QWebEngineView *)handle)->textSizeMultiplier();
}


unsigned int QWebEngineView_renderHints(QWebEngineViewH handle)
{
	return (unsigned int) ((QWebEngineView *)handle)->renderHints();
}

void QWebEngineView_setRenderHints(QWebEngineViewH handle, unsigned int hints)
{
	((QWebEngineView *)handle)->setRenderHints((QPainter::RenderHints)hints);
}

void QWebEngineView_setRenderHint(QWebEngineViewH handle, QPainter::RenderHint hint, bool enabled)
{
	((QWebEngineView *)handle)->setRenderHint(hint, enabled);
}
*/

bool QWebEngineView_findText(QWebEngineViewH handle, PWideString subString, unsigned int options)
{
	QString t_subString;
	copyPWideStringToQString(subString, t_subString);
	return (bool) ((QWebEngineView *)handle)->findText(t_subString, (QWebEnginePage::FindFlags)options);
}

bool QWebEngineView_event(QWebEngineViewH handle, QEventH AnonParam1)
{
	return (bool) ((QWebEngineView *)handle)->event((QEvent*)AnonParam1);
}

void QWebEngineView_stop(QWebEngineViewH handle)
{
	((QWebEngineView *)handle)->stop();
}

void QWebEngineView_back(QWebEngineViewH handle)
{
	((QWebEngineView *)handle)->back();
}

void QWebEngineView_forward(QWebEngineViewH handle)
{
	((QWebEngineView *)handle)->forward();
}

void QWebEngineView_reload(QWebEngineViewH handle)
{
	((QWebEngineView *)handle)->reload();
}

/* webkit
void QWebEngineView_print(QWebEngineViewH handle, QPrinterH AnonParam1)
{
	((QWebEngineView *)handle)->print((QPrinter*)AnonParam1);
}
*/
