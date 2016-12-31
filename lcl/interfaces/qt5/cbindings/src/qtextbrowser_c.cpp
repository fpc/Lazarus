//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qtextbrowser_c.h"

QTextBrowserH QTextBrowser_Create(QWidgetH parent)
{
	return (QTextBrowserH) new QTextBrowser((QWidget*)parent);
}

void QTextBrowser_Destroy(QTextBrowserH handle)
{
	delete (QTextBrowser *)handle;
}

void QTextBrowser_source(QTextBrowserH handle, QUrlH retval)
{
	*(QUrl *)retval = ((QTextBrowser *)handle)->source();
}

void QTextBrowser_searchPaths(QTextBrowserH handle, QStringListH retval)
{
	*(QStringList *)retval = ((QTextBrowser *)handle)->searchPaths();
}

void QTextBrowser_setSearchPaths(QTextBrowserH handle, const QStringListH paths)
{
	((QTextBrowser *)handle)->setSearchPaths(*(const QStringList*)paths);
}

void QTextBrowser_loadResource(QTextBrowserH handle, QVariantH retval, int type, const QUrlH name)
{
	*(QVariant *)retval = ((QTextBrowser *)handle)->loadResource(type, *(const QUrl*)name);
}

bool QTextBrowser_isBackwardAvailable(QTextBrowserH handle)
{
	return (bool) ((QTextBrowser *)handle)->isBackwardAvailable();
}

bool QTextBrowser_isForwardAvailable(QTextBrowserH handle)
{
	return (bool) ((QTextBrowser *)handle)->isForwardAvailable();
}

void QTextBrowser_clearHistory(QTextBrowserH handle)
{
	((QTextBrowser *)handle)->clearHistory();
}

void QTextBrowser_historyTitle(QTextBrowserH handle, PWideString retval, int AnonParam1)
{
	QString t_retval;
	t_retval = ((QTextBrowser *)handle)->historyTitle(AnonParam1);
	copyQStringToPWideString(t_retval, retval);
}

void QTextBrowser_historyUrl(QTextBrowserH handle, QUrlH retval, int AnonParam1)
{
	*(QUrl *)retval = ((QTextBrowser *)handle)->historyUrl(AnonParam1);
}

int QTextBrowser_backwardHistoryCount(QTextBrowserH handle)
{
	return (int) ((QTextBrowser *)handle)->backwardHistoryCount();
}

int QTextBrowser_forwardHistoryCount(QTextBrowserH handle)
{
	return (int) ((QTextBrowser *)handle)->forwardHistoryCount();
}

bool QTextBrowser_openExternalLinks(QTextBrowserH handle)
{
	return (bool) ((QTextBrowser *)handle)->openExternalLinks();
}

void QTextBrowser_setOpenExternalLinks(QTextBrowserH handle, bool open)
{
	((QTextBrowser *)handle)->setOpenExternalLinks(open);
}

bool QTextBrowser_openLinks(QTextBrowserH handle)
{
	return (bool) ((QTextBrowser *)handle)->openLinks();
}

void QTextBrowser_setOpenLinks(QTextBrowserH handle, bool open)
{
	((QTextBrowser *)handle)->setOpenLinks(open);
}

void QTextBrowser_setSource(QTextBrowserH handle, const QUrlH name)
{
	((QTextBrowser *)handle)->setSource(*(const QUrl*)name);
}

void QTextBrowser_backward(QTextBrowserH handle)
{
	((QTextBrowser *)handle)->backward();
}

void QTextBrowser_forward(QTextBrowserH handle)
{
	((QTextBrowser *)handle)->forward();
}

void QTextBrowser_home(QTextBrowserH handle)
{
	((QTextBrowser *)handle)->home();
}

void QTextBrowser_reload(QTextBrowserH handle)
{
	((QTextBrowser *)handle)->reload();
}

