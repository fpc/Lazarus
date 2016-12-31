//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qwebview_hook_c.h"

QWebView_hookH QWebView_hook_Create(QObjectH handle)
{
	return (QWebView_hookH) new QWebView_hook((QObject*)handle);
}

void QWebView_hook_Destroy(QWebView_hookH handle)
{
	delete (QWebView_hook *)handle;
}

void QWebView_hook_hook_loadStarted(QWebView_hookH handle, QHookH hook)
{
	((QWebView_hook *)handle)->hook_loadStarted(hook);
}

void QWebView_hook_hook_loadProgress(QWebView_hookH handle, QHookH hook)
{
	((QWebView_hook *)handle)->hook_loadProgress(hook);
}

void QWebView_hook_hook_loadFinished(QWebView_hookH handle, QHookH hook)
{
	((QWebView_hook *)handle)->hook_loadFinished(hook);
}

void QWebView_hook_hook_titleChanged(QWebView_hookH handle, QHookH hook)
{
	((QWebView_hook *)handle)->hook_titleChanged(hook);
}

void QWebView_hook_hook_statusBarMessage(QWebView_hookH handle, QHookH hook)
{
	((QWebView_hook *)handle)->hook_statusBarMessage(hook);
}

void QWebView_hook_hook_linkClicked(QWebView_hookH handle, QHookH hook)
{
	((QWebView_hook *)handle)->hook_linkClicked(hook);
}

void QWebView_hook_hook_selectionChanged(QWebView_hookH handle, QHookH hook)
{
	((QWebView_hook *)handle)->hook_selectionChanged(hook);
}

void QWebView_hook_hook_iconChanged(QWebView_hookH handle, QHookH hook)
{
	((QWebView_hook *)handle)->hook_iconChanged(hook);
}

void QWebView_hook_hook_urlChanged(QWebView_hookH handle, QHookH hook)
{
	((QWebView_hook *)handle)->hook_urlChanged(hook);
}

