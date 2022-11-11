//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qwebengineview_hook_c.h"

QWebEngineView_hookH QWebEngineView_hook_Create(QObjectH handle)
{
	return (QWebEngineView_hookH) new QWebEngineView_hook((QObject*)handle);
}

void QWebEngineView_hook_Destroy(QWebEngineView_hookH handle)
{
	delete (QWebEngineView_hook *)handle;
}

void QWebEngineView_hook_hook_loadStarted(QWebEngineView_hookH handle, QHookH hook)
{
	((QWebEngineView_hook *)handle)->hook_loadStarted(hook);
}

void QWebEngineView_hook_hook_loadProgress(QWebEngineView_hookH handle, QHookH hook)
{
	((QWebEngineView_hook *)handle)->hook_loadProgress(hook);
}

void QWebEngineView_hook_hook_loadFinished(QWebEngineView_hookH handle, QHookH hook)
{
	((QWebEngineView_hook *)handle)->hook_loadFinished(hook);
}

void QWebEngineView_hook_hook_titleChanged(QWebEngineView_hookH handle, QHookH hook)
{
	((QWebEngineView_hook *)handle)->hook_titleChanged(hook);
}

/* void QWebEngineView_hook_hook_statusBarMessage(QWebEngineView_hookH handle, QHookH hook)
{
	((QWebEngineView_hook *)handle)->hook_statusBarMessage(hook);
}*/

/* void QWebEngineView_hook_hook_linkClicked(QWebEngineView_hookH handle, QHookH hook)
{
	((QWebEngineView_hook *)handle)->hook_linkClicked(hook);
}*/

void QWebEngineView_hook_hook_selectionChanged(QWebEngineView_hookH handle, QHookH hook)
{
	((QWebEngineView_hook *)handle)->hook_selectionChanged(hook);
}

void QWebEngineView_hook_hook_iconChanged(QWebEngineView_hookH handle, QHookH hook)
{
	((QWebEngineView_hook *)handle)->hook_iconChanged(hook);
}

void QWebEngineView_hook_hook_urlChanged(QWebEngineView_hookH handle, QHookH hook)
{
	((QWebEngineView_hook *)handle)->hook_urlChanged(hook);
}

