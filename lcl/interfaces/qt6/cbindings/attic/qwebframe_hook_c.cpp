//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qwebframe_hook_c.h"

QWebHitTestResult_hookH QWebHitTestResult_hook_Create(QObjectH handle)
{
	return (QWebHitTestResult_hookH) new QWebHitTestResult_hook((QObject*)handle);
}

void QWebHitTestResult_hook_Destroy(QWebHitTestResult_hookH handle)
{
	delete (QWebHitTestResult_hook *)handle;
}

QWebFrame_hookH QWebFrame_hook_Create(QObjectH handle)
{
	return (QWebFrame_hookH) new QWebFrame_hook((QObject*)handle);
}

void QWebFrame_hook_Destroy(QWebFrame_hookH handle)
{
	delete (QWebFrame_hook *)handle;
}

void QWebFrame_hook_hook_javaScriptWindowObjectCleared(QWebFrame_hookH handle, QHookH hook)
{
	((QWebFrame_hook *)handle)->hook_javaScriptWindowObjectCleared(hook);
}

void QWebFrame_hook_hook_provisionalLoad(QWebFrame_hookH handle, QHookH hook)
{
	((QWebFrame_hook *)handle)->hook_provisionalLoad(hook);
}

void QWebFrame_hook_hook_titleChanged(QWebFrame_hookH handle, QHookH hook)
{
	((QWebFrame_hook *)handle)->hook_titleChanged(hook);
}

void QWebFrame_hook_hook_urlChanged(QWebFrame_hookH handle, QHookH hook)
{
	((QWebFrame_hook *)handle)->hook_urlChanged(hook);
}

void QWebFrame_hook_hook_initialLayoutCompleted(QWebFrame_hookH handle, QHookH hook)
{
	((QWebFrame_hook *)handle)->hook_initialLayoutCompleted(hook);
}

void QWebFrame_hook_hook_iconChanged(QWebFrame_hookH handle, QHookH hook)
{
	((QWebFrame_hook *)handle)->hook_iconChanged(hook);
}

void QWebFrame_hook_hook_contentsSizeChanged(QWebFrame_hookH handle, QHookH hook)
{
	((QWebFrame_hook *)handle)->hook_contentsSizeChanged(hook);
}

void QWebFrame_hook_hook_loadStarted(QWebFrame_hookH handle, QHookH hook)
{
	((QWebFrame_hook *)handle)->hook_loadStarted(hook);
}

void QWebFrame_hook_hook_loadFinished(QWebFrame_hookH handle, QHookH hook)
{
	((QWebFrame_hook *)handle)->hook_loadFinished(hook);
}

void QWebFrame_hook_hook_pageChanged(QWebFrame_hookH handle, QHookH hook)
{
	((QWebFrame_hook *)handle)->hook_pageChanged(hook);
}

