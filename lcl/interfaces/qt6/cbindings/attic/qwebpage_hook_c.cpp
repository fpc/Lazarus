//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qwebpage_hook_c.h"

QWebPage_hookH QWebPage_hook_Create(QObjectH handle)
{
	return (QWebPage_hookH) new QWebPage_hook((QObject*)handle);
}

void QWebPage_hook_Destroy(QWebPage_hookH handle)
{
	delete (QWebPage_hook *)handle;
}

void QWebPage_hook_hook_loadStarted(QWebPage_hookH handle, QHookH hook)
{
	((QWebPage_hook *)handle)->hook_loadStarted(hook);
}

void QWebPage_hook_hook_loadProgress(QWebPage_hookH handle, QHookH hook)
{
	((QWebPage_hook *)handle)->hook_loadProgress(hook);
}

void QWebPage_hook_hook_loadFinished(QWebPage_hookH handle, QHookH hook)
{
	((QWebPage_hook *)handle)->hook_loadFinished(hook);
}

void QWebPage_hook_hook_linkHovered(QWebPage_hookH handle, QHookH hook)
{
	((QWebPage_hook *)handle)->hook_linkHovered(hook);
}

void QWebPage_hook_hook_statusBarMessage(QWebPage_hookH handle, QHookH hook)
{
	((QWebPage_hook *)handle)->hook_statusBarMessage(hook);
}

void QWebPage_hook_hook_selectionChanged(QWebPage_hookH handle, QHookH hook)
{
	((QWebPage_hook *)handle)->hook_selectionChanged(hook);
}

void QWebPage_hook_hook_frameCreated(QWebPage_hookH handle, QHookH hook)
{
	((QWebPage_hook *)handle)->hook_frameCreated(hook);
}

void QWebPage_hook_hook_geometryChangeRequested(QWebPage_hookH handle, QHookH hook)
{
	((QWebPage_hook *)handle)->hook_geometryChangeRequested(hook);
}

void QWebPage_hook_hook_repaintRequested(QWebPage_hookH handle, QHookH hook)
{
	((QWebPage_hook *)handle)->hook_repaintRequested(hook);
}

void QWebPage_hook_hook_scrollRequested(QWebPage_hookH handle, QHookH hook)
{
	((QWebPage_hook *)handle)->hook_scrollRequested(hook);
}

void QWebPage_hook_hook_windowCloseRequested(QWebPage_hookH handle, QHookH hook)
{
	((QWebPage_hook *)handle)->hook_windowCloseRequested(hook);
}

void QWebPage_hook_hook_printRequested(QWebPage_hookH handle, QHookH hook)
{
	((QWebPage_hook *)handle)->hook_printRequested(hook);
}

void QWebPage_hook_hook_linkClicked(QWebPage_hookH handle, QHookH hook)
{
	((QWebPage_hook *)handle)->hook_linkClicked(hook);
}

void QWebPage_hook_hook_toolBarVisibilityChangeRequested(QWebPage_hookH handle, QHookH hook)
{
	((QWebPage_hook *)handle)->hook_toolBarVisibilityChangeRequested(hook);
}

void QWebPage_hook_hook_statusBarVisibilityChangeRequested(QWebPage_hookH handle, QHookH hook)
{
	((QWebPage_hook *)handle)->hook_statusBarVisibilityChangeRequested(hook);
}

void QWebPage_hook_hook_menuBarVisibilityChangeRequested(QWebPage_hookH handle, QHookH hook)
{
	((QWebPage_hook *)handle)->hook_menuBarVisibilityChangeRequested(hook);
}

void QWebPage_hook_hook_unsupportedContent(QWebPage_hookH handle, QHookH hook)
{
	((QWebPage_hook *)handle)->hook_unsupportedContent(hook);
}

void QWebPage_hook_hook_downloadRequested(QWebPage_hookH handle, QHookH hook)
{
	((QWebPage_hook *)handle)->hook_downloadRequested(hook);
}

void QWebPage_hook_hook_microFocusChanged(QWebPage_hookH handle, QHookH hook)
{
	((QWebPage_hook *)handle)->hook_microFocusChanged(hook);
}

void QWebPage_hook_hook_contentsChanged(QWebPage_hookH handle, QHookH hook)
{
	((QWebPage_hook *)handle)->hook_contentsChanged(hook);
}

void QWebPage_hook_hook_databaseQuotaExceeded(QWebPage_hookH handle, QHookH hook)
{
	((QWebPage_hook *)handle)->hook_databaseQuotaExceeded(hook);
}

void QWebPage_hook_hook_applicationCacheQuotaExceeded(QWebPage_hookH handle, QHookH hook)
{
	((QWebPage_hook *)handle)->hook_applicationCacheQuotaExceeded(hook);
}

void QWebPage_hook_hook_saveFrameStateRequested(QWebPage_hookH handle, QHookH hook)
{
	((QWebPage_hook *)handle)->hook_saveFrameStateRequested(hook);
}

void QWebPage_hook_hook_restoreFrameStateRequested(QWebPage_hookH handle, QHookH hook)
{
	((QWebPage_hook *)handle)->hook_restoreFrameStateRequested(hook);
}

void QWebPage_hook_hook_viewportChangeRequested(QWebPage_hookH handle, QHookH hook)
{
	((QWebPage_hook *)handle)->hook_viewportChangeRequested(hook);
}

void QWebPage_hook_hook_featurePermissionRequested(QWebPage_hookH handle, QHookH hook)
{
	((QWebPage_hook *)handle)->hook_featurePermissionRequested(hook);
}

void QWebPage_hook_hook_featurePermissionRequestCanceled(QWebPage_hookH handle, QHookH hook)
{
	((QWebPage_hook *)handle)->hook_featurePermissionRequestCanceled(hook);
}

