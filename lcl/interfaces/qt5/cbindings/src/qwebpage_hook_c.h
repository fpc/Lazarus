//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QWEBPAGE_HOOK_C_H
#define QWEBPAGE_HOOK_C_H

#include "qwebpage_hook.h"

C_EXPORT QWebPage_hookH QWebPage_hook_Create(QObjectH handle);
C_EXPORT void QWebPage_hook_Destroy(QWebPage_hookH handle);
C_EXPORT void QWebPage_hook_hook_loadStarted(QWebPage_hookH handle, QHookH hook);
C_EXPORT void QWebPage_hook_hook_loadProgress(QWebPage_hookH handle, QHookH hook);
C_EXPORT void QWebPage_hook_hook_loadFinished(QWebPage_hookH handle, QHookH hook);
C_EXPORT void QWebPage_hook_hook_linkHovered(QWebPage_hookH handle, QHookH hook);
C_EXPORT void QWebPage_hook_hook_statusBarMessage(QWebPage_hookH handle, QHookH hook);
C_EXPORT void QWebPage_hook_hook_selectionChanged(QWebPage_hookH handle, QHookH hook);
C_EXPORT void QWebPage_hook_hook_frameCreated(QWebPage_hookH handle, QHookH hook);
C_EXPORT void QWebPage_hook_hook_geometryChangeRequested(QWebPage_hookH handle, QHookH hook);
C_EXPORT void QWebPage_hook_hook_repaintRequested(QWebPage_hookH handle, QHookH hook);
C_EXPORT void QWebPage_hook_hook_scrollRequested(QWebPage_hookH handle, QHookH hook);
C_EXPORT void QWebPage_hook_hook_windowCloseRequested(QWebPage_hookH handle, QHookH hook);
C_EXPORT void QWebPage_hook_hook_printRequested(QWebPage_hookH handle, QHookH hook);
C_EXPORT void QWebPage_hook_hook_linkClicked(QWebPage_hookH handle, QHookH hook);
C_EXPORT void QWebPage_hook_hook_toolBarVisibilityChangeRequested(QWebPage_hookH handle, QHookH hook);
C_EXPORT void QWebPage_hook_hook_statusBarVisibilityChangeRequested(QWebPage_hookH handle, QHookH hook);
C_EXPORT void QWebPage_hook_hook_menuBarVisibilityChangeRequested(QWebPage_hookH handle, QHookH hook);
C_EXPORT void QWebPage_hook_hook_unsupportedContent(QWebPage_hookH handle, QHookH hook);
C_EXPORT void QWebPage_hook_hook_downloadRequested(QWebPage_hookH handle, QHookH hook);
C_EXPORT void QWebPage_hook_hook_microFocusChanged(QWebPage_hookH handle, QHookH hook);
C_EXPORT void QWebPage_hook_hook_contentsChanged(QWebPage_hookH handle, QHookH hook);
C_EXPORT void QWebPage_hook_hook_databaseQuotaExceeded(QWebPage_hookH handle, QHookH hook);
C_EXPORT void QWebPage_hook_hook_applicationCacheQuotaExceeded(QWebPage_hookH handle, QHookH hook);
C_EXPORT void QWebPage_hook_hook_saveFrameStateRequested(QWebPage_hookH handle, QHookH hook);
C_EXPORT void QWebPage_hook_hook_restoreFrameStateRequested(QWebPage_hookH handle, QHookH hook);
C_EXPORT void QWebPage_hook_hook_viewportChangeRequested(QWebPage_hookH handle, QHookH hook);
C_EXPORT void QWebPage_hook_hook_featurePermissionRequested(QWebPage_hookH handle, QHookH hook);
C_EXPORT void QWebPage_hook_hook_featurePermissionRequestCanceled(QWebPage_hookH handle, QHookH hook);

#endif
