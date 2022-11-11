//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QWEBENGINEPAGE_HOOK_C_H
#define QWEBENGINEPAGE_HOOK_C_H

#include "qwebenginepage_hook.h"

C_EXPORT QWebEnginePage_hookH QWebEnginePage_hook_Create(QObjectH handle);
C_EXPORT void QWebEnginePage_hook_Destroy(QWebEnginePage_hookH handle);
C_EXPORT void QWebEnginePage_hook_hook_loadStarted(QWebEnginePage_hookH handle, QHookH hook);
C_EXPORT void QWebEnginePage_hook_hook_loadProgress(QWebEnginePage_hookH handle, QHookH hook);
C_EXPORT void QWebEnginePage_hook_hook_loadFinished(QWebEnginePage_hookH handle, QHookH hook);
C_EXPORT void QWebEnginePage_hook_hook_linkHovered(QWebEnginePage_hookH handle, QHookH hook);
/*C_EXPORT void QWebEnginePage_hook_hook_statusBarMessage(QWebEnginePage_hookH handle, QHookH hook);*/
C_EXPORT void QWebEnginePage_hook_hook_selectionChanged(QWebEnginePage_hookH handle, QHookH hook);
/*C_EXPORT void QWebEnginePage_hook_hook_frameCreated(QWebEnginePage_hookH handle, QHookH hook);*/
C_EXPORT void QWebEnginePage_hook_hook_geometryChangeRequested(QWebEnginePage_hookH handle, QHookH hook);
/*C_EXPORT void QWebEnginePage_hook_hook_repaintRequested(QWebEnginePage_hookH handle, QHookH hook);
C_EXPORT void QWebEnginePage_hook_hook_scrollRequested(QWebEnginePage_hookH handle, QHookH hook);*/
C_EXPORT void QWebEnginePage_hook_hook_windowCloseRequested(QWebEnginePage_hookH handle, QHookH hook);
/*C_EXPORT void QWebEnginePage_hook_hook_printRequested(QWebEnginePage_hookH handle, QHookH hook);
C_EXPORT void QWebEnginePage_hook_hook_linkClicked(QWebEnginePage_hookH handle, QHookH hook);
C_EXPORT void QWebEnginePage_hook_hook_toolBarVisibilityChangeRequested(QWebEnginePage_hookH handle, QHookH hook);
C_EXPORT void QWebEnginePage_hook_hook_statusBarVisibilityChangeRequested(QWebEnginePage_hookH handle, QHookH hook);
C_EXPORT void QWebEnginePage_hook_hook_menuBarVisibilityChangeRequested(QWebEnginePage_hookH handle, QHookH hook);
C_EXPORT void QWebEnginePage_hook_hook_unsupportedContent(QWebEnginePage_hookH handle, QHookH hook);
C_EXPORT void QWebEnginePage_hook_hook_downloadRequested(QWebEnginePage_hookH handle, QHookH hook);
C_EXPORT void QWebEnginePage_hook_hook_microFocusChanged(QWebEnginePage_hookH handle, QHookH hook);*/
C_EXPORT void QWebEnginePage_hook_hook_contentsChanged(QWebEnginePage_hookH handle, QHookH hook);
/*C_EXPORT void QWebEnginePage_hook_hook_databaseQuotaExceeded(QWebEnginePage_hookH handle, QHookH hook);
C_EXPORT void QWebEnginePage_hook_hook_applicationCacheQuotaExceeded(QWebEnginePage_hookH handle, QHookH hook);
C_EXPORT void QWebEnginePage_hook_hook_saveFrameStateRequested(QWebEnginePage_hookH handle, QHookH hook);
C_EXPORT void QWebEnginePage_hook_hook_restoreFrameStateRequested(QWebEnginePage_hookH handle, QHookH hook);
C_EXPORT void QWebEnginePage_hook_hook_viewportChangeRequested(QWebEnginePage_hookH handle, QHookH hook);*/
C_EXPORT void QWebEnginePage_hook_hook_featurePermissionRequested(QWebEnginePage_hookH handle, QHookH hook);
C_EXPORT void QWebEnginePage_hook_hook_featurePermissionRequestCanceled(QWebEnginePage_hookH handle, QHookH hook);

#endif
