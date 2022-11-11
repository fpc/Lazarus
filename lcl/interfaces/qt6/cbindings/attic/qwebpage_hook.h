//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QWEBPAGE_HOOK_H
#define QWEBPAGE_HOOK_H

#include <qwebpage.h>

#include "qobject_hook.h"

class QWebPage_hook : public QObject_hook {
  Q_OBJECT
  public:
    QWebPage_hook(QObject *handle) : QObject_hook(handle) {
      loadStarted_event.func = NULL;
      loadProgress_event.func = NULL;
      loadFinished_event.func = NULL;
      linkHovered_event.func = NULL;
      statusBarMessage_event.func = NULL;
      selectionChanged_event.func = NULL;
      frameCreated_event.func = NULL;
      geometryChangeRequested_event.func = NULL;
      repaintRequested_event.func = NULL;
      scrollRequested_event.func = NULL;
      windowCloseRequested_event.func = NULL;
      printRequested_event.func = NULL;
      linkClicked_event.func = NULL;
      toolBarVisibilityChangeRequested_event.func = NULL;
      statusBarVisibilityChangeRequested_event.func = NULL;
      menuBarVisibilityChangeRequested_event.func = NULL;
      unsupportedContent_event.func = NULL;
      downloadRequested_event.func = NULL;
      microFocusChanged_event.func = NULL;
      contentsChanged_event.func = NULL;
      databaseQuotaExceeded_event.func = NULL;
      applicationCacheQuotaExceeded_event.func = NULL;
      saveFrameStateRequested_event.func = NULL;
      restoreFrameStateRequested_event.func = NULL;
      viewportChangeRequested_event.func = NULL;
      featurePermissionRequested_event.func = NULL;
      featurePermissionRequestCanceled_event.func = NULL;
    }
    void hook_loadStarted(QHook &hook) { 
      if ( !loadStarted_event.func )
        connect(handle, SIGNAL(loadStarted()), this, SLOT(loadStarted_hook()));
      loadStarted_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(loadStarted()), this, SLOT(loadStarted_hook()));
    }
    void hook_loadProgress(QHook &hook) { 
      if ( !loadProgress_event.func )
        connect(handle, SIGNAL(loadProgress(int)), this, SLOT(loadProgress_hook(int)));
      loadProgress_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(loadProgress(int)), this, SLOT(loadProgress_hook(int)));
    }
    void hook_loadFinished(QHook &hook) { 
      if ( !loadFinished_event.func )
        connect(handle, SIGNAL(loadFinished(bool)), this, SLOT(loadFinished_hook(bool)));
      loadFinished_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(loadFinished(bool)), this, SLOT(loadFinished_hook(bool)));
    }
    void hook_linkHovered(QHook &hook) { 
      if ( !linkHovered_event.func )
        connect(handle, SIGNAL(linkHovered(const QString&, const QString&, const QString&)), this, SLOT(linkHovered_hook(const QString&, const QString&, const QString&)));
      linkHovered_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(linkHovered(const QString&, const QString&, const QString&)), this, SLOT(linkHovered_hook(const QString&, const QString&, const QString&)));
    }
    void hook_statusBarMessage(QHook &hook) { 
      if ( !statusBarMessage_event.func )
        connect(handle, SIGNAL(statusBarMessage(const QString&)), this, SLOT(statusBarMessage_hook(const QString&)));
      statusBarMessage_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(statusBarMessage(const QString&)), this, SLOT(statusBarMessage_hook(const QString&)));
    }
    void hook_selectionChanged(QHook &hook) { 
      if ( !selectionChanged_event.func )
        connect(handle, SIGNAL(selectionChanged()), this, SLOT(selectionChanged_hook()));
      selectionChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(selectionChanged()), this, SLOT(selectionChanged_hook()));
    }
    void hook_frameCreated(QHook &hook) { 
      if ( !frameCreated_event.func )
        connect(handle, SIGNAL(frameCreated(QWebFrame*)), this, SLOT(frameCreated_hook(QWebFrame*)));
      frameCreated_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(frameCreated(QWebFrame*)), this, SLOT(frameCreated_hook(QWebFrame*)));
    }
    void hook_geometryChangeRequested(QHook &hook) { 
      if ( !geometryChangeRequested_event.func )
        connect(handle, SIGNAL(geometryChangeRequested(const QRect&)), this, SLOT(geometryChangeRequested_hook(const QRect&)));
      geometryChangeRequested_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(geometryChangeRequested(const QRect&)), this, SLOT(geometryChangeRequested_hook(const QRect&)));
    }
    void hook_repaintRequested(QHook &hook) { 
      if ( !repaintRequested_event.func )
        connect(handle, SIGNAL(repaintRequested(const QRect&)), this, SLOT(repaintRequested_hook(const QRect&)));
      repaintRequested_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(repaintRequested(const QRect&)), this, SLOT(repaintRequested_hook(const QRect&)));
    }
    void hook_scrollRequested(QHook &hook) { 
      if ( !scrollRequested_event.func )
        connect(handle, SIGNAL(scrollRequested(int, int, const QRect&)), this, SLOT(scrollRequested_hook(int, int, const QRect&)));
      scrollRequested_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(scrollRequested(int, int, const QRect&)), this, SLOT(scrollRequested_hook(int, int, const QRect&)));
    }
    void hook_windowCloseRequested(QHook &hook) { 
      if ( !windowCloseRequested_event.func )
        connect(handle, SIGNAL(windowCloseRequested()), this, SLOT(windowCloseRequested_hook()));
      windowCloseRequested_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(windowCloseRequested()), this, SLOT(windowCloseRequested_hook()));
    }
    void hook_printRequested(QHook &hook) { 
      if ( !printRequested_event.func )
        connect(handle, SIGNAL(printRequested(QWebFrame*)), this, SLOT(printRequested_hook(QWebFrame*)));
      printRequested_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(printRequested(QWebFrame*)), this, SLOT(printRequested_hook(QWebFrame*)));
    }
    void hook_linkClicked(QHook &hook) { 
      if ( !linkClicked_event.func )
        connect(handle, SIGNAL(linkClicked(const QUrl&)), this, SLOT(linkClicked_hook(const QUrl&)));
      linkClicked_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(linkClicked(const QUrl&)), this, SLOT(linkClicked_hook(const QUrl&)));
    }
    void hook_toolBarVisibilityChangeRequested(QHook &hook) { 
      if ( !toolBarVisibilityChangeRequested_event.func )
        connect(handle, SIGNAL(toolBarVisibilityChangeRequested(bool)), this, SLOT(toolBarVisibilityChangeRequested_hook(bool)));
      toolBarVisibilityChangeRequested_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(toolBarVisibilityChangeRequested(bool)), this, SLOT(toolBarVisibilityChangeRequested_hook(bool)));
    }
    void hook_statusBarVisibilityChangeRequested(QHook &hook) { 
      if ( !statusBarVisibilityChangeRequested_event.func )
        connect(handle, SIGNAL(statusBarVisibilityChangeRequested(bool)), this, SLOT(statusBarVisibilityChangeRequested_hook(bool)));
      statusBarVisibilityChangeRequested_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(statusBarVisibilityChangeRequested(bool)), this, SLOT(statusBarVisibilityChangeRequested_hook(bool)));
    }
    void hook_menuBarVisibilityChangeRequested(QHook &hook) { 
      if ( !menuBarVisibilityChangeRequested_event.func )
        connect(handle, SIGNAL(menuBarVisibilityChangeRequested(bool)), this, SLOT(menuBarVisibilityChangeRequested_hook(bool)));
      menuBarVisibilityChangeRequested_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(menuBarVisibilityChangeRequested(bool)), this, SLOT(menuBarVisibilityChangeRequested_hook(bool)));
    }
    void hook_unsupportedContent(QHook &hook) { 
      if ( !unsupportedContent_event.func )
        connect(handle, SIGNAL(unsupportedContent(QNetworkReply*)), this, SLOT(unsupportedContent_hook(QNetworkReply*)));
      unsupportedContent_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(unsupportedContent(QNetworkReply*)), this, SLOT(unsupportedContent_hook(QNetworkReply*)));
    }
    void hook_downloadRequested(QHook &hook) { 
      if ( !downloadRequested_event.func )
        connect(handle, SIGNAL(downloadRequested(const QNetworkRequest&)), this, SLOT(downloadRequested_hook(const QNetworkRequest&)));
      downloadRequested_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(downloadRequested(const QNetworkRequest&)), this, SLOT(downloadRequested_hook(const QNetworkRequest&)));
    }
    void hook_microFocusChanged(QHook &hook) { 
      if ( !microFocusChanged_event.func )
        connect(handle, SIGNAL(microFocusChanged()), this, SLOT(microFocusChanged_hook()));
      microFocusChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(microFocusChanged()), this, SLOT(microFocusChanged_hook()));
    }
    void hook_contentsChanged(QHook &hook) { 
      if ( !contentsChanged_event.func )
        connect(handle, SIGNAL(contentsChanged()), this, SLOT(contentsChanged_hook()));
      contentsChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(contentsChanged()), this, SLOT(contentsChanged_hook()));
    }
    void hook_databaseQuotaExceeded(QHook &hook) { 
      if ( !databaseQuotaExceeded_event.func )
        connect(handle, SIGNAL(databaseQuotaExceeded(QWebFrame*, QString)), this, SLOT(databaseQuotaExceeded_hook(QWebFrame*, QString)));
      databaseQuotaExceeded_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(databaseQuotaExceeded(QWebFrame*, QString)), this, SLOT(databaseQuotaExceeded_hook(QWebFrame*, QString)));
    }
    void hook_applicationCacheQuotaExceeded(QHook &hook) { 
      if ( !applicationCacheQuotaExceeded_event.func )
        connect(handle, SIGNAL(applicationCacheQuotaExceeded(QWebSecurityOrigin*, quint64, quint64)), this, SLOT(applicationCacheQuotaExceeded_hook(QWebSecurityOrigin*, quint64, quint64)));
      applicationCacheQuotaExceeded_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(applicationCacheQuotaExceeded(QWebSecurityOrigin*, quint64, quint64)), this, SLOT(applicationCacheQuotaExceeded_hook(QWebSecurityOrigin*, quint64, quint64)));
    }
    void hook_saveFrameStateRequested(QHook &hook) { 
      if ( !saveFrameStateRequested_event.func )
        connect(handle, SIGNAL(saveFrameStateRequested(QWebFrame*, QWebHistoryItem*)), this, SLOT(saveFrameStateRequested_hook(QWebFrame*, QWebHistoryItem*)));
      saveFrameStateRequested_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(saveFrameStateRequested(QWebFrame*, QWebHistoryItem*)), this, SLOT(saveFrameStateRequested_hook(QWebFrame*, QWebHistoryItem*)));
    }
    void hook_restoreFrameStateRequested(QHook &hook) { 
      if ( !restoreFrameStateRequested_event.func )
        connect(handle, SIGNAL(restoreFrameStateRequested(QWebFrame*)), this, SLOT(restoreFrameStateRequested_hook(QWebFrame*)));
      restoreFrameStateRequested_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(restoreFrameStateRequested(QWebFrame*)), this, SLOT(restoreFrameStateRequested_hook(QWebFrame*)));
    }
    void hook_viewportChangeRequested(QHook &hook) { 
      if ( !viewportChangeRequested_event.func )
        connect(handle, SIGNAL(viewportChangeRequested()), this, SLOT(viewportChangeRequested_hook()));
      viewportChangeRequested_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(viewportChangeRequested()), this, SLOT(viewportChangeRequested_hook()));
    }
    void hook_featurePermissionRequested(QHook &hook) { 
      if ( !featurePermissionRequested_event.func )
        connect(handle, SIGNAL(featurePermissionRequested(QWebFrame*, QWebPage::Feature)), this, SLOT(featurePermissionRequested_hook(QWebFrame*, QWebPage::Feature)));
      featurePermissionRequested_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(featurePermissionRequested(QWebFrame*, QWebPage::Feature)), this, SLOT(featurePermissionRequested_hook(QWebFrame*, QWebPage::Feature)));
    }
    void hook_featurePermissionRequestCanceled(QHook &hook) { 
      if ( !featurePermissionRequestCanceled_event.func )
        connect(handle, SIGNAL(featurePermissionRequestCanceled(QWebFrame*, QWebPage::Feature)), this, SLOT(featurePermissionRequestCanceled_hook(QWebFrame*, QWebPage::Feature)));
      featurePermissionRequestCanceled_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(featurePermissionRequestCanceled(QWebFrame*, QWebPage::Feature)), this, SLOT(featurePermissionRequestCanceled_hook(QWebFrame*, QWebPage::Feature)));
    }

  private slots:
    void loadStarted_hook() {
      if ( loadStarted_event.func ) {
        typedef void (*func_type)(void *data);
	(*(func_type)loadStarted_event.func)(loadStarted_event.data);
      }
    }
    void loadProgress_hook(int progress) {
      if ( loadProgress_event.func ) {
        typedef void (*func_type)(void *data, int progress);
	(*(func_type)loadProgress_event.func)(loadProgress_event.data, progress);
      }
    }
    void loadFinished_hook(bool ok) {
      if ( loadFinished_event.func ) {
        typedef void (*func_type)(void *data, bool ok);
	(*(func_type)loadFinished_event.func)(loadFinished_event.data, ok);
      }
    }
    void linkHovered_hook(const QString& link, const QString& title, const QString& textContent) {
      if ( linkHovered_event.func ) {
        typedef void (*func_type)(void *data, PWideString link, PWideString title, PWideString textContent);
	PWideString t_link;
	PWideString t_title;
	PWideString t_textContent;
	initializePWideString(t_link);
	initializePWideString(t_title);
	initializePWideString(t_textContent);
	copyQStringToPWideString(link, t_link);
	copyQStringToPWideString(title, t_title);
	copyQStringToPWideString(textContent, t_textContent);
	(*(func_type)linkHovered_event.func)(linkHovered_event.data, t_link, t_title, t_textContent);
	finalizePWideString(t_link);
	finalizePWideString(t_title);
	finalizePWideString(t_textContent);
      }
    }
    void statusBarMessage_hook(const QString& text) {
      if ( statusBarMessage_event.func ) {
        typedef void (*func_type)(void *data, PWideString text);
	PWideString t_text;
	initializePWideString(t_text);
	copyQStringToPWideString(text, t_text);
	(*(func_type)statusBarMessage_event.func)(statusBarMessage_event.data, t_text);
	finalizePWideString(t_text);
      }
    }
    void selectionChanged_hook() {
      if ( selectionChanged_event.func ) {
        typedef void (*func_type)(void *data);
	(*(func_type)selectionChanged_event.func)(selectionChanged_event.data);
      }
    }
    void frameCreated_hook(QWebFrame* frame) {
      if ( frameCreated_event.func ) {
        typedef void (*func_type)(void *data, QWebFrameH frame);
	(*(func_type)frameCreated_event.func)(frameCreated_event.data, (QWebFrameH)frame);
      }
    }
    void geometryChangeRequested_hook(const QRect& geom) {
      if ( geometryChangeRequested_event.func ) {
        typedef void (*func_type)(void *data, PRect geom);
	PRect t_geom;
	copyQRectToPRect(geom, t_geom);
	(*(func_type)geometryChangeRequested_event.func)(geometryChangeRequested_event.data, t_geom);
      }
    }
    void repaintRequested_hook(const QRect& dirtyRect) {
      if ( repaintRequested_event.func ) {
        typedef void (*func_type)(void *data, PRect dirtyRect);
	PRect t_dirtyRect;
	copyQRectToPRect(dirtyRect, t_dirtyRect);
	(*(func_type)repaintRequested_event.func)(repaintRequested_event.data, t_dirtyRect);
      }
    }
    void scrollRequested_hook(int dx, int dy, const QRect& scrollViewRect) {
      if ( scrollRequested_event.func ) {
        typedef void (*func_type)(void *data, int dx, int dy, PRect scrollViewRect);
	PRect t_scrollViewRect;
	copyQRectToPRect(scrollViewRect, t_scrollViewRect);
	(*(func_type)scrollRequested_event.func)(scrollRequested_event.data, dx, dy, t_scrollViewRect);
      }
    }
    void windowCloseRequested_hook() {
      if ( windowCloseRequested_event.func ) {
        typedef void (*func_type)(void *data);
	(*(func_type)windowCloseRequested_event.func)(windowCloseRequested_event.data);
      }
    }
    void printRequested_hook(QWebFrame* frame) {
      if ( printRequested_event.func ) {
        typedef void (*func_type)(void *data, QWebFrameH frame);
	(*(func_type)printRequested_event.func)(printRequested_event.data, (QWebFrameH)frame);
      }
    }
    void linkClicked_hook(const QUrl& url) {
      if ( linkClicked_event.func ) {
        typedef void (*func_type)(void *data, const QUrlH url);
	(*(func_type)linkClicked_event.func)(linkClicked_event.data, (const QUrlH)&url);
      }
    }
    void toolBarVisibilityChangeRequested_hook(bool visible) {
      if ( toolBarVisibilityChangeRequested_event.func ) {
        typedef void (*func_type)(void *data, bool visible);
	(*(func_type)toolBarVisibilityChangeRequested_event.func)(toolBarVisibilityChangeRequested_event.data, visible);
      }
    }
    void statusBarVisibilityChangeRequested_hook(bool visible) {
      if ( statusBarVisibilityChangeRequested_event.func ) {
        typedef void (*func_type)(void *data, bool visible);
	(*(func_type)statusBarVisibilityChangeRequested_event.func)(statusBarVisibilityChangeRequested_event.data, visible);
      }
    }
    void menuBarVisibilityChangeRequested_hook(bool visible) {
      if ( menuBarVisibilityChangeRequested_event.func ) {
        typedef void (*func_type)(void *data, bool visible);
	(*(func_type)menuBarVisibilityChangeRequested_event.func)(menuBarVisibilityChangeRequested_event.data, visible);
      }
    }
    void unsupportedContent_hook(QNetworkReply* reply) {
      if ( unsupportedContent_event.func ) {
        typedef void (*func_type)(void *data, QNetworkReplyH reply);
	(*(func_type)unsupportedContent_event.func)(unsupportedContent_event.data, (QNetworkReplyH)reply);
      }
    }
    void downloadRequested_hook(const QNetworkRequest& request) {
      if ( downloadRequested_event.func ) {
        typedef void (*func_type)(void *data, const QNetworkRequestH request);
	(*(func_type)downloadRequested_event.func)(downloadRequested_event.data, (const QNetworkRequestH)&request);
      }
    }
    void microFocusChanged_hook() {
      if ( microFocusChanged_event.func ) {
        typedef void (*func_type)(void *data);
	(*(func_type)microFocusChanged_event.func)(microFocusChanged_event.data);
      }
    }
    void contentsChanged_hook() {
      if ( contentsChanged_event.func ) {
        typedef void (*func_type)(void *data);
	(*(func_type)contentsChanged_event.func)(contentsChanged_event.data);
      }
    }
    void databaseQuotaExceeded_hook(QWebFrame* frame, QString databaseName) {
      if ( databaseQuotaExceeded_event.func ) {
        typedef void (*func_type)(void *data, QWebFrameH frame, PWideString databaseName);
	PWideString t_databaseName;
	initializePWideString(t_databaseName);
	copyQStringToPWideString(databaseName, t_databaseName);
	(*(func_type)databaseQuotaExceeded_event.func)(databaseQuotaExceeded_event.data, (QWebFrameH)frame, t_databaseName);
	finalizePWideString(t_databaseName);
      }
    }
    void applicationCacheQuotaExceeded_hook(QWebSecurityOrigin* origin, quint64 defaultOriginQuota, quint64 totalSpaceNeeded) {
      if ( applicationCacheQuotaExceeded_event.func ) {
        typedef void (*func_type)(void *data, QWebSecurityOriginH origin, quint64 defaultOriginQuota, quint64 totalSpaceNeeded);
	(*(func_type)applicationCacheQuotaExceeded_event.func)(applicationCacheQuotaExceeded_event.data, (QWebSecurityOriginH)origin, defaultOriginQuota, totalSpaceNeeded);
      }
    }
    void saveFrameStateRequested_hook(QWebFrame* frame, QWebHistoryItem* item) {
      if ( saveFrameStateRequested_event.func ) {
        typedef void (*func_type)(void *data, QWebFrameH frame, QWebHistoryItemH item);
	(*(func_type)saveFrameStateRequested_event.func)(saveFrameStateRequested_event.data, (QWebFrameH)frame, (QWebHistoryItemH)item);
      }
    }
    void restoreFrameStateRequested_hook(QWebFrame* frame) {
      if ( restoreFrameStateRequested_event.func ) {
        typedef void (*func_type)(void *data, QWebFrameH frame);
	(*(func_type)restoreFrameStateRequested_event.func)(restoreFrameStateRequested_event.data, (QWebFrameH)frame);
      }
    }
    void viewportChangeRequested_hook() {
      if ( viewportChangeRequested_event.func ) {
        typedef void (*func_type)(void *data);
	(*(func_type)viewportChangeRequested_event.func)(viewportChangeRequested_event.data);
      }
    }
    void featurePermissionRequested_hook(QWebFrame* frame, QWebPage::Feature feature) {
      if ( featurePermissionRequested_event.func ) {
        typedef void (*func_type)(void *data, QWebFrameH frame, QWebPage::Feature feature);
	(*(func_type)featurePermissionRequested_event.func)(featurePermissionRequested_event.data, (QWebFrameH)frame, feature);
      }
    }
    void featurePermissionRequestCanceled_hook(QWebFrame* frame, QWebPage::Feature feature) {
      if ( featurePermissionRequestCanceled_event.func ) {
        typedef void (*func_type)(void *data, QWebFrameH frame, QWebPage::Feature feature);
	(*(func_type)featurePermissionRequestCanceled_event.func)(featurePermissionRequestCanceled_event.data, (QWebFrameH)frame, feature);
      }
    }
  private:
    QHook loadStarted_event;
    QHook loadProgress_event;
    QHook loadFinished_event;
    QHook linkHovered_event;
    QHook statusBarMessage_event;
    QHook selectionChanged_event;
    QHook frameCreated_event;
    QHook geometryChangeRequested_event;
    QHook repaintRequested_event;
    QHook scrollRequested_event;
    QHook windowCloseRequested_event;
    QHook printRequested_event;
    QHook linkClicked_event;
    QHook toolBarVisibilityChangeRequested_event;
    QHook statusBarVisibilityChangeRequested_event;
    QHook menuBarVisibilityChangeRequested_event;
    QHook unsupportedContent_event;
    QHook downloadRequested_event;
    QHook microFocusChanged_event;
    QHook contentsChanged_event;
    QHook databaseQuotaExceeded_event;
    QHook applicationCacheQuotaExceeded_event;
    QHook saveFrameStateRequested_event;
    QHook restoreFrameStateRequested_event;
    QHook viewportChangeRequested_event;
    QHook featurePermissionRequested_event;
    QHook featurePermissionRequestCanceled_event;
};


#endif
