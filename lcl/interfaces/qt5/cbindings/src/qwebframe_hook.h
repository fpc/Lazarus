//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QWEBFRAME_HOOK_H
#define QWEBFRAME_HOOK_H

#include <qwebframe.h>

#include "qobject_hook.h"

class QWebHitTestResult_hook : public QObject_hook {
  Q_OBJECT
  public:
    QWebHitTestResult_hook(QObject *handle) : QObject_hook(handle) {
    }
};


#include "qobject_hook.h"

class QWebFrame_hook : public QObject_hook {
  Q_OBJECT
  public:
    QWebFrame_hook(QObject *handle) : QObject_hook(handle) {
      javaScriptWindowObjectCleared_event.func = NULL;
      provisionalLoad_event.func = NULL;
      titleChanged_event.func = NULL;
      urlChanged_event.func = NULL;
      initialLayoutCompleted_event.func = NULL;
      iconChanged_event.func = NULL;
      contentsSizeChanged_event.func = NULL;
      loadStarted_event.func = NULL;
      loadFinished_event.func = NULL;
      pageChanged_event.func = NULL;
    }
    void hook_javaScriptWindowObjectCleared(QHook &hook) { 
      if ( !javaScriptWindowObjectCleared_event.func )
        connect(handle, SIGNAL(javaScriptWindowObjectCleared()), this, SLOT(javaScriptWindowObjectCleared_hook()));
      javaScriptWindowObjectCleared_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(javaScriptWindowObjectCleared()), this, SLOT(javaScriptWindowObjectCleared_hook()));
    }
    void hook_provisionalLoad(QHook &hook) { 
      if ( !provisionalLoad_event.func )
        connect(handle, SIGNAL(provisionalLoad()), this, SLOT(provisionalLoad_hook()));
      provisionalLoad_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(provisionalLoad()), this, SLOT(provisionalLoad_hook()));
    }
    void hook_titleChanged(QHook &hook) { 
      if ( !titleChanged_event.func )
        connect(handle, SIGNAL(titleChanged(const QString&)), this, SLOT(titleChanged_hook(const QString&)));
      titleChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(titleChanged(const QString&)), this, SLOT(titleChanged_hook(const QString&)));
    }
    void hook_urlChanged(QHook &hook) { 
      if ( !urlChanged_event.func )
        connect(handle, SIGNAL(urlChanged(const QUrl&)), this, SLOT(urlChanged_hook(const QUrl&)));
      urlChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(urlChanged(const QUrl&)), this, SLOT(urlChanged_hook(const QUrl&)));
    }
    void hook_initialLayoutCompleted(QHook &hook) { 
      if ( !initialLayoutCompleted_event.func )
        connect(handle, SIGNAL(initialLayoutCompleted()), this, SLOT(initialLayoutCompleted_hook()));
      initialLayoutCompleted_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(initialLayoutCompleted()), this, SLOT(initialLayoutCompleted_hook()));
    }
    void hook_iconChanged(QHook &hook) { 
      if ( !iconChanged_event.func )
        connect(handle, SIGNAL(iconChanged()), this, SLOT(iconChanged_hook()));
      iconChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(iconChanged()), this, SLOT(iconChanged_hook()));
    }
    void hook_contentsSizeChanged(QHook &hook) { 
      if ( !contentsSizeChanged_event.func )
        connect(handle, SIGNAL(contentsSizeChanged(const QSize&)), this, SLOT(contentsSizeChanged_hook(const QSize&)));
      contentsSizeChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(contentsSizeChanged(const QSize&)), this, SLOT(contentsSizeChanged_hook(const QSize&)));
    }
    void hook_loadStarted(QHook &hook) { 
      if ( !loadStarted_event.func )
        connect(handle, SIGNAL(loadStarted()), this, SLOT(loadStarted_hook()));
      loadStarted_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(loadStarted()), this, SLOT(loadStarted_hook()));
    }
    void hook_loadFinished(QHook &hook) { 
      if ( !loadFinished_event.func )
        connect(handle, SIGNAL(loadFinished(bool)), this, SLOT(loadFinished_hook(bool)));
      loadFinished_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(loadFinished(bool)), this, SLOT(loadFinished_hook(bool)));
    }
    void hook_pageChanged(QHook &hook) { 
      if ( !pageChanged_event.func )
        connect(handle, SIGNAL(pageChanged()), this, SLOT(pageChanged_hook()));
      pageChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(pageChanged()), this, SLOT(pageChanged_hook()));
    }

  private slots:
    void javaScriptWindowObjectCleared_hook() {
      if ( javaScriptWindowObjectCleared_event.func ) {
        typedef void (*func_type)(void *data);
	(*(func_type)javaScriptWindowObjectCleared_event.func)(javaScriptWindowObjectCleared_event.data);
      }
    }
    void provisionalLoad_hook() {
      if ( provisionalLoad_event.func ) {
        typedef void (*func_type)(void *data);
	(*(func_type)provisionalLoad_event.func)(provisionalLoad_event.data);
      }
    }
    void titleChanged_hook(const QString& title) {
      if ( titleChanged_event.func ) {
        typedef void (*func_type)(void *data, PWideString title);
	PWideString t_title;
	initializePWideString(t_title);
	copyQStringToPWideString(title, t_title);
	(*(func_type)titleChanged_event.func)(titleChanged_event.data, t_title);
	finalizePWideString(t_title);
      }
    }
    void urlChanged_hook(const QUrl& url) {
      if ( urlChanged_event.func ) {
        typedef void (*func_type)(void *data, const QUrlH url);
	(*(func_type)urlChanged_event.func)(urlChanged_event.data, (const QUrlH)&url);
      }
    }
    void initialLayoutCompleted_hook() {
      if ( initialLayoutCompleted_event.func ) {
        typedef void (*func_type)(void *data);
	(*(func_type)initialLayoutCompleted_event.func)(initialLayoutCompleted_event.data);
      }
    }
    void iconChanged_hook() {
      if ( iconChanged_event.func ) {
        typedef void (*func_type)(void *data);
	(*(func_type)iconChanged_event.func)(iconChanged_event.data);
      }
    }
    void contentsSizeChanged_hook(const QSize& size) {
      if ( contentsSizeChanged_event.func ) {
        typedef void (*func_type)(void *data, const QSizeH size);
	(*(func_type)contentsSizeChanged_event.func)(contentsSizeChanged_event.data, (const QSizeH)&size);
      }
    }
    void loadStarted_hook() {
      if ( loadStarted_event.func ) {
        typedef void (*func_type)(void *data);
	(*(func_type)loadStarted_event.func)(loadStarted_event.data);
      }
    }
    void loadFinished_hook(bool ok) {
      if ( loadFinished_event.func ) {
        typedef void (*func_type)(void *data, bool ok);
	(*(func_type)loadFinished_event.func)(loadFinished_event.data, ok);
      }
    }
    void pageChanged_hook() {
      if ( pageChanged_event.func ) {
        typedef void (*func_type)(void *data);
	(*(func_type)pageChanged_event.func)(pageChanged_event.data);
      }
    }
  private:
    QHook javaScriptWindowObjectCleared_event;
    QHook provisionalLoad_event;
    QHook titleChanged_event;
    QHook urlChanged_event;
    QHook initialLayoutCompleted_event;
    QHook iconChanged_event;
    QHook contentsSizeChanged_event;
    QHook loadStarted_event;
    QHook loadFinished_event;
    QHook pageChanged_event;
};


#endif
