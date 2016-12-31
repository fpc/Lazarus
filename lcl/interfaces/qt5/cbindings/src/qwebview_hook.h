//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QWEBVIEW_HOOK_H
#define QWEBVIEW_HOOK_H

#include <qwebview.h>

#include "qwidget_hook.h"

class QWebView_hook : public QWidget_hook {
  Q_OBJECT
  public:
    QWebView_hook(QObject *handle) : QWidget_hook(handle) {
      loadStarted_event.func = NULL;
      loadProgress_event.func = NULL;
      loadFinished_event.func = NULL;
      titleChanged_event.func = NULL;
      statusBarMessage_event.func = NULL;
      linkClicked_event.func = NULL;
      selectionChanged_event.func = NULL;
      iconChanged_event.func = NULL;
      urlChanged_event.func = NULL;
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
    void hook_titleChanged(QHook &hook) { 
      if ( !titleChanged_event.func )
        connect(handle, SIGNAL(titleChanged(const QString&)), this, SLOT(titleChanged_hook(const QString&)));
      titleChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(titleChanged(const QString&)), this, SLOT(titleChanged_hook(const QString&)));
    }
    void hook_statusBarMessage(QHook &hook) { 
      if ( !statusBarMessage_event.func )
        connect(handle, SIGNAL(statusBarMessage(const QString&)), this, SLOT(statusBarMessage_hook(const QString&)));
      statusBarMessage_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(statusBarMessage(const QString&)), this, SLOT(statusBarMessage_hook(const QString&)));
    }
    void hook_linkClicked(QHook &hook) { 
      if ( !linkClicked_event.func )
        connect(handle, SIGNAL(linkClicked(const QUrl&)), this, SLOT(linkClicked_hook(const QUrl&)));
      linkClicked_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(linkClicked(const QUrl&)), this, SLOT(linkClicked_hook(const QUrl&)));
    }
    void hook_selectionChanged(QHook &hook) { 
      if ( !selectionChanged_event.func )
        connect(handle, SIGNAL(selectionChanged()), this, SLOT(selectionChanged_hook()));
      selectionChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(selectionChanged()), this, SLOT(selectionChanged_hook()));
    }
    void hook_iconChanged(QHook &hook) { 
      if ( !iconChanged_event.func )
        connect(handle, SIGNAL(iconChanged()), this, SLOT(iconChanged_hook()));
      iconChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(iconChanged()), this, SLOT(iconChanged_hook()));
    }
    void hook_urlChanged(QHook &hook) { 
      if ( !urlChanged_event.func )
        connect(handle, SIGNAL(urlChanged(const QUrl&)), this, SLOT(urlChanged_hook(const QUrl&)));
      urlChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(urlChanged(const QUrl&)), this, SLOT(urlChanged_hook(const QUrl&)));
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
    void loadFinished_hook(bool AnonParam1) {
      if ( loadFinished_event.func ) {
        typedef void (*func_type)(void *data, bool AnonParam1);
	(*(func_type)loadFinished_event.func)(loadFinished_event.data, AnonParam1);
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
    void linkClicked_hook(const QUrl& AnonParam1) {
      if ( linkClicked_event.func ) {
        typedef void (*func_type)(void *data, const QUrlH AnonParam1);
	(*(func_type)linkClicked_event.func)(linkClicked_event.data, (const QUrlH)&AnonParam1);
      }
    }
    void selectionChanged_hook() {
      if ( selectionChanged_event.func ) {
        typedef void (*func_type)(void *data);
	(*(func_type)selectionChanged_event.func)(selectionChanged_event.data);
      }
    }
    void iconChanged_hook() {
      if ( iconChanged_event.func ) {
        typedef void (*func_type)(void *data);
	(*(func_type)iconChanged_event.func)(iconChanged_event.data);
      }
    }
    void urlChanged_hook(const QUrl& AnonParam1) {
      if ( urlChanged_event.func ) {
        typedef void (*func_type)(void *data, const QUrlH AnonParam1);
	(*(func_type)urlChanged_event.func)(urlChanged_event.data, (const QUrlH)&AnonParam1);
      }
    }
  private:
    QHook loadStarted_event;
    QHook loadProgress_event;
    QHook loadFinished_event;
    QHook titleChanged_event;
    QHook statusBarMessage_event;
    QHook linkClicked_event;
    QHook selectionChanged_event;
    QHook iconChanged_event;
    QHook urlChanged_event;
};


#endif
