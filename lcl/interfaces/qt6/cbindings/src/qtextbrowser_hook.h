//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QTEXTBROWSER_HOOK_H
#define QTEXTBROWSER_HOOK_H

#include <qtextbrowser.h>

#include "qtextedit_hook.h"

class QTextBrowser_hook : public QTextEdit_hook {
  Q_OBJECT
  public:
    QTextBrowser_hook(QObject *handle) : QTextEdit_hook(handle) {
      backwardAvailable_event.func = NULL;
      forwardAvailable_event.func = NULL;
      historyChanged_event.func = NULL;
      sourceChanged_event.func = NULL;
      highlighted_event.func = NULL;
      highlighted2_event.func = NULL;
      anchorClicked_event.func = NULL;
    }
    void hook_backwardAvailable(QHook &hook) { 
      if ( !backwardAvailable_event.func )
        connect(handle, SIGNAL(backwardAvailable(bool)), this, SLOT(backwardAvailable_hook(bool)));
      backwardAvailable_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(backwardAvailable(bool)), this, SLOT(backwardAvailable_hook(bool)));
    }
    void hook_forwardAvailable(QHook &hook) { 
      if ( !forwardAvailable_event.func )
        connect(handle, SIGNAL(forwardAvailable(bool)), this, SLOT(forwardAvailable_hook(bool)));
      forwardAvailable_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(forwardAvailable(bool)), this, SLOT(forwardAvailable_hook(bool)));
    }
    void hook_historyChanged(QHook &hook) { 
      if ( !historyChanged_event.func )
        connect(handle, SIGNAL(historyChanged()), this, SLOT(historyChanged_hook()));
      historyChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(historyChanged()), this, SLOT(historyChanged_hook()));
    }
    void hook_sourceChanged(QHook &hook) { 
      if ( !sourceChanged_event.func )
        connect(handle, SIGNAL(sourceChanged(const QUrl&)), this, SLOT(sourceChanged_hook(const QUrl&)));
      sourceChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(sourceChanged(const QUrl&)), this, SLOT(sourceChanged_hook(const QUrl&)));
    }
    void hook_highlighted(QHook &hook) { 
      if ( !highlighted_event.func )
        connect(handle, SIGNAL(highlighted(const QUrl&)), this, SLOT(highlighted_hook(const QUrl&)));
      highlighted_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(highlighted(const QUrl&)), this, SLOT(highlighted_hook(const QUrl&)));
    }
    void hook_highlighted2(QHook &hook) { 
      if ( !highlighted2_event.func )
        connect(handle, SIGNAL(highlighted(const QString&)), this, SLOT(highlighted2_hook(const QString&)));
      highlighted2_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(highlighted(const QString&)), this, SLOT(highlighted2_hook(const QString&)));
    }
    void hook_anchorClicked(QHook &hook) { 
      if ( !anchorClicked_event.func )
        connect(handle, SIGNAL(anchorClicked(const QUrl&)), this, SLOT(anchorClicked_hook(const QUrl&)));
      anchorClicked_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(anchorClicked(const QUrl&)), this, SLOT(anchorClicked_hook(const QUrl&)));
    }

  private slots:
    void backwardAvailable_hook(bool AnonParam1) {
      if ( backwardAvailable_event.func ) {
        typedef void (*func_type)(void *data, bool AnonParam1);
	(*(func_type)backwardAvailable_event.func)(backwardAvailable_event.data, AnonParam1);
      }
    }
    void forwardAvailable_hook(bool AnonParam1) {
      if ( forwardAvailable_event.func ) {
        typedef void (*func_type)(void *data, bool AnonParam1);
	(*(func_type)forwardAvailable_event.func)(forwardAvailable_event.data, AnonParam1);
      }
    }
    void historyChanged_hook() {
      if ( historyChanged_event.func ) {
        typedef void (*func_type)(void *data);
	(*(func_type)historyChanged_event.func)(historyChanged_event.data);
      }
    }
    void sourceChanged_hook(const QUrl& AnonParam1) {
      if ( sourceChanged_event.func ) {
        typedef void (*func_type)(void *data, const QUrlH AnonParam1);
	(*(func_type)sourceChanged_event.func)(sourceChanged_event.data, (const QUrlH)&AnonParam1);
      }
    }
    void highlighted_hook(const QUrl& AnonParam1) {
      if ( highlighted_event.func ) {
        typedef void (*func_type)(void *data, const QUrlH AnonParam1);
	(*(func_type)highlighted_event.func)(highlighted_event.data, (const QUrlH)&AnonParam1);
      }
    }
    void highlighted2_hook(const QString& AnonParam1) {
      if ( highlighted2_event.func ) {
        typedef void (*func_type)(void *data, PWideString AnonParam1);
	PWideString t_AnonParam1;
	initializePWideString(t_AnonParam1);
	copyQStringToPWideString(AnonParam1, t_AnonParam1);
	(*(func_type)highlighted2_event.func)(highlighted2_event.data, t_AnonParam1);
	finalizePWideString(t_AnonParam1);
      }
    }
    void anchorClicked_hook(const QUrl& AnonParam1) {
      if ( anchorClicked_event.func ) {
        typedef void (*func_type)(void *data, const QUrlH AnonParam1);
	(*(func_type)anchorClicked_event.func)(anchorClicked_event.data, (const QUrlH)&AnonParam1);
      }
    }
  private:
    QHook backwardAvailable_event;
    QHook forwardAvailable_event;
    QHook historyChanged_event;
    QHook sourceChanged_event;
    QHook highlighted_event;
    QHook highlighted2_event;
    QHook anchorClicked_event;
};


#endif
