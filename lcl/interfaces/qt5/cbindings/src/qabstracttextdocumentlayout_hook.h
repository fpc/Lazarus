//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QABSTRACTTEXTDOCUMENTLAYOUT_HOOK_H
#define QABSTRACTTEXTDOCUMENTLAYOUT_HOOK_H

#include <qabstracttextdocumentlayout.h>

#include "qobject_hook.h"

class QAbstractTextDocumentLayout_hook : public QObject_hook {
  Q_OBJECT
  public:
    QAbstractTextDocumentLayout_hook(QObject *handle) : QObject_hook(handle) {
      update_event.func = NULL;
      update2_event.func = NULL;
      updateBlock_event.func = NULL;
      documentSizeChanged_event.func = NULL;
      pageCountChanged_event.func = NULL;
    }
    void hook_update(QHook &hook) { 
      if ( !update_event.func )
        connect(handle, SIGNAL(update(const QRectF&)), this, SLOT(update_hook(const QRectF&)));
      update_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(update(const QRectF&)), this, SLOT(update_hook(const QRectF&)));
    }
    void hook_update2(QHook &hook) { 
      if ( !update2_event.func )
        connect(handle, SIGNAL(update()), this, SLOT(update2_hook()));
      update2_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(update()), this, SLOT(update2_hook()));
    }
    void hook_updateBlock(QHook &hook) { 
      if ( !updateBlock_event.func )
        connect(handle, SIGNAL(updateBlock(const QTextBlock&)), this, SLOT(updateBlock_hook(const QTextBlock&)));
      updateBlock_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(updateBlock(const QTextBlock&)), this, SLOT(updateBlock_hook(const QTextBlock&)));
    }
    void hook_documentSizeChanged(QHook &hook) { 
      if ( !documentSizeChanged_event.func )
        connect(handle, SIGNAL(documentSizeChanged(const QSizeF&)), this, SLOT(documentSizeChanged_hook(const QSizeF&)));
      documentSizeChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(documentSizeChanged(const QSizeF&)), this, SLOT(documentSizeChanged_hook(const QSizeF&)));
    }
    void hook_pageCountChanged(QHook &hook) { 
      if ( !pageCountChanged_event.func )
        connect(handle, SIGNAL(pageCountChanged(int)), this, SLOT(pageCountChanged_hook(int)));
      pageCountChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(pageCountChanged(int)), this, SLOT(pageCountChanged_hook(int)));
    }

  private slots:
    void update_hook(const QRectF& AnonParam1) {
      if ( update_event.func ) {
        typedef void (*func_type)(void *data, const QRectFH AnonParam1);
	(*(func_type)update_event.func)(update_event.data, (const QRectFH)&AnonParam1);
      }
    }
    void update2_hook() {
      if ( update2_event.func ) {
        typedef void (*func_type)(void *data);
	(*(func_type)update2_event.func)(update2_event.data);
      }
    }
    void updateBlock_hook(const QTextBlock& block) {
      if ( updateBlock_event.func ) {
        typedef void (*func_type)(void *data, const QTextBlockH block);
	(*(func_type)updateBlock_event.func)(updateBlock_event.data, (const QTextBlockH)&block);
      }
    }
    void documentSizeChanged_hook(const QSizeF& newSize) {
      if ( documentSizeChanged_event.func ) {
        typedef void (*func_type)(void *data, const QSizeFH newSize);
	(*(func_type)documentSizeChanged_event.func)(documentSizeChanged_event.data, (const QSizeFH)&newSize);
      }
    }
    void pageCountChanged_hook(int newPages) {
      if ( pageCountChanged_event.func ) {
        typedef void (*func_type)(void *data, int newPages);
	(*(func_type)pageCountChanged_event.func)(pageCountChanged_event.data, newPages);
      }
    }
  private:
    QHook update_event;
    QHook update2_event;
    QHook updateBlock_event;
    QHook documentSizeChanged_event;
    QHook pageCountChanged_event;
};


#include "qobject_hook.h"

class QTextObjectInterface_hook : public QObject_hook {
  Q_OBJECT
  public:
    QTextObjectInterface_hook(QObject *handle) : QObject_hook(handle) {
    }
};


#endif
