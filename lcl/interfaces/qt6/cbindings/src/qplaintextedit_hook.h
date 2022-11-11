//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QPLAINTEXTEDIT_HOOK_H
#define QPLAINTEXTEDIT_HOOK_H

#include <qplaintextedit.h>

#include "qabstractscrollarea_hook.h"

class QPlainTextEdit_hook : public QAbstractScrollArea_hook {
  Q_OBJECT
  public:
    QPlainTextEdit_hook(QObject *handle) : QAbstractScrollArea_hook(handle) {
      textChanged_event.func = NULL;
      undoAvailable_event.func = NULL;
      redoAvailable_event.func = NULL;
      copyAvailable_event.func = NULL;
      selectionChanged_event.func = NULL;
      cursorPositionChanged_event.func = NULL;
      updateRequest_event.func = NULL;
      blockCountChanged_event.func = NULL;
      modificationChanged_event.func = NULL;
    }
    void hook_textChanged(QHook &hook) { 
      if ( !textChanged_event.func )
        connect(handle, SIGNAL(textChanged()), this, SLOT(textChanged_hook()));
      textChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(textChanged()), this, SLOT(textChanged_hook()));
    }
    void hook_undoAvailable(QHook &hook) { 
      if ( !undoAvailable_event.func )
        connect(handle, SIGNAL(undoAvailable(bool)), this, SLOT(undoAvailable_hook(bool)));
      undoAvailable_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(undoAvailable(bool)), this, SLOT(undoAvailable_hook(bool)));
    }
    void hook_redoAvailable(QHook &hook) { 
      if ( !redoAvailable_event.func )
        connect(handle, SIGNAL(redoAvailable(bool)), this, SLOT(redoAvailable_hook(bool)));
      redoAvailable_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(redoAvailable(bool)), this, SLOT(redoAvailable_hook(bool)));
    }
    void hook_copyAvailable(QHook &hook) { 
      if ( !copyAvailable_event.func )
        connect(handle, SIGNAL(copyAvailable(bool)), this, SLOT(copyAvailable_hook(bool)));
      copyAvailable_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(copyAvailable(bool)), this, SLOT(copyAvailable_hook(bool)));
    }
    void hook_selectionChanged(QHook &hook) { 
      if ( !selectionChanged_event.func )
        connect(handle, SIGNAL(selectionChanged()), this, SLOT(selectionChanged_hook()));
      selectionChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(selectionChanged()), this, SLOT(selectionChanged_hook()));
    }
    void hook_cursorPositionChanged(QHook &hook) { 
      if ( !cursorPositionChanged_event.func )
        connect(handle, SIGNAL(cursorPositionChanged()), this, SLOT(cursorPositionChanged_hook()));
      cursorPositionChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(cursorPositionChanged()), this, SLOT(cursorPositionChanged_hook()));
    }
    void hook_updateRequest(QHook &hook) { 
      if ( !updateRequest_event.func )
        connect(handle, SIGNAL(updateRequest(const QRect&, int)), this, SLOT(updateRequest_hook(const QRect&, int)));
      updateRequest_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(updateRequest(const QRect&, int)), this, SLOT(updateRequest_hook(const QRect&, int)));
    }
    void hook_blockCountChanged(QHook &hook) { 
      if ( !blockCountChanged_event.func )
        connect(handle, SIGNAL(blockCountChanged(int)), this, SLOT(blockCountChanged_hook(int)));
      blockCountChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(blockCountChanged(int)), this, SLOT(blockCountChanged_hook(int)));
    }
    void hook_modificationChanged(QHook &hook) { 
      if ( !modificationChanged_event.func )
        connect(handle, SIGNAL(modificationChanged(bool)), this, SLOT(modificationChanged_hook(bool)));
      modificationChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(modificationChanged(bool)), this, SLOT(modificationChanged_hook(bool)));
    }

  private slots:
    void textChanged_hook() {
      if ( textChanged_event.func ) {
        typedef void (*func_type)(void *data);
	(*(func_type)textChanged_event.func)(textChanged_event.data);
      }
    }
    void undoAvailable_hook(bool b) {
      if ( undoAvailable_event.func ) {
        typedef void (*func_type)(void *data, bool b);
	(*(func_type)undoAvailable_event.func)(undoAvailable_event.data, b);
      }
    }
    void redoAvailable_hook(bool b) {
      if ( redoAvailable_event.func ) {
        typedef void (*func_type)(void *data, bool b);
	(*(func_type)redoAvailable_event.func)(redoAvailable_event.data, b);
      }
    }
    void copyAvailable_hook(bool b) {
      if ( copyAvailable_event.func ) {
        typedef void (*func_type)(void *data, bool b);
	(*(func_type)copyAvailable_event.func)(copyAvailable_event.data, b);
      }
    }
    void selectionChanged_hook() {
      if ( selectionChanged_event.func ) {
        typedef void (*func_type)(void *data);
	(*(func_type)selectionChanged_event.func)(selectionChanged_event.data);
      }
    }
    void cursorPositionChanged_hook() {
      if ( cursorPositionChanged_event.func ) {
        typedef void (*func_type)(void *data);
	(*(func_type)cursorPositionChanged_event.func)(cursorPositionChanged_event.data);
      }
    }
    void updateRequest_hook(const QRect& rect, int dy) {
      if ( updateRequest_event.func ) {
        typedef void (*func_type)(void *data, PRect rect, int dy);
	PRect t_rect;
	copyQRectToPRect(rect, t_rect);
	(*(func_type)updateRequest_event.func)(updateRequest_event.data, t_rect, dy);
      }
    }
    void blockCountChanged_hook(int newBlockCount) {
      if ( blockCountChanged_event.func ) {
        typedef void (*func_type)(void *data, int newBlockCount);
	(*(func_type)blockCountChanged_event.func)(blockCountChanged_event.data, newBlockCount);
      }
    }
    void modificationChanged_hook(bool AnonParam1) {
      if ( modificationChanged_event.func ) {
        typedef void (*func_type)(void *data, bool AnonParam1);
	(*(func_type)modificationChanged_event.func)(modificationChanged_event.data, AnonParam1);
      }
    }
  private:
    QHook textChanged_event;
    QHook undoAvailable_event;
    QHook redoAvailable_event;
    QHook copyAvailable_event;
    QHook selectionChanged_event;
    QHook cursorPositionChanged_event;
    QHook updateRequest_event;
    QHook blockCountChanged_event;
    QHook modificationChanged_event;
};


#include "qabstracttextdocumentlayout_hook.h"

class QPlainTextDocumentLayout_hook : public QAbstractTextDocumentLayout_hook {
  Q_OBJECT
  public:
    QPlainTextDocumentLayout_hook(QObject *handle) : QAbstractTextDocumentLayout_hook(handle) {
    }
};


#endif
