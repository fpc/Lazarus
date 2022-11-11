//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QFONTCOMBOBOX_HOOK_H
#define QFONTCOMBOBOX_HOOK_H

#include <qfontcombobox.h>

#include "qcombobox_hook.h"

class QFontComboBox_hook : public QComboBox_hook {
  Q_OBJECT
  public:
    QFontComboBox_hook(QObject *handle) : QComboBox_hook(handle) {
      currentFontChanged_event.func = NULL;
    }
    void hook_currentFontChanged(QHook &hook) { 
      if ( !currentFontChanged_event.func )
        connect(handle, SIGNAL(currentFontChanged(const QFont&)), this, SLOT(currentFontChanged_hook(const QFont&)));
      currentFontChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(currentFontChanged(const QFont&)), this, SLOT(currentFontChanged_hook(const QFont&)));
    }

  private slots:
    void currentFontChanged_hook(const QFont& f) {
      if ( currentFontChanged_event.func ) {
        typedef void (*func_type)(void *data, const QFontH f);
	(*(func_type)currentFontChanged_event.func)(currentFontChanged_event.data, (const QFontH)&f);
      }
    }
  private:
    QHook currentFontChanged_event;
};


#endif
