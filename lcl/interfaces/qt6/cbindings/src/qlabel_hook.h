//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QLABEL_HOOK_H
#define QLABEL_HOOK_H

#include <qlabel.h>

#include "qframe_hook.h"

class QLabel_hook : public QFrame_hook {
  Q_OBJECT
  public:
    QLabel_hook(QObject *handle) : QFrame_hook(handle) {
      linkActivated_event.func = NULL;
      linkHovered_event.func = NULL;
    }
    void hook_linkActivated(QHook &hook) { 
      if ( !linkActivated_event.func )
        connect(handle, SIGNAL(linkActivated(const QString&)), this, SLOT(linkActivated_hook(const QString&)));
      linkActivated_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(linkActivated(const QString&)), this, SLOT(linkActivated_hook(const QString&)));
    }
    void hook_linkHovered(QHook &hook) { 
      if ( !linkHovered_event.func )
        connect(handle, SIGNAL(linkHovered(const QString&)), this, SLOT(linkHovered_hook(const QString&)));
      linkHovered_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(linkHovered(const QString&)), this, SLOT(linkHovered_hook(const QString&)));
    }

  private slots:
    void linkActivated_hook(const QString& link) {
      if ( linkActivated_event.func ) {
        typedef void (*func_type)(void *data, PWideString link);
	PWideString t_link;
	initializePWideString(t_link);
	copyQStringToPWideString(link, t_link);
	(*(func_type)linkActivated_event.func)(linkActivated_event.data, t_link);
	finalizePWideString(t_link);
      }
    }
    void linkHovered_hook(const QString& link) {
      if ( linkHovered_event.func ) {
        typedef void (*func_type)(void *data, PWideString link);
	PWideString t_link;
	initializePWideString(t_link);
	copyQStringToPWideString(link, t_link);
	(*(func_type)linkHovered_event.func)(linkHovered_event.data, t_link);
	finalizePWideString(t_link);
      }
    }
  private:
    QHook linkActivated_event;
    QHook linkHovered_event;
};


#endif
