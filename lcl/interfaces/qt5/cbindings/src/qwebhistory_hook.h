//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QWEBHISTORY_HOOK_H
#define QWEBHISTORY_HOOK_H

#include <qwebhistory.h>

#include "qobject_hook.h"

class QWebHistoryItem_hook : public QObject_hook {
  Q_OBJECT
  public:
    QWebHistoryItem_hook(QObject *handle) : QObject_hook(handle) {
    }
};


#include "qobject_hook.h"

class QWebHistory_hook : public QObject_hook {
  Q_OBJECT
  public:
    QWebHistory_hook(QObject *handle) : QObject_hook(handle) {
    }
};


#endif
