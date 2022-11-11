//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QLAYOUT_HOOK_H
#define QLAYOUT_HOOK_H

#include <qlayout.h>

#include "qobject_hook.h"

class QLayout_hook : public QObject_hook {
  Q_OBJECT
  public:
    QLayout_hook(QObject *handle) : QObject_hook(handle) {
    }
};


#endif
