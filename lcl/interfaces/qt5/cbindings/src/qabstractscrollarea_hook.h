//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QABSTRACTSCROLLAREA_HOOK_H
#define QABSTRACTSCROLLAREA_HOOK_H

#include <qabstractscrollarea.h>

#include "qframe_hook.h"

class QAbstractScrollArea_hook : public QFrame_hook {
  Q_OBJECT
  public:
    QAbstractScrollArea_hook(QObject *handle) : QFrame_hook(handle) {
    }
};


#endif
