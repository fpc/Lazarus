//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QFRAME_HOOK_H
#define QFRAME_HOOK_H

#include <qframe.h>

#include "qwidget_hook.h"

class QFrame_hook : public QWidget_hook {
  Q_OBJECT
  public:
    QFrame_hook(QObject *handle) : QWidget_hook(handle) {
    }
};


#endif
