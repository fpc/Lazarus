//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QPUSHBUTTON_HOOK_H
#define QPUSHBUTTON_HOOK_H

#include <qpushbutton.h>

#include "qabstractbutton_hook.h"

class QPushButton_hook : public QAbstractButton_hook {
  Q_OBJECT
  public:
    QPushButton_hook(QObject *handle) : QAbstractButton_hook(handle) {
    }
};


#endif
