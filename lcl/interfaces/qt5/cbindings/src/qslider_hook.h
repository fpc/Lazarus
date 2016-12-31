//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QSLIDER_HOOK_H
#define QSLIDER_HOOK_H

#include <qslider.h>

#include "qabstractslider_hook.h"

class QSlider_hook : public QAbstractSlider_hook {
  Q_OBJECT
  public:
    QSlider_hook(QObject *handle) : QAbstractSlider_hook(handle) {
    }
};


#endif
