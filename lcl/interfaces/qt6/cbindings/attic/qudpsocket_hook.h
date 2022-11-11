//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QUDPSOCKET_HOOK_H
#define QUDPSOCKET_HOOK_H

#include <qudpsocket.h>

#include "qabstractsocket_hook.h"

class QUdpSocket_hook : public QAbstractSocket_hook {
  Q_OBJECT
  public:
    QUdpSocket_hook(QObject *handle) : QAbstractSocket_hook(handle) {
    }
};


#endif
