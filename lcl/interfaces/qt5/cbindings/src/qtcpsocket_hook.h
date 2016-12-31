//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QTCPSOCKET_HOOK_H
#define QTCPSOCKET_HOOK_H

#include <qtcpsocket.h>

#include "qabstractsocket_hook.h"

class QTcpSocket_hook : public QAbstractSocket_hook {
  Q_OBJECT
  public:
    QTcpSocket_hook(QObject *handle) : QAbstractSocket_hook(handle) {
    }
};


#endif
