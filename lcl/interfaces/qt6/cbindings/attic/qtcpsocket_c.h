//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QTCPSOCKET_C_H
#define QTCPSOCKET_C_H

#include <QtNetwork>
#include "pascalbind.h"

C_EXPORT QTcpSocketH QTcpSocket_Create(QObjectH parent);
C_EXPORT void QTcpSocket_Destroy(QTcpSocketH handle);

#endif
