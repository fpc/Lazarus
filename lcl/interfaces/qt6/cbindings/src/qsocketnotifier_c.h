//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QSOCKETNOTIFIER_C_H
#define QSOCKETNOTIFIER_C_H

#include <QtCore>
#include "pascalbind.h"

C_EXPORT QSocketNotifierH QSocketNotifier_Create(qintptr socket, QSocketNotifier::Type AnonParam2, QObjectH parent);
C_EXPORT void QSocketNotifier_Destroy(QSocketNotifierH handle);
C_EXPORT qintptr QSocketNotifier_socket(QSocketNotifierH handle);
C_EXPORT QSocketNotifier::Type QSocketNotifier_type(QSocketNotifierH handle);
C_EXPORT bool QSocketNotifier_isEnabled(QSocketNotifierH handle);
C_EXPORT void QSocketNotifier_setEnabled(QSocketNotifierH handle, bool AnonParam1);

#endif
