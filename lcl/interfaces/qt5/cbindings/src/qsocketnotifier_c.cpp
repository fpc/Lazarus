//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qsocketnotifier_c.h"

QSocketNotifierH QSocketNotifier_Create(qintptr socket, QSocketNotifier::Type AnonParam2, QObjectH parent)
{
	return (QSocketNotifierH) new QSocketNotifier(socket, AnonParam2, (QObject*)parent);
}

void QSocketNotifier_Destroy(QSocketNotifierH handle)
{
	delete (QSocketNotifier *)handle;
}

qintptr QSocketNotifier_socket(QSocketNotifierH handle)
{
	return (qintptr) ((QSocketNotifier *)handle)->socket();
}

QSocketNotifier::Type QSocketNotifier_type(QSocketNotifierH handle)
{
	return (QSocketNotifier::Type) ((QSocketNotifier *)handle)->type();
}

bool QSocketNotifier_isEnabled(QSocketNotifierH handle)
{
	return (bool) ((QSocketNotifier *)handle)->isEnabled();
}

void QSocketNotifier_setEnabled(QSocketNotifierH handle, bool AnonParam1)
{
	((QSocketNotifier *)handle)->setEnabled(AnonParam1);
}

