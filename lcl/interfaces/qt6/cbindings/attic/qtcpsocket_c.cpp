//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qtcpsocket_c.h"

QTcpSocketH QTcpSocket_Create(QObjectH parent)
{
	return (QTcpSocketH) new QTcpSocket((QObject*)parent);
}

void QTcpSocket_Destroy(QTcpSocketH handle)
{
	delete (QTcpSocket *)handle;
}

