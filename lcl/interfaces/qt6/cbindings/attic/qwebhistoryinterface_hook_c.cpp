//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qwebhistoryinterface_hook_c.h"

QWebHistoryInterface_hookH QWebHistoryInterface_hook_Create(QObjectH handle)
{
	return (QWebHistoryInterface_hookH) new QWebHistoryInterface_hook((QObject*)handle);
}

void QWebHistoryInterface_hook_Destroy(QWebHistoryInterface_hookH handle)
{
	delete (QWebHistoryInterface_hook *)handle;
}

