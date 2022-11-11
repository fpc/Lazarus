//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qwebhistoryinterface_c.h"

void QWebHistoryInterface_setDefaultInterface(QWebHistoryInterfaceH defaultInterface)
{
	QWebHistoryInterface::setDefaultInterface((QWebHistoryInterface*)defaultInterface);
}

QWebHistoryInterfaceH QWebHistoryInterface_defaultInterface()
{
	return (QWebHistoryInterfaceH) QWebHistoryInterface::defaultInterface();
}

bool QWebHistoryInterface_historyContains(QWebHistoryInterfaceH handle, PWideString url)
{
	QString t_url;
	copyPWideStringToQString(url, t_url);
	return (bool) ((QWebHistoryInterface *)handle)->historyContains(t_url);
}

void QWebHistoryInterface_addHistoryEntry(QWebHistoryInterfaceH handle, PWideString url)
{
	QString t_url;
	copyPWideStringToQString(url, t_url);
	((QWebHistoryInterface *)handle)->addHistoryEntry(t_url);
}

