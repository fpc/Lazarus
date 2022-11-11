//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qwebhistory_hook_c.h"

QWebHistoryItem_hookH QWebHistoryItem_hook_Create(QObjectH handle)
{
	return (QWebHistoryItem_hookH) new QWebHistoryItem_hook((QObject*)handle);
}

void QWebHistoryItem_hook_Destroy(QWebHistoryItem_hookH handle)
{
	delete (QWebHistoryItem_hook *)handle;
}

QWebHistory_hookH QWebHistory_hook_Create(QObjectH handle)
{
	return (QWebHistory_hookH) new QWebHistory_hook((QObject*)handle);
}

void QWebHistory_hook_Destroy(QWebHistory_hookH handle)
{
	delete (QWebHistory_hook *)handle;
}

