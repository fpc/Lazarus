//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qabstractscrollarea_hook_c.h"

QAbstractScrollArea_hookH QAbstractScrollArea_hook_Create(QObjectH handle)
{
	return (QAbstractScrollArea_hookH) new QAbstractScrollArea_hook((QObject*)handle);
}

void QAbstractScrollArea_hook_Destroy(QAbstractScrollArea_hookH handle)
{
	delete (QAbstractScrollArea_hook *)handle;
}

