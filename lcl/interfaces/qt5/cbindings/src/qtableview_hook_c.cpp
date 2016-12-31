//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qtableview_hook_c.h"

QTableView_hookH QTableView_hook_Create(QObjectH handle)
{
	return (QTableView_hookH) new QTableView_hook((QObject*)handle);
}

void QTableView_hook_Destroy(QTableView_hookH handle)
{
	delete (QTableView_hook *)handle;
}

