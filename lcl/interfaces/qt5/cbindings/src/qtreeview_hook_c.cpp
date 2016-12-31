//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qtreeview_hook_c.h"

QTreeView_hookH QTreeView_hook_Create(QObjectH handle)
{
	return (QTreeView_hookH) new QTreeView_hook((QObject*)handle);
}

void QTreeView_hook_Destroy(QTreeView_hookH handle)
{
	delete (QTreeView_hook *)handle;
}

void QTreeView_hook_hook_expanded(QTreeView_hookH handle, QHookH hook)
{
	((QTreeView_hook *)handle)->hook_expanded(hook);
}

void QTreeView_hook_hook_collapsed(QTreeView_hookH handle, QHookH hook)
{
	((QTreeView_hook *)handle)->hook_collapsed(hook);
}

