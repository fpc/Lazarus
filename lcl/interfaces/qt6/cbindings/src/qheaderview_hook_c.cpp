//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qheaderview_hook_c.h"

QHeaderView_hookH QHeaderView_hook_Create(QObjectH handle)
{
	return (QHeaderView_hookH) new QHeaderView_hook((QObject*)handle);
}

void QHeaderView_hook_Destroy(QHeaderView_hookH handle)
{
	delete (QHeaderView_hook *)handle;
}

void QHeaderView_hook_hook_sectionMoved(QHeaderView_hookH handle, QHookH hook)
{
	((QHeaderView_hook *)handle)->hook_sectionMoved(hook);
}

void QHeaderView_hook_hook_sectionResized(QHeaderView_hookH handle, QHookH hook)
{
	((QHeaderView_hook *)handle)->hook_sectionResized(hook);
}

void QHeaderView_hook_hook_sectionPressed(QHeaderView_hookH handle, QHookH hook)
{
	((QHeaderView_hook *)handle)->hook_sectionPressed(hook);
}

void QHeaderView_hook_hook_sectionClicked(QHeaderView_hookH handle, QHookH hook)
{
	((QHeaderView_hook *)handle)->hook_sectionClicked(hook);
}

void QHeaderView_hook_hook_sectionEntered(QHeaderView_hookH handle, QHookH hook)
{
	((QHeaderView_hook *)handle)->hook_sectionEntered(hook);
}

void QHeaderView_hook_hook_sectionDoubleClicked(QHeaderView_hookH handle, QHookH hook)
{
	((QHeaderView_hook *)handle)->hook_sectionDoubleClicked(hook);
}

void QHeaderView_hook_hook_sectionCountChanged(QHeaderView_hookH handle, QHookH hook)
{
	((QHeaderView_hook *)handle)->hook_sectionCountChanged(hook);
}

void QHeaderView_hook_hook_sectionHandleDoubleClicked(QHeaderView_hookH handle, QHookH hook)
{
	((QHeaderView_hook *)handle)->hook_sectionHandleDoubleClicked(hook);
}

void QHeaderView_hook_hook_geometriesChanged(QHeaderView_hookH handle, QHookH hook)
{
	((QHeaderView_hook *)handle)->hook_geometriesChanged(hook);
}

void QHeaderView_hook_hook_sortIndicatorChanged(QHeaderView_hookH handle, QHookH hook)
{
	((QHeaderView_hook *)handle)->hook_sortIndicatorChanged(hook);
}

