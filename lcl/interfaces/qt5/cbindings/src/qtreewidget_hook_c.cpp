//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qtreewidget_hook_c.h"

QTreeWidgetItem_hookH QTreeWidgetItem_hook_Create(QObjectH handle)
{
	return (QTreeWidgetItem_hookH) new QTreeWidgetItem_hook((QObject*)handle);
}

void QTreeWidgetItem_hook_Destroy(QTreeWidgetItem_hookH handle)
{
	delete (QTreeWidgetItem_hook *)handle;
}

QTreeWidget_hookH QTreeWidget_hook_Create(QObjectH handle)
{
	return (QTreeWidget_hookH) new QTreeWidget_hook((QObject*)handle);
}

void QTreeWidget_hook_Destroy(QTreeWidget_hookH handle)
{
	delete (QTreeWidget_hook *)handle;
}

void QTreeWidget_hook_hook_itemPressed(QTreeWidget_hookH handle, QHookH hook)
{
	((QTreeWidget_hook *)handle)->hook_itemPressed(hook);
}

void QTreeWidget_hook_hook_itemClicked(QTreeWidget_hookH handle, QHookH hook)
{
	((QTreeWidget_hook *)handle)->hook_itemClicked(hook);
}

void QTreeWidget_hook_hook_itemDoubleClicked(QTreeWidget_hookH handle, QHookH hook)
{
	((QTreeWidget_hook *)handle)->hook_itemDoubleClicked(hook);
}

void QTreeWidget_hook_hook_itemActivated(QTreeWidget_hookH handle, QHookH hook)
{
	((QTreeWidget_hook *)handle)->hook_itemActivated(hook);
}

void QTreeWidget_hook_hook_itemEntered(QTreeWidget_hookH handle, QHookH hook)
{
	((QTreeWidget_hook *)handle)->hook_itemEntered(hook);
}

void QTreeWidget_hook_hook_itemChanged(QTreeWidget_hookH handle, QHookH hook)
{
	((QTreeWidget_hook *)handle)->hook_itemChanged(hook);
}

void QTreeWidget_hook_hook_itemExpanded(QTreeWidget_hookH handle, QHookH hook)
{
	((QTreeWidget_hook *)handle)->hook_itemExpanded(hook);
}

void QTreeWidget_hook_hook_itemCollapsed(QTreeWidget_hookH handle, QHookH hook)
{
	((QTreeWidget_hook *)handle)->hook_itemCollapsed(hook);
}

void QTreeWidget_hook_hook_currentItemChanged(QTreeWidget_hookH handle, QHookH hook)
{
	((QTreeWidget_hook *)handle)->hook_currentItemChanged(hook);
}

void QTreeWidget_hook_hook_itemSelectionChanged(QTreeWidget_hookH handle, QHookH hook)
{
	((QTreeWidget_hook *)handle)->hook_itemSelectionChanged(hook);
}

