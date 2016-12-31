//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qtablewidget_hook_c.h"

QTableWidgetSelectionRange_hookH QTableWidgetSelectionRange_hook_Create(QObjectH handle)
{
	return (QTableWidgetSelectionRange_hookH) new QTableWidgetSelectionRange_hook((QObject*)handle);
}

void QTableWidgetSelectionRange_hook_Destroy(QTableWidgetSelectionRange_hookH handle)
{
	delete (QTableWidgetSelectionRange_hook *)handle;
}

QTableWidgetItem_hookH QTableWidgetItem_hook_Create(QObjectH handle)
{
	return (QTableWidgetItem_hookH) new QTableWidgetItem_hook((QObject*)handle);
}

void QTableWidgetItem_hook_Destroy(QTableWidgetItem_hookH handle)
{
	delete (QTableWidgetItem_hook *)handle;
}

QTableWidget_hookH QTableWidget_hook_Create(QObjectH handle)
{
	return (QTableWidget_hookH) new QTableWidget_hook((QObject*)handle);
}

void QTableWidget_hook_Destroy(QTableWidget_hookH handle)
{
	delete (QTableWidget_hook *)handle;
}

void QTableWidget_hook_hook_itemPressed(QTableWidget_hookH handle, QHookH hook)
{
	((QTableWidget_hook *)handle)->hook_itemPressed(hook);
}

void QTableWidget_hook_hook_itemClicked(QTableWidget_hookH handle, QHookH hook)
{
	((QTableWidget_hook *)handle)->hook_itemClicked(hook);
}

void QTableWidget_hook_hook_itemDoubleClicked(QTableWidget_hookH handle, QHookH hook)
{
	((QTableWidget_hook *)handle)->hook_itemDoubleClicked(hook);
}

void QTableWidget_hook_hook_itemActivated(QTableWidget_hookH handle, QHookH hook)
{
	((QTableWidget_hook *)handle)->hook_itemActivated(hook);
}

void QTableWidget_hook_hook_itemEntered(QTableWidget_hookH handle, QHookH hook)
{
	((QTableWidget_hook *)handle)->hook_itemEntered(hook);
}

void QTableWidget_hook_hook_itemChanged(QTableWidget_hookH handle, QHookH hook)
{
	((QTableWidget_hook *)handle)->hook_itemChanged(hook);
}

void QTableWidget_hook_hook_currentItemChanged(QTableWidget_hookH handle, QHookH hook)
{
	((QTableWidget_hook *)handle)->hook_currentItemChanged(hook);
}

void QTableWidget_hook_hook_itemSelectionChanged(QTableWidget_hookH handle, QHookH hook)
{
	((QTableWidget_hook *)handle)->hook_itemSelectionChanged(hook);
}

void QTableWidget_hook_hook_cellPressed(QTableWidget_hookH handle, QHookH hook)
{
	((QTableWidget_hook *)handle)->hook_cellPressed(hook);
}

void QTableWidget_hook_hook_cellClicked(QTableWidget_hookH handle, QHookH hook)
{
	((QTableWidget_hook *)handle)->hook_cellClicked(hook);
}

void QTableWidget_hook_hook_cellDoubleClicked(QTableWidget_hookH handle, QHookH hook)
{
	((QTableWidget_hook *)handle)->hook_cellDoubleClicked(hook);
}

void QTableWidget_hook_hook_cellActivated(QTableWidget_hookH handle, QHookH hook)
{
	((QTableWidget_hook *)handle)->hook_cellActivated(hook);
}

void QTableWidget_hook_hook_cellEntered(QTableWidget_hookH handle, QHookH hook)
{
	((QTableWidget_hook *)handle)->hook_cellEntered(hook);
}

void QTableWidget_hook_hook_cellChanged(QTableWidget_hookH handle, QHookH hook)
{
	((QTableWidget_hook *)handle)->hook_cellChanged(hook);
}

void QTableWidget_hook_hook_currentCellChanged(QTableWidget_hookH handle, QHookH hook)
{
	((QTableWidget_hook *)handle)->hook_currentCellChanged(hook);
}

