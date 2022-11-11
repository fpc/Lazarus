//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qlistwidget_hook_c.h"

QListWidgetItem_hookH QListWidgetItem_hook_Create(QObjectH handle)
{
	return (QListWidgetItem_hookH) new QListWidgetItem_hook((QObject*)handle);
}

void QListWidgetItem_hook_Destroy(QListWidgetItem_hookH handle)
{
	delete (QListWidgetItem_hook *)handle;
}

QListWidget_hookH QListWidget_hook_Create(QObjectH handle)
{
	return (QListWidget_hookH) new QListWidget_hook((QObject*)handle);
}

void QListWidget_hook_Destroy(QListWidget_hookH handle)
{
	delete (QListWidget_hook *)handle;
}

void QListWidget_hook_hook_itemPressed(QListWidget_hookH handle, QHookH hook)
{
	((QListWidget_hook *)handle)->hook_itemPressed(hook);
}

void QListWidget_hook_hook_itemClicked(QListWidget_hookH handle, QHookH hook)
{
	((QListWidget_hook *)handle)->hook_itemClicked(hook);
}

void QListWidget_hook_hook_itemDoubleClicked(QListWidget_hookH handle, QHookH hook)
{
	((QListWidget_hook *)handle)->hook_itemDoubleClicked(hook);
}

void QListWidget_hook_hook_itemActivated(QListWidget_hookH handle, QHookH hook)
{
	((QListWidget_hook *)handle)->hook_itemActivated(hook);
}

void QListWidget_hook_hook_itemEntered(QListWidget_hookH handle, QHookH hook)
{
	((QListWidget_hook *)handle)->hook_itemEntered(hook);
}

void QListWidget_hook_hook_itemChanged(QListWidget_hookH handle, QHookH hook)
{
	((QListWidget_hook *)handle)->hook_itemChanged(hook);
}

void QListWidget_hook_hook_currentItemChanged(QListWidget_hookH handle, QHookH hook)
{
	((QListWidget_hook *)handle)->hook_currentItemChanged(hook);
}

void QListWidget_hook_hook_currentTextChanged(QListWidget_hookH handle, QHookH hook)
{
	((QListWidget_hook *)handle)->hook_currentTextChanged(hook);
}

void QListWidget_hook_hook_currentRowChanged(QListWidget_hookH handle, QHookH hook)
{
	((QListWidget_hook *)handle)->hook_currentRowChanged(hook);
}

void QListWidget_hook_hook_itemSelectionChanged(QListWidget_hookH handle, QHookH hook)
{
	((QListWidget_hook *)handle)->hook_itemSelectionChanged(hook);
}

