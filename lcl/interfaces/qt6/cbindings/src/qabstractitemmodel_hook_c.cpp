//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qabstractitemmodel_hook_c.h"

QModelIndex_hookH QModelIndex_hook_Create(QObjectH handle)
{
	return (QModelIndex_hookH) new QModelIndex_hook((QObject*)handle);
}

void QModelIndex_hook_Destroy(QModelIndex_hookH handle)
{
	delete (QModelIndex_hook *)handle;
}

QPersistentModelIndex_hookH QPersistentModelIndex_hook_Create(QObjectH handle)
{
	return (QPersistentModelIndex_hookH) new QPersistentModelIndex_hook((QObject*)handle);
}

void QPersistentModelIndex_hook_Destroy(QPersistentModelIndex_hookH handle)
{
	delete (QPersistentModelIndex_hook *)handle;
}

QAbstractItemModel_hookH QAbstractItemModel_hook_Create(QObjectH handle)
{
	return (QAbstractItemModel_hookH) new QAbstractItemModel_hook((QObject*)handle);
}

void QAbstractItemModel_hook_Destroy(QAbstractItemModel_hookH handle)
{
	delete (QAbstractItemModel_hook *)handle;
}

void QAbstractItemModel_hook_hook_dataChanged(QAbstractItemModel_hookH handle, QHookH hook)
{
	((QAbstractItemModel_hook *)handle)->hook_dataChanged(hook);
}

void QAbstractItemModel_hook_hook_headerDataChanged(QAbstractItemModel_hookH handle, QHookH hook)
{
	((QAbstractItemModel_hook *)handle)->hook_headerDataChanged(hook);
}

void QAbstractItemModel_hook_hook_layoutChanged(QAbstractItemModel_hookH handle, QHookH hook)
{
	((QAbstractItemModel_hook *)handle)->hook_layoutChanged(hook);
}

void QAbstractItemModel_hook_hook_layoutAboutToBeChanged(QAbstractItemModel_hookH handle, QHookH hook)
{
	((QAbstractItemModel_hook *)handle)->hook_layoutAboutToBeChanged(hook);
}

void QAbstractItemModel_hook_hook_rowsAboutToBeInserted(QAbstractItemModel_hookH handle, QHookH hook)
{
	((QAbstractItemModel_hook *)handle)->hook_rowsAboutToBeInserted(hook);
}

void QAbstractItemModel_hook_hook_rowsInserted(QAbstractItemModel_hookH handle, QHookH hook)
{
	((QAbstractItemModel_hook *)handle)->hook_rowsInserted(hook);
}

void QAbstractItemModel_hook_hook_rowsAboutToBeRemoved(QAbstractItemModel_hookH handle, QHookH hook)
{
	((QAbstractItemModel_hook *)handle)->hook_rowsAboutToBeRemoved(hook);
}

void QAbstractItemModel_hook_hook_rowsRemoved(QAbstractItemModel_hookH handle, QHookH hook)
{
	((QAbstractItemModel_hook *)handle)->hook_rowsRemoved(hook);
}

void QAbstractItemModel_hook_hook_columnsAboutToBeInserted(QAbstractItemModel_hookH handle, QHookH hook)
{
	((QAbstractItemModel_hook *)handle)->hook_columnsAboutToBeInserted(hook);
}

void QAbstractItemModel_hook_hook_columnsInserted(QAbstractItemModel_hookH handle, QHookH hook)
{
	((QAbstractItemModel_hook *)handle)->hook_columnsInserted(hook);
}

void QAbstractItemModel_hook_hook_columnsAboutToBeRemoved(QAbstractItemModel_hookH handle, QHookH hook)
{
	((QAbstractItemModel_hook *)handle)->hook_columnsAboutToBeRemoved(hook);
}

void QAbstractItemModel_hook_hook_columnsRemoved(QAbstractItemModel_hookH handle, QHookH hook)
{
	((QAbstractItemModel_hook *)handle)->hook_columnsRemoved(hook);
}

void QAbstractItemModel_hook_hook_modelAboutToBeReset(QAbstractItemModel_hookH handle, QHookH hook)
{
	((QAbstractItemModel_hook *)handle)->hook_modelAboutToBeReset(hook);
}

void QAbstractItemModel_hook_hook_modelReset(QAbstractItemModel_hookH handle, QHookH hook)
{
	((QAbstractItemModel_hook *)handle)->hook_modelReset(hook);
}

void QAbstractItemModel_hook_hook_rowsAboutToBeMoved(QAbstractItemModel_hookH handle, QHookH hook)
{
	((QAbstractItemModel_hook *)handle)->hook_rowsAboutToBeMoved(hook);
}

void QAbstractItemModel_hook_hook_rowsMoved(QAbstractItemModel_hookH handle, QHookH hook)
{
	((QAbstractItemModel_hook *)handle)->hook_rowsMoved(hook);
}

void QAbstractItemModel_hook_hook_columnsAboutToBeMoved(QAbstractItemModel_hookH handle, QHookH hook)
{
	((QAbstractItemModel_hook *)handle)->hook_columnsAboutToBeMoved(hook);
}

void QAbstractItemModel_hook_hook_columnsMoved(QAbstractItemModel_hookH handle, QHookH hook)
{
	((QAbstractItemModel_hook *)handle)->hook_columnsMoved(hook);
}

QAbstractTableModel_hookH QAbstractTableModel_hook_Create(QObjectH handle)
{
	return (QAbstractTableModel_hookH) new QAbstractTableModel_hook((QObject*)handle);
}

void QAbstractTableModel_hook_Destroy(QAbstractTableModel_hookH handle)
{
	delete (QAbstractTableModel_hook *)handle;
}

QAbstractListModel_hookH QAbstractListModel_hook_Create(QObjectH handle)
{
	return (QAbstractListModel_hookH) new QAbstractListModel_hook((QObject*)handle);
}

void QAbstractListModel_hook_Destroy(QAbstractListModel_hookH handle)
{
	delete (QAbstractListModel_hook *)handle;
}

