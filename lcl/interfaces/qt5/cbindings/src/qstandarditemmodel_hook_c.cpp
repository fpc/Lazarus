//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qstandarditemmodel_hook_c.h"

QStandardItem_hookH QStandardItem_hook_Create(QObjectH handle)
{
	return (QStandardItem_hookH) new QStandardItem_hook((QObject*)handle);
}

void QStandardItem_hook_Destroy(QStandardItem_hookH handle)
{
	delete (QStandardItem_hook *)handle;
}

QStandardItemModel_hookH QStandardItemModel_hook_Create(QObjectH handle)
{
	return (QStandardItemModel_hookH) new QStandardItemModel_hook((QObject*)handle);
}

void QStandardItemModel_hook_Destroy(QStandardItemModel_hookH handle)
{
	delete (QStandardItemModel_hook *)handle;
}

void QStandardItemModel_hook_hook_itemChanged(QStandardItemModel_hookH handle, QHookH hook)
{
	((QStandardItemModel_hook *)handle)->hook_itemChanged(hook);
}

