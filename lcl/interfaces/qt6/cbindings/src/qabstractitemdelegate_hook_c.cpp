//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qabstractitemdelegate_hook_c.h"

QAbstractItemDelegate_hookH QAbstractItemDelegate_hook_Create(QObjectH handle)
{
	return (QAbstractItemDelegate_hookH) new QAbstractItemDelegate_hook((QObject*)handle);
}

void QAbstractItemDelegate_hook_Destroy(QAbstractItemDelegate_hookH handle)
{
	delete (QAbstractItemDelegate_hook *)handle;
}

void QAbstractItemDelegate_hook_hook_commitData(QAbstractItemDelegate_hookH handle, QHookH hook)
{
	((QAbstractItemDelegate_hook *)handle)->hook_commitData(hook);
}

void QAbstractItemDelegate_hook_hook_closeEditor(QAbstractItemDelegate_hookH handle, QHookH hook)
{
	((QAbstractItemDelegate_hook *)handle)->hook_closeEditor(hook);
}

void QAbstractItemDelegate_hook_hook_closeEditor2(QAbstractItemDelegate_hookH handle, QHookH hook)
{
	((QAbstractItemDelegate_hook *)handle)->hook_closeEditor2(hook);
}

void QAbstractItemDelegate_hook_hook_sizeHintChanged(QAbstractItemDelegate_hookH handle, QHookH hook)
{
	((QAbstractItemDelegate_hook *)handle)->hook_sizeHintChanged(hook);
}

