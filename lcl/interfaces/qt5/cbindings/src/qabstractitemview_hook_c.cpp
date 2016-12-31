//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qabstractitemview_hook_c.h"

QAbstractItemView_hookH QAbstractItemView_hook_Create(QObjectH handle)
{
	return (QAbstractItemView_hookH) new QAbstractItemView_hook((QObject*)handle);
}

void QAbstractItemView_hook_Destroy(QAbstractItemView_hookH handle)
{
	delete (QAbstractItemView_hook *)handle;
}

void QAbstractItemView_hook_hook_pressed(QAbstractItemView_hookH handle, QHookH hook)
{
	((QAbstractItemView_hook *)handle)->hook_pressed(hook);
}

void QAbstractItemView_hook_hook_clicked(QAbstractItemView_hookH handle, QHookH hook)
{
	((QAbstractItemView_hook *)handle)->hook_clicked(hook);
}

void QAbstractItemView_hook_hook_doubleClicked(QAbstractItemView_hookH handle, QHookH hook)
{
	((QAbstractItemView_hook *)handle)->hook_doubleClicked(hook);
}

void QAbstractItemView_hook_hook_activated(QAbstractItemView_hookH handle, QHookH hook)
{
	((QAbstractItemView_hook *)handle)->hook_activated(hook);
}

void QAbstractItemView_hook_hook_entered(QAbstractItemView_hookH handle, QHookH hook)
{
	((QAbstractItemView_hook *)handle)->hook_entered(hook);
}

void QAbstractItemView_hook_hook_viewportEntered(QAbstractItemView_hookH handle, QHookH hook)
{
	((QAbstractItemView_hook *)handle)->hook_viewportEntered(hook);
}

