//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qitemselectionmodel_hook_c.h"

QItemSelectionRange_hookH QItemSelectionRange_hook_Create(QObjectH handle)
{
	return (QItemSelectionRange_hookH) new QItemSelectionRange_hook((QObject*)handle);
}

void QItemSelectionRange_hook_Destroy(QItemSelectionRange_hookH handle)
{
	delete (QItemSelectionRange_hook *)handle;
}

QItemSelectionModel_hookH QItemSelectionModel_hook_Create(QObjectH handle)
{
	return (QItemSelectionModel_hookH) new QItemSelectionModel_hook((QObject*)handle);
}

void QItemSelectionModel_hook_Destroy(QItemSelectionModel_hookH handle)
{
	delete (QItemSelectionModel_hook *)handle;
}

void QItemSelectionModel_hook_hook_currentChanged(QItemSelectionModel_hookH handle, QHookH hook)
{
	((QItemSelectionModel_hook *)handle)->hook_currentChanged(hook);
}

void QItemSelectionModel_hook_hook_currentRowChanged(QItemSelectionModel_hookH handle, QHookH hook)
{
	((QItemSelectionModel_hook *)handle)->hook_currentRowChanged(hook);
}

void QItemSelectionModel_hook_hook_currentColumnChanged(QItemSelectionModel_hookH handle, QHookH hook)
{
	((QItemSelectionModel_hook *)handle)->hook_currentColumnChanged(hook);
}

