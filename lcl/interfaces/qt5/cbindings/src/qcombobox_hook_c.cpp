//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qcombobox_hook_c.h"

QComboBox_hookH QComboBox_hook_Create(QObjectH handle)
{
	return (QComboBox_hookH) new QComboBox_hook((QObject*)handle);
}

void QComboBox_hook_Destroy(QComboBox_hookH handle)
{
	delete (QComboBox_hook *)handle;
}

void QComboBox_hook_hook_editTextChanged(QComboBox_hookH handle, QHookH hook)
{
	((QComboBox_hook *)handle)->hook_editTextChanged(hook);
}

void QComboBox_hook_hook_activated(QComboBox_hookH handle, QHookH hook)
{
	((QComboBox_hook *)handle)->hook_activated(hook);
}

void QComboBox_hook_hook_activated2(QComboBox_hookH handle, QHookH hook)
{
	((QComboBox_hook *)handle)->hook_activated2(hook);
}

void QComboBox_hook_hook_highlighted(QComboBox_hookH handle, QHookH hook)
{
	((QComboBox_hook *)handle)->hook_highlighted(hook);
}

void QComboBox_hook_hook_highlighted2(QComboBox_hookH handle, QHookH hook)
{
	((QComboBox_hook *)handle)->hook_highlighted2(hook);
}

void QComboBox_hook_hook_currentIndexChanged(QComboBox_hookH handle, QHookH hook)
{
	((QComboBox_hook *)handle)->hook_currentIndexChanged(hook);
}

void QComboBox_hook_hook_currentIndexChanged2(QComboBox_hookH handle, QHookH hook)
{
	((QComboBox_hook *)handle)->hook_currentIndexChanged2(hook);
}

void QComboBox_hook_hook_currentTextChanged(QComboBox_hookH handle, QHookH hook)
{
	((QComboBox_hook *)handle)->hook_currentTextChanged(hook);
}

