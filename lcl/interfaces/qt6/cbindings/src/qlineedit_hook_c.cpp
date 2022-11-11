//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qlineedit_hook_c.h"

QLineEdit_hookH QLineEdit_hook_Create(QObjectH handle)
{
	return (QLineEdit_hookH) new QLineEdit_hook((QObject*)handle);
}

void QLineEdit_hook_Destroy(QLineEdit_hookH handle)
{
	delete (QLineEdit_hook *)handle;
}

void QLineEdit_hook_hook_textChanged(QLineEdit_hookH handle, QHookH hook)
{
	((QLineEdit_hook *)handle)->hook_textChanged(hook);
}

void QLineEdit_hook_hook_textEdited(QLineEdit_hookH handle, QHookH hook)
{
	((QLineEdit_hook *)handle)->hook_textEdited(hook);
}

void QLineEdit_hook_hook_cursorPositionChanged(QLineEdit_hookH handle, QHookH hook)
{
	((QLineEdit_hook *)handle)->hook_cursorPositionChanged(hook);
}

void QLineEdit_hook_hook_inputRejected(QLineEdit_hookH handle, QHookH hook)
{
	((QLineEdit_hook *)handle)->hook_inputRejected(hook);
}

void QLineEdit_hook_hook_returnPressed(QLineEdit_hookH handle, QHookH hook)
{
	((QLineEdit_hook *)handle)->hook_returnPressed(hook);
}

void QLineEdit_hook_hook_editingFinished(QLineEdit_hookH handle, QHookH hook)
{
	((QLineEdit_hook *)handle)->hook_editingFinished(hook);
}

void QLineEdit_hook_hook_selectionChanged(QLineEdit_hookH handle, QHookH hook)
{
	((QLineEdit_hook *)handle)->hook_selectionChanged(hook);
}

