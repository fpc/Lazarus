//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qtextedit_hook_c.h"

QTextEdit_hookH QTextEdit_hook_Create(QObjectH handle)
{
	return (QTextEdit_hookH) new QTextEdit_hook((QObject*)handle);
}

void QTextEdit_hook_Destroy(QTextEdit_hookH handle)
{
	delete (QTextEdit_hook *)handle;
}

void QTextEdit_hook_hook_textChanged(QTextEdit_hookH handle, QHookH hook)
{
	((QTextEdit_hook *)handle)->hook_textChanged(hook);
}

void QTextEdit_hook_hook_undoAvailable(QTextEdit_hookH handle, QHookH hook)
{
	((QTextEdit_hook *)handle)->hook_undoAvailable(hook);
}

void QTextEdit_hook_hook_redoAvailable(QTextEdit_hookH handle, QHookH hook)
{
	((QTextEdit_hook *)handle)->hook_redoAvailable(hook);
}

void QTextEdit_hook_hook_currentCharFormatChanged(QTextEdit_hookH handle, QHookH hook)
{
	((QTextEdit_hook *)handle)->hook_currentCharFormatChanged(hook);
}

void QTextEdit_hook_hook_copyAvailable(QTextEdit_hookH handle, QHookH hook)
{
	((QTextEdit_hook *)handle)->hook_copyAvailable(hook);
}

void QTextEdit_hook_hook_selectionChanged(QTextEdit_hookH handle, QHookH hook)
{
	((QTextEdit_hook *)handle)->hook_selectionChanged(hook);
}

void QTextEdit_hook_hook_cursorPositionChanged(QTextEdit_hookH handle, QHookH hook)
{
	((QTextEdit_hook *)handle)->hook_cursorPositionChanged(hook);
}

