//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qplaintextedit_hook_c.h"

QPlainTextEdit_hookH QPlainTextEdit_hook_Create(QObjectH handle)
{
	return (QPlainTextEdit_hookH) new QPlainTextEdit_hook((QObject*)handle);
}

void QPlainTextEdit_hook_Destroy(QPlainTextEdit_hookH handle)
{
	delete (QPlainTextEdit_hook *)handle;
}

void QPlainTextEdit_hook_hook_textChanged(QPlainTextEdit_hookH handle, QHookH hook)
{
	((QPlainTextEdit_hook *)handle)->hook_textChanged(hook);
}

void QPlainTextEdit_hook_hook_undoAvailable(QPlainTextEdit_hookH handle, QHookH hook)
{
	((QPlainTextEdit_hook *)handle)->hook_undoAvailable(hook);
}

void QPlainTextEdit_hook_hook_redoAvailable(QPlainTextEdit_hookH handle, QHookH hook)
{
	((QPlainTextEdit_hook *)handle)->hook_redoAvailable(hook);
}

void QPlainTextEdit_hook_hook_copyAvailable(QPlainTextEdit_hookH handle, QHookH hook)
{
	((QPlainTextEdit_hook *)handle)->hook_copyAvailable(hook);
}

void QPlainTextEdit_hook_hook_selectionChanged(QPlainTextEdit_hookH handle, QHookH hook)
{
	((QPlainTextEdit_hook *)handle)->hook_selectionChanged(hook);
}

void QPlainTextEdit_hook_hook_cursorPositionChanged(QPlainTextEdit_hookH handle, QHookH hook)
{
	((QPlainTextEdit_hook *)handle)->hook_cursorPositionChanged(hook);
}

void QPlainTextEdit_hook_hook_updateRequest(QPlainTextEdit_hookH handle, QHookH hook)
{
	((QPlainTextEdit_hook *)handle)->hook_updateRequest(hook);
}

void QPlainTextEdit_hook_hook_blockCountChanged(QPlainTextEdit_hookH handle, QHookH hook)
{
	((QPlainTextEdit_hook *)handle)->hook_blockCountChanged(hook);
}

void QPlainTextEdit_hook_hook_modificationChanged(QPlainTextEdit_hookH handle, QHookH hook)
{
	((QPlainTextEdit_hook *)handle)->hook_modificationChanged(hook);
}

QPlainTextDocumentLayout_hookH QPlainTextDocumentLayout_hook_Create(QObjectH handle)
{
	return (QPlainTextDocumentLayout_hookH) new QPlainTextDocumentLayout_hook((QObject*)handle);
}

void QPlainTextDocumentLayout_hook_Destroy(QPlainTextDocumentLayout_hookH handle)
{
	delete (QPlainTextDocumentLayout_hook *)handle;
}

