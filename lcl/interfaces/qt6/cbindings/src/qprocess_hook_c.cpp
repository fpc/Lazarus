//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qprocess_hook_c.h"

QProcessEnvironment_hookH QProcessEnvironment_hook_Create(QObjectH handle)
{
	return (QProcessEnvironment_hookH) new QProcessEnvironment_hook((QObject*)handle);
}

void QProcessEnvironment_hook_Destroy(QProcessEnvironment_hookH handle)
{
	delete (QProcessEnvironment_hook *)handle;
}

QProcess_hookH QProcess_hook_Create(QObjectH handle)
{
	return (QProcess_hookH) new QProcess_hook((QObject*)handle);
}

void QProcess_hook_Destroy(QProcess_hookH handle)
{
	delete (QProcess_hook *)handle;
}

void QProcess_hook_hook_started(QProcess_hookH handle, QHookH hook)
{
	((QProcess_hook *)handle)->hook_started(hook);
}

void QProcess_hook_hook_finished(QProcess_hookH handle, QHookH hook)
{
	((QProcess_hook *)handle)->hook_finished(hook);
}

void QProcess_hook_hook_finished2(QProcess_hookH handle, QHookH hook)
{
	((QProcess_hook *)handle)->hook_finished2(hook);
}

void QProcess_hook_hook_error(QProcess_hookH handle, QHookH hook)
{
	((QProcess_hook *)handle)->hook_error(hook);
}

void QProcess_hook_hook_stateChanged(QProcess_hookH handle, QHookH hook)
{
	((QProcess_hook *)handle)->hook_stateChanged(hook);
}

void QProcess_hook_hook_readyReadStandardOutput(QProcess_hookH handle, QHookH hook)
{
	((QProcess_hook *)handle)->hook_readyReadStandardOutput(hook);
}

void QProcess_hook_hook_readyReadStandardError(QProcess_hookH handle, QHookH hook)
{
	((QProcess_hook *)handle)->hook_readyReadStandardError(hook);
}

