//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qsessionmanager_c.h"

void QSessionManager_sessionId(QSessionManagerH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QSessionManager *)handle)->sessionId();
	copyQStringToPWideString(t_retval, retval);
}

void QSessionManager_sessionKey(QSessionManagerH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QSessionManager *)handle)->sessionKey();
	copyQStringToPWideString(t_retval, retval);
}

bool QSessionManager_allowsInteraction(QSessionManagerH handle)
{
	return (bool) ((QSessionManager *)handle)->allowsInteraction();
}

bool QSessionManager_allowsErrorInteraction(QSessionManagerH handle)
{
	return (bool) ((QSessionManager *)handle)->allowsErrorInteraction();
}

void QSessionManager_release(QSessionManagerH handle)
{
	((QSessionManager *)handle)->release();
}

void QSessionManager_cancel(QSessionManagerH handle)
{
	((QSessionManager *)handle)->cancel();
}

void QSessionManager_setRestartHint(QSessionManagerH handle, QSessionManager::RestartHint AnonParam1)
{
	((QSessionManager *)handle)->setRestartHint(AnonParam1);
}

QSessionManager::RestartHint QSessionManager_restartHint(QSessionManagerH handle)
{
	return (QSessionManager::RestartHint) ((QSessionManager *)handle)->restartHint();
}

void QSessionManager_setRestartCommand(QSessionManagerH handle, const QStringListH AnonParam1)
{
	((QSessionManager *)handle)->setRestartCommand(*(const QStringList*)AnonParam1);
}

void QSessionManager_restartCommand(QSessionManagerH handle, QStringListH retval)
{
	*(QStringList *)retval = ((QSessionManager *)handle)->restartCommand();
}

void QSessionManager_setDiscardCommand(QSessionManagerH handle, const QStringListH AnonParam1)
{
	((QSessionManager *)handle)->setDiscardCommand(*(const QStringList*)AnonParam1);
}

void QSessionManager_discardCommand(QSessionManagerH handle, QStringListH retval)
{
	*(QStringList *)retval = ((QSessionManager *)handle)->discardCommand();
}

void QSessionManager_setManagerProperty(QSessionManagerH handle, PWideString name, PWideString value)
{
	QString t_name;
	QString t_value;
	copyPWideStringToQString(name, t_name);
	copyPWideStringToQString(value, t_value);
	((QSessionManager *)handle)->setManagerProperty(t_name, t_value);
}

void QSessionManager_setManagerProperty2(QSessionManagerH handle, PWideString name, const QStringListH value)
{
	QString t_name;
	copyPWideStringToQString(name, t_name);
	((QSessionManager *)handle)->setManagerProperty(t_name, *(const QStringList*)value);
}

bool QSessionManager_isPhase2(QSessionManagerH handle)
{
	return (bool) ((QSessionManager *)handle)->isPhase2();
}

void QSessionManager_requestPhase2(QSessionManagerH handle)
{
	((QSessionManager *)handle)->requestPhase2();
}

