//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QSESSIONMANAGER_C_H
#define QSESSIONMANAGER_C_H

#include <QtGui>
#include "pascalbind.h"

C_EXPORT void QSessionManager_sessionId(QSessionManagerH handle, PWideString retval);
C_EXPORT void QSessionManager_sessionKey(QSessionManagerH handle, PWideString retval);
C_EXPORT bool QSessionManager_allowsInteraction(QSessionManagerH handle);
C_EXPORT bool QSessionManager_allowsErrorInteraction(QSessionManagerH handle);
C_EXPORT void QSessionManager_release(QSessionManagerH handle);
C_EXPORT void QSessionManager_cancel(QSessionManagerH handle);
C_EXPORT void QSessionManager_setRestartHint(QSessionManagerH handle, QSessionManager::RestartHint AnonParam1);
C_EXPORT QSessionManager::RestartHint QSessionManager_restartHint(QSessionManagerH handle);
C_EXPORT void QSessionManager_setRestartCommand(QSessionManagerH handle, const QStringListH AnonParam1);
C_EXPORT void QSessionManager_restartCommand(QSessionManagerH handle, QStringListH retval);
C_EXPORT void QSessionManager_setDiscardCommand(QSessionManagerH handle, const QStringListH AnonParam1);
C_EXPORT void QSessionManager_discardCommand(QSessionManagerH handle, QStringListH retval);
C_EXPORT void QSessionManager_setManagerProperty(QSessionManagerH handle, PWideString name, PWideString value);
C_EXPORT void QSessionManager_setManagerProperty2(QSessionManagerH handle, PWideString name, const QStringListH value);
C_EXPORT bool QSessionManager_isPhase2(QSessionManagerH handle);
C_EXPORT void QSessionManager_requestPhase2(QSessionManagerH handle);

#endif
