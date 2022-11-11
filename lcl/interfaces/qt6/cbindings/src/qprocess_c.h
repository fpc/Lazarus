//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QPROCESS_C_H
#define QPROCESS_C_H

#include <QtCore>
#include "pascalbind.h"

C_EXPORT QProcessEnvironmentH QProcessEnvironment_Create();
C_EXPORT void QProcessEnvironment_Destroy(QProcessEnvironmentH handle);
C_EXPORT QProcessEnvironmentH QProcessEnvironment_Create2(const QProcessEnvironmentH other);
C_EXPORT void QProcessEnvironment_swap(QProcessEnvironmentH handle, QProcessEnvironmentH other);
C_EXPORT bool QProcessEnvironment_isEmpty(QProcessEnvironmentH handle);
C_EXPORT void QProcessEnvironment_clear(QProcessEnvironmentH handle);
C_EXPORT bool QProcessEnvironment_contains(QProcessEnvironmentH handle, PWideString name);
C_EXPORT void QProcessEnvironment_insert(QProcessEnvironmentH handle, PWideString name, PWideString value);
C_EXPORT void QProcessEnvironment_remove(QProcessEnvironmentH handle, PWideString name);
C_EXPORT void QProcessEnvironment_value(QProcessEnvironmentH handle, PWideString retval, PWideString name, PWideString defaultValue);
C_EXPORT void QProcessEnvironment_toStringList(QProcessEnvironmentH handle, QStringListH retval);
C_EXPORT void QProcessEnvironment_keys(QProcessEnvironmentH handle, QStringListH retval);
C_EXPORT void QProcessEnvironment_insert2(QProcessEnvironmentH handle, const QProcessEnvironmentH e);
C_EXPORT void QProcessEnvironment_systemEnvironment(QProcessEnvironmentH retval);
C_EXPORT QProcessH QProcess_Create(QObjectH parent);
C_EXPORT void QProcess_Destroy(QProcessH handle);
C_EXPORT void QProcess_start(QProcessH handle, PWideString program, const QStringListH arguments, unsigned int mode);
C_EXPORT void QProcess_start3(QProcessH handle, unsigned int mode);
C_EXPORT bool QProcess_open(QProcessH handle, unsigned int mode);
C_EXPORT void QProcess_program(QProcessH handle, PWideString retval);
C_EXPORT void QProcess_setProgram(QProcessH handle, PWideString program);
C_EXPORT void QProcess_arguments(QProcessH handle, QStringListH retval);
C_EXPORT void QProcess_setArguments(QProcessH handle, const QStringListH arguments);
C_EXPORT QProcess::ProcessChannelMode QProcess_processChannelMode(QProcessH handle);
C_EXPORT void QProcess_setProcessChannelMode(QProcessH handle, QProcess::ProcessChannelMode mode);
C_EXPORT QProcess::ProcessChannel QProcess_readChannel(QProcessH handle);
C_EXPORT void QProcess_setReadChannel(QProcessH handle, QProcess::ProcessChannel channel);
C_EXPORT void QProcess_closeReadChannel(QProcessH handle, QProcess::ProcessChannel channel);
C_EXPORT void QProcess_closeWriteChannel(QProcessH handle);
C_EXPORT void QProcess_setStandardInputFile(QProcessH handle, PWideString fileName);
C_EXPORT void QProcess_setStandardOutputFile(QProcessH handle, PWideString fileName, unsigned int mode);
C_EXPORT void QProcess_setStandardErrorFile(QProcessH handle, PWideString fileName, unsigned int mode);
C_EXPORT void QProcess_setStandardOutputProcess(QProcessH handle, QProcessH destination);
C_EXPORT void QProcess_workingDirectory(QProcessH handle, PWideString retval);
C_EXPORT void QProcess_setWorkingDirectory(QProcessH handle, PWideString dir);
C_EXPORT void QProcess_setEnvironment(QProcessH handle, const QStringListH environment);
C_EXPORT void QProcess_environment(QProcessH handle, QStringListH retval);
C_EXPORT void QProcess_setProcessEnvironment(QProcessH handle, const QProcessEnvironmentH environment);
C_EXPORT void QProcess_processEnvironment(QProcessH handle, QProcessEnvironmentH retval);
C_EXPORT QProcess::ProcessError QProcess_error(QProcessH handle);
C_EXPORT QProcess::ProcessState QProcess_state(QProcessH handle);
C_EXPORT qint64 QProcess_processId(QProcessH handle);
C_EXPORT bool QProcess_waitForStarted(QProcessH handle, int msecs);
C_EXPORT bool QProcess_waitForReadyRead(QProcessH handle, int msecs);
C_EXPORT bool QProcess_waitForBytesWritten(QProcessH handle, int msecs);
C_EXPORT bool QProcess_waitForFinished(QProcessH handle, int msecs);
C_EXPORT void QProcess_readAllStandardOutput(QProcessH handle, QByteArrayH retval);
C_EXPORT void QProcess_readAllStandardError(QProcessH handle, QByteArrayH retval);
C_EXPORT int QProcess_exitCode(QProcessH handle);
C_EXPORT QProcess::ExitStatus QProcess_exitStatus(QProcessH handle);
C_EXPORT qint64 QProcess_bytesAvailable(QProcessH handle);
C_EXPORT qint64 QProcess_bytesToWrite(QProcessH handle);
C_EXPORT bool QProcess_isSequential(QProcessH handle);
C_EXPORT bool QProcess_canReadLine(QProcessH handle);
C_EXPORT void QProcess_close(QProcessH handle);
C_EXPORT bool QProcess_atEnd(QProcessH handle);
C_EXPORT int QProcess_execute(PWideString program, const QStringListH arguments);
C_EXPORT int QProcess_execute2(PWideString program);
C_EXPORT bool QProcess_startDetached(PWideString program, const QStringListH arguments, PWideString workingDirectory, qint64* pid);
C_EXPORT bool QProcess_startDetached2(PWideString program, const QStringListH arguments);
C_EXPORT bool QProcess_startDetached3(PWideString program);
C_EXPORT void QProcess_systemEnvironment(QStringListH retval);
C_EXPORT void QProcess_terminate(QProcessH handle);
C_EXPORT void QProcess_kill(QProcessH handle);
#if defined MSWINDOWS
C_EXPORT void QProcess_nativeArguments(QProcessH handle, PWideString retval);
C_EXPORT void QProcess_setNativeArguments(QProcessH handle, PWideString arguments);
#endif

#endif
