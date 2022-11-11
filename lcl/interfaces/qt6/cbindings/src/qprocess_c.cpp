//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qprocess_c.h"

QProcessEnvironmentH QProcessEnvironment_Create()
{
	return (QProcessEnvironmentH) new QProcessEnvironment();
}

void QProcessEnvironment_Destroy(QProcessEnvironmentH handle)
{
	delete (QProcessEnvironment *)handle;
}

QProcessEnvironmentH QProcessEnvironment_Create2(const QProcessEnvironmentH other)
{
	return (QProcessEnvironmentH) new QProcessEnvironment(*(const QProcessEnvironment*)other);
}

void QProcessEnvironment_swap(QProcessEnvironmentH handle, QProcessEnvironmentH other)
{
	((QProcessEnvironment *)handle)->swap(*(QProcessEnvironment*)other);
}

bool QProcessEnvironment_isEmpty(QProcessEnvironmentH handle)
{
	return (bool) ((QProcessEnvironment *)handle)->isEmpty();
}

void QProcessEnvironment_clear(QProcessEnvironmentH handle)
{
	((QProcessEnvironment *)handle)->clear();
}

bool QProcessEnvironment_contains(QProcessEnvironmentH handle, PWideString name)
{
	QString t_name;
	copyPWideStringToQString(name, t_name);
	return (bool) ((QProcessEnvironment *)handle)->contains(t_name);
}

void QProcessEnvironment_insert(QProcessEnvironmentH handle, PWideString name, PWideString value)
{
	QString t_name;
	QString t_value;
	copyPWideStringToQString(name, t_name);
	copyPWideStringToQString(value, t_value);
	((QProcessEnvironment *)handle)->insert(t_name, t_value);
}

void QProcessEnvironment_remove(QProcessEnvironmentH handle, PWideString name)
{
	QString t_name;
	copyPWideStringToQString(name, t_name);
	((QProcessEnvironment *)handle)->remove(t_name);
}

void QProcessEnvironment_value(QProcessEnvironmentH handle, PWideString retval, PWideString name, PWideString defaultValue)
{
	QString t_retval;
	QString t_name;
	QString t_defaultValue;
	copyPWideStringToQString(name, t_name);
	copyPWideStringToQString(defaultValue, t_defaultValue);
	t_retval = ((QProcessEnvironment *)handle)->value(t_name, t_defaultValue);
	copyQStringToPWideString(t_retval, retval);
}

void QProcessEnvironment_toStringList(QProcessEnvironmentH handle, QStringListH retval)
{
	*(QStringList *)retval = ((QProcessEnvironment *)handle)->toStringList();
}

void QProcessEnvironment_keys(QProcessEnvironmentH handle, QStringListH retval)
{
	*(QStringList *)retval = ((QProcessEnvironment *)handle)->keys();
}

void QProcessEnvironment_insert2(QProcessEnvironmentH handle, const QProcessEnvironmentH e)
{
	((QProcessEnvironment *)handle)->insert(*(const QProcessEnvironment*)e);
}

void QProcessEnvironment_systemEnvironment(QProcessEnvironmentH retval)
{
	*(QProcessEnvironment *)retval = QProcessEnvironment::systemEnvironment();
}

QProcessH QProcess_Create(QObjectH parent)
{
	return (QProcessH) new QProcess((QObject*)parent);
}

void QProcess_Destroy(QProcessH handle)
{
	delete (QProcess *)handle;
}

void QProcess_start(QProcessH handle, PWideString program, const QStringListH arguments, unsigned int mode)
{
	QString t_program;
	copyPWideStringToQString(program, t_program);
	((QProcess *)handle)->start(t_program, *(const QStringList*)arguments, (QIODevice::OpenMode)mode);
}

void QProcess_start3(QProcessH handle, unsigned int mode)
{
	((QProcess *)handle)->start((QIODevice::OpenMode)mode);
}

bool QProcess_open(QProcessH handle, unsigned int mode)
{
	return (bool) ((QProcess *)handle)->open((QIODevice::OpenMode)mode);
}

void QProcess_program(QProcessH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QProcess *)handle)->program();
	copyQStringToPWideString(t_retval, retval);
}

void QProcess_setProgram(QProcessH handle, PWideString program)
{
	QString t_program;
	copyPWideStringToQString(program, t_program);
	((QProcess *)handle)->setProgram(t_program);
}

void QProcess_arguments(QProcessH handle, QStringListH retval)
{
	*(QStringList *)retval = ((QProcess *)handle)->arguments();
}

void QProcess_setArguments(QProcessH handle, const QStringListH arguments)
{
	((QProcess *)handle)->setArguments(*(const QStringList*)arguments);
}

QProcess::ProcessChannelMode QProcess_processChannelMode(QProcessH handle)
{
	return (QProcess::ProcessChannelMode) ((QProcess *)handle)->processChannelMode();
}

void QProcess_setProcessChannelMode(QProcessH handle, QProcess::ProcessChannelMode mode)
{
	((QProcess *)handle)->setProcessChannelMode(mode);
}

QProcess::ProcessChannel QProcess_readChannel(QProcessH handle)
{
	return (QProcess::ProcessChannel) ((QProcess *)handle)->readChannel();
}

void QProcess_setReadChannel(QProcessH handle, QProcess::ProcessChannel channel)
{
	((QProcess *)handle)->setReadChannel(channel);
}

void QProcess_closeReadChannel(QProcessH handle, QProcess::ProcessChannel channel)
{
	((QProcess *)handle)->closeReadChannel(channel);
}

void QProcess_closeWriteChannel(QProcessH handle)
{
	((QProcess *)handle)->closeWriteChannel();
}

void QProcess_setStandardInputFile(QProcessH handle, PWideString fileName)
{
	QString t_fileName;
	copyPWideStringToQString(fileName, t_fileName);
	((QProcess *)handle)->setStandardInputFile(t_fileName);
}

void QProcess_setStandardOutputFile(QProcessH handle, PWideString fileName, unsigned int mode)
{
	QString t_fileName;
	copyPWideStringToQString(fileName, t_fileName);
	((QProcess *)handle)->setStandardOutputFile(t_fileName, (QIODevice::OpenMode)mode);
}

void QProcess_setStandardErrorFile(QProcessH handle, PWideString fileName, unsigned int mode)
{
	QString t_fileName;
	copyPWideStringToQString(fileName, t_fileName);
	((QProcess *)handle)->setStandardErrorFile(t_fileName, (QIODevice::OpenMode)mode);
}

void QProcess_setStandardOutputProcess(QProcessH handle, QProcessH destination)
{
	((QProcess *)handle)->setStandardOutputProcess((QProcess*)destination);
}

void QProcess_workingDirectory(QProcessH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QProcess *)handle)->workingDirectory();
	copyQStringToPWideString(t_retval, retval);
}

void QProcess_setWorkingDirectory(QProcessH handle, PWideString dir)
{
	QString t_dir;
	copyPWideStringToQString(dir, t_dir);
	((QProcess *)handle)->setWorkingDirectory(t_dir);
}

void QProcess_setEnvironment(QProcessH handle, const QStringListH environment)
{
	((QProcess *)handle)->setEnvironment(*(const QStringList*)environment);
}

void QProcess_environment(QProcessH handle, QStringListH retval)
{
	*(QStringList *)retval = ((QProcess *)handle)->environment();
}

void QProcess_setProcessEnvironment(QProcessH handle, const QProcessEnvironmentH environment)
{
	((QProcess *)handle)->setProcessEnvironment(*(const QProcessEnvironment*)environment);
}

void QProcess_processEnvironment(QProcessH handle, QProcessEnvironmentH retval)
{
	*(QProcessEnvironment *)retval = ((QProcess *)handle)->processEnvironment();
}

QProcess::ProcessError QProcess_error(QProcessH handle)
{
	return (QProcess::ProcessError) ((QProcess *)handle)->error();
}

QProcess::ProcessState QProcess_state(QProcessH handle)
{
	return (QProcess::ProcessState) ((QProcess *)handle)->state();
}

qint64 QProcess_processId(QProcessH handle)
{
	return (qint64) ((QProcess *)handle)->processId();
}

bool QProcess_waitForStarted(QProcessH handle, int msecs)
{
	return (bool) ((QProcess *)handle)->waitForStarted(msecs);
}

bool QProcess_waitForReadyRead(QProcessH handle, int msecs)
{
	return (bool) ((QProcess *)handle)->waitForReadyRead(msecs);
}

bool QProcess_waitForBytesWritten(QProcessH handle, int msecs)
{
	return (bool) ((QProcess *)handle)->waitForBytesWritten(msecs);
}

bool QProcess_waitForFinished(QProcessH handle, int msecs)
{
	return (bool) ((QProcess *)handle)->waitForFinished(msecs);
}

void QProcess_readAllStandardOutput(QProcessH handle, QByteArrayH retval)
{
	*(QByteArray *)retval = ((QProcess *)handle)->readAllStandardOutput();
}

void QProcess_readAllStandardError(QProcessH handle, QByteArrayH retval)
{
	*(QByteArray *)retval = ((QProcess *)handle)->readAllStandardError();
}

int QProcess_exitCode(QProcessH handle)
{
	return (int) ((QProcess *)handle)->exitCode();
}

QProcess::ExitStatus QProcess_exitStatus(QProcessH handle)
{
	return (QProcess::ExitStatus) ((QProcess *)handle)->exitStatus();
}

qint64 QProcess_bytesAvailable(QProcessH handle)
{
	return (qint64) ((QProcess *)handle)->bytesAvailable();
}

qint64 QProcess_bytesToWrite(QProcessH handle)
{
	return (qint64) ((QProcess *)handle)->bytesToWrite();
}

bool QProcess_isSequential(QProcessH handle)
{
	return (bool) ((QProcess *)handle)->isSequential();
}

bool QProcess_canReadLine(QProcessH handle)
{
	return (bool) ((QProcess *)handle)->canReadLine();
}

void QProcess_close(QProcessH handle)
{
	((QProcess *)handle)->close();
}

bool QProcess_atEnd(QProcessH handle)
{
	return (bool) ((QProcess *)handle)->atEnd();
}

int QProcess_execute(PWideString program, const QStringListH arguments)
{
	QString t_program;
	copyPWideStringToQString(program, t_program);
	return (int) QProcess::execute(t_program, *(const QStringList*)arguments);
}

int QProcess_execute2(PWideString program)
{
	QString t_program;
	copyPWideStringToQString(program, t_program);
	return (int) QProcess::execute(t_program);
}

bool QProcess_startDetached(PWideString program, const QStringListH arguments, PWideString workingDirectory, qint64* pid)
{
	QString t_program;
	QString t_workingDirectory;
	copyPWideStringToQString(program, t_program);
	copyPWideStringToQString(workingDirectory, t_workingDirectory);
	return (bool) QProcess::startDetached(t_program, *(const QStringList*)arguments, t_workingDirectory, pid);
}

bool QProcess_startDetached2(PWideString program, const QStringListH arguments)
{
	QString t_program;
	copyPWideStringToQString(program, t_program);
	return (bool) QProcess::startDetached(t_program, *(const QStringList*)arguments);
}

bool QProcess_startDetached3(PWideString program)
{
	QString t_program;
	copyPWideStringToQString(program, t_program);
	return (bool) QProcess::startDetached(t_program);
}

void QProcess_systemEnvironment(QStringListH retval)
{
	*(QStringList *)retval = QProcess::systemEnvironment();
}

void QProcess_terminate(QProcessH handle)
{
	((QProcess *)handle)->terminate();
}

void QProcess_kill(QProcessH handle)
{
	((QProcess *)handle)->kill();
}

#if defined MSWINDOWS
void QProcess_nativeArguments(QProcessH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QProcess *)handle)->nativeArguments();
	copyQStringToPWideString(t_retval, retval);
}

void QProcess_setNativeArguments(QProcessH handle, PWideString arguments)
{
	QString t_arguments;
	copyPWideStringToQString(arguments, t_arguments);
	((QProcess *)handle)->setNativeArguments(t_arguments);
}

#endif
