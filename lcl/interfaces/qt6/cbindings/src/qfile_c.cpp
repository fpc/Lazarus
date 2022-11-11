//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qfile_c.h"

QFileH QFile_Create()
{
	return (QFileH) new QFile();
}

void QFile_Destroy(QFileH handle)
{
	delete (QFile *)handle;
}

QFileH QFile_Create2(PWideString name)
{
	QString t_name;
	copyPWideStringToQString(name, t_name);
	return (QFileH) new QFile(t_name);
}

QFileH QFile_Create3(QObjectH parent)
{
	return (QFileH) new QFile((QObject*)parent);
}

QFileH QFile_Create4(PWideString name, QObjectH parent)
{
	QString t_name;
	copyPWideStringToQString(name, t_name);
	return (QFileH) new QFile(t_name, (QObject*)parent);
}

void QFile_fileName(QFileH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QFile *)handle)->fileName();
	copyQStringToPWideString(t_retval, retval);
}

void QFile_setFileName(QFileH handle, PWideString name)
{
	QString t_name;
	copyPWideStringToQString(name, t_name);
	((QFile *)handle)->setFileName(t_name);
}

void QFile_encodeName(QByteArrayH retval, PWideString fileName)
{
	QString t_fileName;
	copyPWideStringToQString(fileName, t_fileName);
	*(QByteArray *)retval = QFile::encodeName(t_fileName);
}

void QFile_decodeName(PWideString retval, const QByteArrayH localFileName)
{
	QString t_retval;
	t_retval = QFile::decodeName(*(const QByteArray*)localFileName);
	copyQStringToPWideString(t_retval, retval);
}

void QFile_decodeName2(PWideString retval, const char* localFileName)
{
	QString t_retval;
	t_retval = QFile::decodeName(localFileName);
	copyQStringToPWideString(t_retval, retval);
}

bool QFile_exists(QFileH handle)
{
	return (bool) ((QFile *)handle)->exists();
}

bool QFile_exists2(PWideString fileName)
{
	QString t_fileName;
	copyPWideStringToQString(fileName, t_fileName);
	return (bool) QFile::exists(t_fileName);
}

void QFile_symLinkTarget(QFileH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QFile *)handle)->symLinkTarget();
	copyQStringToPWideString(t_retval, retval);
}

void QFile_symLinkTarget2(PWideString retval, PWideString fileName)
{
	QString t_retval;
	QString t_fileName;
	copyPWideStringToQString(fileName, t_fileName);
	t_retval = QFile::symLinkTarget(t_fileName);
	copyQStringToPWideString(t_retval, retval);
}

bool QFile_remove(QFileH handle)
{
	return (bool) ((QFile *)handle)->remove();
}

bool QFile_remove2(PWideString fileName)
{
	QString t_fileName;
	copyPWideStringToQString(fileName, t_fileName);
	return (bool) QFile::remove(t_fileName);
}

bool QFile_rename(QFileH handle, PWideString newName)
{
	QString t_newName;
	copyPWideStringToQString(newName, t_newName);
	return (bool) ((QFile *)handle)->rename(t_newName);
}

bool QFile_rename2(PWideString oldName, PWideString newName)
{
	QString t_oldName;
	QString t_newName;
	copyPWideStringToQString(oldName, t_oldName);
	copyPWideStringToQString(newName, t_newName);
	return (bool) QFile::rename(t_oldName, t_newName);
}

bool QFile_link(QFileH handle, PWideString newName)
{
	QString t_newName;
	copyPWideStringToQString(newName, t_newName);
	return (bool) ((QFile *)handle)->link(t_newName);
}

bool QFile_link2(PWideString oldname, PWideString newName)
{
	QString t_oldname;
	QString t_newName;
	copyPWideStringToQString(oldname, t_oldname);
	copyPWideStringToQString(newName, t_newName);
	return (bool) QFile::link(t_oldname, t_newName);
}

bool QFile_copy(QFileH handle, PWideString newName)
{
	QString t_newName;
	copyPWideStringToQString(newName, t_newName);
	return (bool) ((QFile *)handle)->copy(t_newName);
}

bool QFile_copy2(PWideString fileName, PWideString newName)
{
	QString t_fileName;
	QString t_newName;
	copyPWideStringToQString(fileName, t_fileName);
	copyPWideStringToQString(newName, t_newName);
	return (bool) QFile::copy(t_fileName, t_newName);
}

bool QFile_open(QFileH handle, unsigned int flags)
{
	return (bool) ((QFile *)handle)->open((QIODevice::OpenMode)flags);
}

bool QFile_open2(QFileH handle, int fd, unsigned int ioFlags, unsigned int handleFlags)
{
	return (bool) ((QFile *)handle)->open(fd, (QIODevice::OpenMode)ioFlags, (QFileDevice::FileHandleFlags)handleFlags);
}

qint64 QFile_size(QFileH handle)
{
	return (qint64) ((QFile *)handle)->size();
}

bool QFile_resize(QFileH handle, qint64 sz)
{
	return (bool) ((QFile *)handle)->resize(sz);
}

bool QFile_resize2(PWideString filename, qint64 sz)
{
	QString t_filename;
	copyPWideStringToQString(filename, t_filename);
	return (bool) QFile::resize(t_filename, sz);
}

unsigned int QFile_permissions(QFileH handle)
{
	return (unsigned int) ((QFile *)handle)->permissions();
}

unsigned int QFile_permissions2(PWideString filename)
{
	QString t_filename;
	copyPWideStringToQString(filename, t_filename);
	return (unsigned int) QFile::permissions(t_filename);
}

bool QFile_setPermissions(QFileH handle, unsigned int permissionSpec)
{
	return (bool) ((QFile *)handle)->setPermissions((QFileDevice::Permissions)permissionSpec);
}

bool QFile_setPermissions2(PWideString filename, unsigned int permissionSpec)
{
	QString t_filename;
	copyPWideStringToQString(filename, t_filename);
	return (bool) QFile::setPermissions(t_filename, (QFileDevice::Permissions)permissionSpec);
}

