//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qfileinfo_c.h"

QFileInfoH QFileInfo_Create()
{
	return (QFileInfoH) new QFileInfo();
}

void QFileInfo_Destroy(QFileInfoH handle)
{
	delete (QFileInfo *)handle;
}

QFileInfoH QFileInfo_Create2(PWideString file)
{
	QString t_file;
	copyPWideStringToQString(file, t_file);
	return (QFileInfoH) new QFileInfo(t_file);
}

QFileInfoH QFileInfo_Create3(const QFileH file)
{
	return (QFileInfoH) new QFileInfo(*(const QFile*)file);
}

QFileInfoH QFileInfo_Create4(const QDirH dir, PWideString file)
{
	QString t_file;
	copyPWideStringToQString(file, t_file);
	return (QFileInfoH) new QFileInfo(*(const QDir*)dir, t_file);
}

QFileInfoH QFileInfo_Create5(const QFileInfoH fileinfo)
{
	return (QFileInfoH) new QFileInfo(*(const QFileInfo*)fileinfo);
}

void QFileInfo_swap(QFileInfoH handle, QFileInfoH other)
{
	((QFileInfo *)handle)->swap(*(QFileInfo*)other);
}

void QFileInfo_setFile(QFileInfoH handle, PWideString file)
{
	QString t_file;
	copyPWideStringToQString(file, t_file);
	((QFileInfo *)handle)->setFile(t_file);
}

void QFileInfo_setFile2(QFileInfoH handle, const QFileH file)
{
	((QFileInfo *)handle)->setFile(*(const QFile*)file);
}

void QFileInfo_setFile3(QFileInfoH handle, const QDirH dir, PWideString file)
{
	QString t_file;
	copyPWideStringToQString(file, t_file);
	((QFileInfo *)handle)->setFile(*(const QDir*)dir, t_file);
}

bool QFileInfo_exists(QFileInfoH handle)
{
	return (bool) ((QFileInfo *)handle)->exists();
}

void QFileInfo_refresh(QFileInfoH handle)
{
	((QFileInfo *)handle)->refresh();
}

void QFileInfo_filePath(QFileInfoH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QFileInfo *)handle)->filePath();
	copyQStringToPWideString(t_retval, retval);
}

void QFileInfo_absoluteFilePath(QFileInfoH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QFileInfo *)handle)->absoluteFilePath();
	copyQStringToPWideString(t_retval, retval);
}

void QFileInfo_canonicalFilePath(QFileInfoH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QFileInfo *)handle)->canonicalFilePath();
	copyQStringToPWideString(t_retval, retval);
}

void QFileInfo_fileName(QFileInfoH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QFileInfo *)handle)->fileName();
	copyQStringToPWideString(t_retval, retval);
}

void QFileInfo_baseName(QFileInfoH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QFileInfo *)handle)->baseName();
	copyQStringToPWideString(t_retval, retval);
}

void QFileInfo_completeBaseName(QFileInfoH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QFileInfo *)handle)->completeBaseName();
	copyQStringToPWideString(t_retval, retval);
}

void QFileInfo_suffix(QFileInfoH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QFileInfo *)handle)->suffix();
	copyQStringToPWideString(t_retval, retval);
}

void QFileInfo_bundleName(QFileInfoH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QFileInfo *)handle)->bundleName();
	copyQStringToPWideString(t_retval, retval);
}

void QFileInfo_completeSuffix(QFileInfoH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QFileInfo *)handle)->completeSuffix();
	copyQStringToPWideString(t_retval, retval);
}

void QFileInfo_path(QFileInfoH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QFileInfo *)handle)->path();
	copyQStringToPWideString(t_retval, retval);
}

void QFileInfo_absolutePath(QFileInfoH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QFileInfo *)handle)->absolutePath();
	copyQStringToPWideString(t_retval, retval);
}

void QFileInfo_canonicalPath(QFileInfoH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QFileInfo *)handle)->canonicalPath();
	copyQStringToPWideString(t_retval, retval);
}

void QFileInfo_dir(QFileInfoH handle, QDirH retval)
{
	*(QDir *)retval = ((QFileInfo *)handle)->dir();
}

void QFileInfo_absoluteDir(QFileInfoH handle, QDirH retval)
{
	*(QDir *)retval = ((QFileInfo *)handle)->absoluteDir();
}

bool QFileInfo_isReadable(QFileInfoH handle)
{
	return (bool) ((QFileInfo *)handle)->isReadable();
}

bool QFileInfo_isWritable(QFileInfoH handle)
{
	return (bool) ((QFileInfo *)handle)->isWritable();
}

bool QFileInfo_isExecutable(QFileInfoH handle)
{
	return (bool) ((QFileInfo *)handle)->isExecutable();
}

bool QFileInfo_isHidden(QFileInfoH handle)
{
	return (bool) ((QFileInfo *)handle)->isHidden();
}

bool QFileInfo_isNativePath(QFileInfoH handle)
{
	return (bool) ((QFileInfo *)handle)->isNativePath();
}

bool QFileInfo_isRelative(QFileInfoH handle)
{
	return (bool) ((QFileInfo *)handle)->isRelative();
}

bool QFileInfo_isAbsolute(QFileInfoH handle)
{
	return (bool) ((QFileInfo *)handle)->isAbsolute();
}

bool QFileInfo_makeAbsolute(QFileInfoH handle)
{
	return (bool) ((QFileInfo *)handle)->makeAbsolute();
}

bool QFileInfo_isFile(QFileInfoH handle)
{
	return (bool) ((QFileInfo *)handle)->isFile();
}

bool QFileInfo_isDir(QFileInfoH handle)
{
	return (bool) ((QFileInfo *)handle)->isDir();
}

bool QFileInfo_isSymLink(QFileInfoH handle)
{
	return (bool) ((QFileInfo *)handle)->isSymLink();
}

bool QFileInfo_isRoot(QFileInfoH handle)
{
	return (bool) ((QFileInfo *)handle)->isRoot();
}

bool QFileInfo_isBundle(QFileInfoH handle)
{
	return (bool) ((QFileInfo *)handle)->isBundle();
}

void QFileInfo_symLinkTarget(QFileInfoH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QFileInfo *)handle)->symLinkTarget();
	copyQStringToPWideString(t_retval, retval);
}

void QFileInfo_owner(QFileInfoH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QFileInfo *)handle)->owner();
	copyQStringToPWideString(t_retval, retval);
}

uint QFileInfo_ownerId(QFileInfoH handle)
{
	return (uint) ((QFileInfo *)handle)->ownerId();
}

void QFileInfo_group(QFileInfoH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QFileInfo *)handle)->group();
	copyQStringToPWideString(t_retval, retval);
}

uint QFileInfo_groupId(QFileInfoH handle)
{
	return (uint) ((QFileInfo *)handle)->groupId();
}

bool QFileInfo_permission(QFileInfoH handle, unsigned int permissions)
{
	return (bool) ((QFileInfo *)handle)->permission((QFileDevice::Permissions)permissions);
}

unsigned int QFileInfo_permissions(QFileInfoH handle)
{
	return (unsigned int) ((QFileInfo *)handle)->permissions();
}

qint64 QFileInfo_size(QFileInfoH handle)
{
	return (qint64) ((QFileInfo *)handle)->size();
}

void QFileInfo_birthTime(QFileInfoH handle, QDateTimeH retval)
{
	*(QDateTime *)retval = ((QFileInfo *)handle)->birthTime();
}

void QFileInfo_lastModified(QFileInfoH handle, QDateTimeH retval)
{
	*(QDateTime *)retval = ((QFileInfo *)handle)->lastModified();
}

void QFileInfo_lastRead(QFileInfoH handle, QDateTimeH retval)
{
	*(QDateTime *)retval = ((QFileInfo *)handle)->lastRead();
}

bool QFileInfo_caching(QFileInfoH handle)
{
	return (bool) ((QFileInfo *)handle)->caching();
}

void QFileInfo_setCaching(QFileInfoH handle, bool on)
{
	((QFileInfo *)handle)->setCaching(on);
}

