//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qdir_c.h"

QDirH QDir_Create(const QDirH AnonParam1)
{
	return (QDirH) new QDir(*(const QDir*)AnonParam1);
}

void QDir_Destroy(QDirH handle)
{
	delete (QDir *)handle;
}

QDirH QDir_Create2(PWideString path)
{
	QString t_path;
	copyPWideStringToQString(path, t_path);
	return (QDirH) new QDir(t_path);
}

QDirH QDir_Create3(PWideString path, PWideString nameFilter, unsigned int sort, unsigned int filter)
{
	QString t_path;
	QString t_nameFilter;
	copyPWideStringToQString(path, t_path);
	copyPWideStringToQString(nameFilter, t_nameFilter);
	return (QDirH) new QDir(t_path, t_nameFilter, (QDir::SortFlags)sort, (QDir::Filters)filter);
}

void QDir_swap(QDirH handle, QDirH other)
{
	((QDir *)handle)->swap(*(QDir*)other);
}

void QDir_setPath(QDirH handle, PWideString path)
{
	QString t_path;
	copyPWideStringToQString(path, t_path);
	((QDir *)handle)->setPath(t_path);
}

void QDir_path(QDirH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QDir *)handle)->path();
	copyQStringToPWideString(t_retval, retval);
}

void QDir_absolutePath(QDirH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QDir *)handle)->absolutePath();
	copyQStringToPWideString(t_retval, retval);
}

void QDir_canonicalPath(QDirH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QDir *)handle)->canonicalPath();
	copyQStringToPWideString(t_retval, retval);
}

void QDir_setSearchPaths(PWideString prefix, const QStringListH searchPaths)
{
	QString t_prefix;
	copyPWideStringToQString(prefix, t_prefix);
	QDir::setSearchPaths(t_prefix, *(const QStringList*)searchPaths);
}

void QDir_addSearchPath(PWideString prefix, PWideString path)
{
	QString t_prefix;
	QString t_path;
	copyPWideStringToQString(prefix, t_prefix);
	copyPWideStringToQString(path, t_path);
	QDir::addSearchPath(t_prefix, t_path);
}

void QDir_searchPaths(QStringListH retval, PWideString prefix)
{
	QString t_prefix;
	copyPWideStringToQString(prefix, t_prefix);
	*(QStringList *)retval = QDir::searchPaths(t_prefix);
}

void QDir_dirName(QDirH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QDir *)handle)->dirName();
	copyQStringToPWideString(t_retval, retval);
}

void QDir_filePath(QDirH handle, PWideString retval, PWideString fileName)
{
	QString t_retval;
	QString t_fileName;
	copyPWideStringToQString(fileName, t_fileName);
	t_retval = ((QDir *)handle)->filePath(t_fileName);
	copyQStringToPWideString(t_retval, retval);
}

void QDir_absoluteFilePath(QDirH handle, PWideString retval, PWideString fileName)
{
	QString t_retval;
	QString t_fileName;
	copyPWideStringToQString(fileName, t_fileName);
	t_retval = ((QDir *)handle)->absoluteFilePath(t_fileName);
	copyQStringToPWideString(t_retval, retval);
}

void QDir_relativeFilePath(QDirH handle, PWideString retval, PWideString fileName)
{
	QString t_retval;
	QString t_fileName;
	copyPWideStringToQString(fileName, t_fileName);
	t_retval = ((QDir *)handle)->relativeFilePath(t_fileName);
	copyQStringToPWideString(t_retval, retval);
}

void QDir_toNativeSeparators(PWideString retval, PWideString pathName)
{
	QString t_retval;
	QString t_pathName;
	copyPWideStringToQString(pathName, t_pathName);
	t_retval = QDir::toNativeSeparators(t_pathName);
	copyQStringToPWideString(t_retval, retval);
}

void QDir_fromNativeSeparators(PWideString retval, PWideString pathName)
{
	QString t_retval;
	QString t_pathName;
	copyPWideStringToQString(pathName, t_pathName);
	t_retval = QDir::fromNativeSeparators(t_pathName);
	copyQStringToPWideString(t_retval, retval);
}

bool QDir_cd(QDirH handle, PWideString dirName)
{
	QString t_dirName;
	copyPWideStringToQString(dirName, t_dirName);
	return (bool) ((QDir *)handle)->cd(t_dirName);
}

bool QDir_cdUp(QDirH handle)
{
	return (bool) ((QDir *)handle)->cdUp();
}

void QDir_nameFilters(QDirH handle, QStringListH retval)
{
	*(QStringList *)retval = ((QDir *)handle)->nameFilters();
}

void QDir_setNameFilters(QDirH handle, const QStringListH nameFilters)
{
	((QDir *)handle)->setNameFilters(*(const QStringList*)nameFilters);
}

unsigned int QDir_filter(QDirH handle)
{
	return (unsigned int) ((QDir *)handle)->filter();
}

void QDir_setFilter(QDirH handle, unsigned int filter)
{
	((QDir *)handle)->setFilter((QDir::Filters)filter);
}

unsigned int QDir_sorting(QDirH handle)
{
	return (unsigned int) ((QDir *)handle)->sorting();
}

void QDir_setSorting(QDirH handle, unsigned int sort)
{
	((QDir *)handle)->setSorting((QDir::SortFlags)sort);
}

uint QDir_count(QDirH handle)
{
	return (uint) ((QDir *)handle)->count();
}

void QDir_nameFiltersFromString(QStringListH retval, PWideString nameFilter)
{
	QString t_nameFilter;
	copyPWideStringToQString(nameFilter, t_nameFilter);
	*(QStringList *)retval = QDir::nameFiltersFromString(t_nameFilter);
}

void QDir_entryList(QDirH handle, QStringListH retval, unsigned int filters, unsigned int sort)
{
	*(QStringList *)retval = ((QDir *)handle)->entryList((QDir::Filters)filters, (QDir::SortFlags)sort);
}

void QDir_entryList2(QDirH handle, QStringListH retval, const QStringListH nameFilters, unsigned int filters, unsigned int sort)
{
	*(QStringList *)retval = ((QDir *)handle)->entryList(*(const QStringList*)nameFilters, (QDir::Filters)filters, (QDir::SortFlags)sort);
}

void QDir_entryInfoList(QDirH handle, PPtrIntArray retval, unsigned int filters, unsigned int sort)
{
	QFileInfoList t_retval;
	t_retval = ((QDir *)handle)->entryInfoList((QDir::Filters)filters, (QDir::SortFlags)sort);
	copyQListTemplateToPtrIntArrayWithNew(t_retval, retval);
}

void QDir_entryInfoList2(QDirH handle, PPtrIntArray retval, const QStringListH nameFilters, unsigned int filters, unsigned int sort)
{
	QFileInfoList t_retval;
	t_retval = ((QDir *)handle)->entryInfoList(*(const QStringList*)nameFilters, (QDir::Filters)filters, (QDir::SortFlags)sort);
	copyQListTemplateToPtrIntArrayWithNew(t_retval, retval);
}

bool QDir_mkdir(QDirH handle, PWideString dirName)
{
	QString t_dirName;
	copyPWideStringToQString(dirName, t_dirName);
	return (bool) ((QDir *)handle)->mkdir(t_dirName);
}

bool QDir_rmdir(QDirH handle, PWideString dirName)
{
	QString t_dirName;
	copyPWideStringToQString(dirName, t_dirName);
	return (bool) ((QDir *)handle)->rmdir(t_dirName);
}

bool QDir_mkpath(QDirH handle, PWideString dirPath)
{
	QString t_dirPath;
	copyPWideStringToQString(dirPath, t_dirPath);
	return (bool) ((QDir *)handle)->mkpath(t_dirPath);
}

bool QDir_rmpath(QDirH handle, PWideString dirPath)
{
	QString t_dirPath;
	copyPWideStringToQString(dirPath, t_dirPath);
	return (bool) ((QDir *)handle)->rmpath(t_dirPath);
}

bool QDir_removeRecursively(QDirH handle)
{
	return (bool) ((QDir *)handle)->removeRecursively();
}

bool QDir_isReadable(QDirH handle)
{
	return (bool) ((QDir *)handle)->isReadable();
}

bool QDir_exists(QDirH handle)
{
	return (bool) ((QDir *)handle)->exists();
}

bool QDir_isRoot(QDirH handle)
{
	return (bool) ((QDir *)handle)->isRoot();
}

bool QDir_isRelativePath(PWideString path)
{
	QString t_path;
	copyPWideStringToQString(path, t_path);
	return (bool) QDir::isRelativePath(t_path);
}

bool QDir_isAbsolutePath(PWideString path)
{
	QString t_path;
	copyPWideStringToQString(path, t_path);
	return (bool) QDir::isAbsolutePath(t_path);
}

bool QDir_isRelative(QDirH handle)
{
	return (bool) ((QDir *)handle)->isRelative();
}

bool QDir_isAbsolute(QDirH handle)
{
	return (bool) ((QDir *)handle)->isAbsolute();
}

bool QDir_makeAbsolute(QDirH handle)
{
	return (bool) ((QDir *)handle)->makeAbsolute();
}

bool QDir_remove(QDirH handle, PWideString fileName)
{
	QString t_fileName;
	copyPWideStringToQString(fileName, t_fileName);
	return (bool) ((QDir *)handle)->remove(t_fileName);
}

bool QDir_rename(QDirH handle, PWideString oldName, PWideString newName)
{
	QString t_oldName;
	QString t_newName;
	copyPWideStringToQString(oldName, t_oldName);
	copyPWideStringToQString(newName, t_newName);
	return (bool) ((QDir *)handle)->rename(t_oldName, t_newName);
}

bool QDir_exists2(QDirH handle, PWideString name)
{
	QString t_name;
	copyPWideStringToQString(name, t_name);
	return (bool) ((QDir *)handle)->exists(t_name);
}

void QDir_drives(PPtrIntArray retval)
{
	QFileInfoList t_retval;
	t_retval = QDir::drives();
	copyQListTemplateToPtrIntArrayWithNew(t_retval, retval);
}

void QDir_separator(PWideChar retval)
{
	*(QChar *)retval = QDir::separator();
}

bool QDir_setCurrent(PWideString path)
{
	QString t_path;
	copyPWideStringToQString(path, t_path);
	return (bool) QDir::setCurrent(t_path);
}

void QDir_current(QDirH retval)
{
	*(QDir *)retval = QDir::current();
}

void QDir_currentPath(PWideString retval)
{
	QString t_retval;
	t_retval = QDir::currentPath();
	copyQStringToPWideString(t_retval, retval);
}

void QDir_home(QDirH retval)
{
	*(QDir *)retval = QDir::home();
}

void QDir_homePath(PWideString retval)
{
	QString t_retval;
	t_retval = QDir::homePath();
	copyQStringToPWideString(t_retval, retval);
}

void QDir_root(QDirH retval)
{
	*(QDir *)retval = QDir::root();
}

void QDir_rootPath(PWideString retval)
{
	QString t_retval;
	t_retval = QDir::rootPath();
	copyQStringToPWideString(t_retval, retval);
}

void QDir_temp(QDirH retval)
{
	*(QDir *)retval = QDir::temp();
}

void QDir_tempPath(PWideString retval)
{
	QString t_retval;
	t_retval = QDir::tempPath();
	copyQStringToPWideString(t_retval, retval);
}

bool QDir_match(const QStringListH filters, PWideString fileName)
{
	QString t_fileName;
	copyPWideStringToQString(fileName, t_fileName);
	return (bool) QDir::match(*(const QStringList*)filters, t_fileName);
}

bool QDir_match2(PWideString filter, PWideString fileName)
{
	QString t_filter;
	QString t_fileName;
	copyPWideStringToQString(filter, t_filter);
	copyPWideStringToQString(fileName, t_fileName);
	return (bool) QDir::match(t_filter, t_fileName);
}

void QDir_cleanPath(PWideString retval, PWideString path)
{
	QString t_retval;
	QString t_path;
	copyPWideStringToQString(path, t_path);
	t_retval = QDir::cleanPath(t_path);
	copyQStringToPWideString(t_retval, retval);
}

void QDir_refresh(QDirH handle)
{
	((QDir *)handle)->refresh();
}

