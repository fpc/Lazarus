//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QDIR_C_H
#define QDIR_C_H

#include <QtCore>
#include "pascalbind.h"

C_EXPORT QDirH QDir_Create(const QDirH AnonParam1);
C_EXPORT void QDir_Destroy(QDirH handle);
C_EXPORT QDirH QDir_Create2(PWideString path);
C_EXPORT QDirH QDir_Create3(PWideString path, PWideString nameFilter, unsigned int sort, unsigned int filter);
C_EXPORT void QDir_swap(QDirH handle, QDirH other);
C_EXPORT void QDir_setPath(QDirH handle, PWideString path);
C_EXPORT void QDir_path(QDirH handle, PWideString retval);
C_EXPORT void QDir_absolutePath(QDirH handle, PWideString retval);
C_EXPORT void QDir_canonicalPath(QDirH handle, PWideString retval);
C_EXPORT void QDir_setSearchPaths(PWideString prefix, const QStringListH searchPaths);
C_EXPORT void QDir_addSearchPath(PWideString prefix, PWideString path);
C_EXPORT void QDir_searchPaths(QStringListH retval, PWideString prefix);
C_EXPORT void QDir_dirName(QDirH handle, PWideString retval);
C_EXPORT void QDir_filePath(QDirH handle, PWideString retval, PWideString fileName);
C_EXPORT void QDir_absoluteFilePath(QDirH handle, PWideString retval, PWideString fileName);
C_EXPORT void QDir_relativeFilePath(QDirH handle, PWideString retval, PWideString fileName);
C_EXPORT void QDir_toNativeSeparators(PWideString retval, PWideString pathName);
C_EXPORT void QDir_fromNativeSeparators(PWideString retval, PWideString pathName);
C_EXPORT bool QDir_cd(QDirH handle, PWideString dirName);
C_EXPORT bool QDir_cdUp(QDirH handle);
C_EXPORT void QDir_nameFilters(QDirH handle, QStringListH retval);
C_EXPORT void QDir_setNameFilters(QDirH handle, const QStringListH nameFilters);
C_EXPORT unsigned int QDir_filter(QDirH handle);
C_EXPORT void QDir_setFilter(QDirH handle, unsigned int filter);
C_EXPORT unsigned int QDir_sorting(QDirH handle);
C_EXPORT void QDir_setSorting(QDirH handle, unsigned int sort);
C_EXPORT uint QDir_count(QDirH handle);
C_EXPORT void QDir_nameFiltersFromString(QStringListH retval, PWideString nameFilter);
C_EXPORT void QDir_entryList(QDirH handle, QStringListH retval, unsigned int filters, unsigned int sort);
C_EXPORT void QDir_entryList2(QDirH handle, QStringListH retval, const QStringListH nameFilters, unsigned int filters, unsigned int sort);
C_EXPORT void QDir_entryInfoList(QDirH handle, PPtrIntArray retval, unsigned int filters, unsigned int sort);
C_EXPORT void QDir_entryInfoList2(QDirH handle, PPtrIntArray retval, const QStringListH nameFilters, unsigned int filters, unsigned int sort);
C_EXPORT bool QDir_mkdir(QDirH handle, PWideString dirName);
C_EXPORT bool QDir_rmdir(QDirH handle, PWideString dirName);
C_EXPORT bool QDir_mkpath(QDirH handle, PWideString dirPath);
C_EXPORT bool QDir_rmpath(QDirH handle, PWideString dirPath);
C_EXPORT bool QDir_removeRecursively(QDirH handle);
C_EXPORT bool QDir_isReadable(QDirH handle);
C_EXPORT bool QDir_exists(QDirH handle);
C_EXPORT bool QDir_isRoot(QDirH handle);
C_EXPORT bool QDir_isRelativePath(PWideString path);
C_EXPORT bool QDir_isAbsolutePath(PWideString path);
C_EXPORT bool QDir_isRelative(QDirH handle);
C_EXPORT bool QDir_isAbsolute(QDirH handle);
C_EXPORT bool QDir_makeAbsolute(QDirH handle);
C_EXPORT bool QDir_remove(QDirH handle, PWideString fileName);
C_EXPORT bool QDir_rename(QDirH handle, PWideString oldName, PWideString newName);
C_EXPORT bool QDir_exists2(QDirH handle, PWideString name);
C_EXPORT void QDir_drives(PPtrIntArray retval);
C_EXPORT void QDir_separator(PWideChar retval);
C_EXPORT bool QDir_setCurrent(PWideString path);
C_EXPORT void QDir_current(QDirH retval);
C_EXPORT void QDir_currentPath(PWideString retval);
C_EXPORT void QDir_home(QDirH retval);
C_EXPORT void QDir_homePath(PWideString retval);
C_EXPORT void QDir_root(QDirH retval);
C_EXPORT void QDir_rootPath(PWideString retval);
C_EXPORT void QDir_temp(QDirH retval);
C_EXPORT void QDir_tempPath(PWideString retval);
C_EXPORT bool QDir_match(const QStringListH filters, PWideString fileName);
C_EXPORT bool QDir_match2(PWideString filter, PWideString fileName);
C_EXPORT void QDir_cleanPath(PWideString retval, PWideString path);
C_EXPORT void QDir_refresh(QDirH handle);

#endif
