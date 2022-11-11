//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QFILEINFO_C_H
#define QFILEINFO_C_H

#include <QtCore>
#include "pascalbind.h"

C_EXPORT QFileInfoH QFileInfo_Create();
C_EXPORT void QFileInfo_Destroy(QFileInfoH handle);
C_EXPORT QFileInfoH QFileInfo_Create2(PWideString file);
C_EXPORT QFileInfoH QFileInfo_Create3(const QFileH file);
C_EXPORT QFileInfoH QFileInfo_Create4(const QDirH dir, PWideString file);
C_EXPORT QFileInfoH QFileInfo_Create5(const QFileInfoH fileinfo);
C_EXPORT void QFileInfo_swap(QFileInfoH handle, QFileInfoH other);
C_EXPORT void QFileInfo_setFile(QFileInfoH handle, PWideString file);
C_EXPORT void QFileInfo_setFile2(QFileInfoH handle, const QFileH file);
C_EXPORT void QFileInfo_setFile3(QFileInfoH handle, const QDirH dir, PWideString file);
C_EXPORT bool QFileInfo_exists(QFileInfoH handle);
C_EXPORT void QFileInfo_refresh(QFileInfoH handle);
C_EXPORT void QFileInfo_filePath(QFileInfoH handle, PWideString retval);
C_EXPORT void QFileInfo_absoluteFilePath(QFileInfoH handle, PWideString retval);
C_EXPORT void QFileInfo_canonicalFilePath(QFileInfoH handle, PWideString retval);
C_EXPORT void QFileInfo_fileName(QFileInfoH handle, PWideString retval);
C_EXPORT void QFileInfo_baseName(QFileInfoH handle, PWideString retval);
C_EXPORT void QFileInfo_completeBaseName(QFileInfoH handle, PWideString retval);
C_EXPORT void QFileInfo_suffix(QFileInfoH handle, PWideString retval);
C_EXPORT void QFileInfo_bundleName(QFileInfoH handle, PWideString retval);
C_EXPORT void QFileInfo_completeSuffix(QFileInfoH handle, PWideString retval);
C_EXPORT void QFileInfo_path(QFileInfoH handle, PWideString retval);
C_EXPORT void QFileInfo_absolutePath(QFileInfoH handle, PWideString retval);
C_EXPORT void QFileInfo_canonicalPath(QFileInfoH handle, PWideString retval);
C_EXPORT void QFileInfo_dir(QFileInfoH handle, QDirH retval);
C_EXPORT void QFileInfo_absoluteDir(QFileInfoH handle, QDirH retval);
C_EXPORT bool QFileInfo_isReadable(QFileInfoH handle);
C_EXPORT bool QFileInfo_isWritable(QFileInfoH handle);
C_EXPORT bool QFileInfo_isExecutable(QFileInfoH handle);
C_EXPORT bool QFileInfo_isHidden(QFileInfoH handle);
C_EXPORT bool QFileInfo_isNativePath(QFileInfoH handle);
C_EXPORT bool QFileInfo_isRelative(QFileInfoH handle);
C_EXPORT bool QFileInfo_isAbsolute(QFileInfoH handle);
C_EXPORT bool QFileInfo_makeAbsolute(QFileInfoH handle);
C_EXPORT bool QFileInfo_isFile(QFileInfoH handle);
C_EXPORT bool QFileInfo_isDir(QFileInfoH handle);
C_EXPORT bool QFileInfo_isSymLink(QFileInfoH handle);
C_EXPORT bool QFileInfo_isRoot(QFileInfoH handle);
C_EXPORT bool QFileInfo_isBundle(QFileInfoH handle);
C_EXPORT void QFileInfo_symLinkTarget(QFileInfoH handle, PWideString retval);
C_EXPORT void QFileInfo_owner(QFileInfoH handle, PWideString retval);
C_EXPORT uint QFileInfo_ownerId(QFileInfoH handle);
C_EXPORT void QFileInfo_group(QFileInfoH handle, PWideString retval);
C_EXPORT uint QFileInfo_groupId(QFileInfoH handle);
C_EXPORT bool QFileInfo_permission(QFileInfoH handle, unsigned int permissions);
C_EXPORT unsigned int QFileInfo_permissions(QFileInfoH handle);
C_EXPORT qint64 QFileInfo_size(QFileInfoH handle);
C_EXPORT void QFileInfo_birthTime(QFileInfoH handle, QDateTimeH retval);
C_EXPORT void QFileInfo_lastModified(QFileInfoH handle, QDateTimeH retval);
C_EXPORT void QFileInfo_lastRead(QFileInfoH handle, QDateTimeH retval);
C_EXPORT bool QFileInfo_caching(QFileInfoH handle);
C_EXPORT void QFileInfo_setCaching(QFileInfoH handle, bool on);

#endif
