//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QFILE_C_H
#define QFILE_C_H

#include <QtCore>
#include "pascalbind.h"

C_EXPORT QFileH QFile_Create();
C_EXPORT void QFile_Destroy(QFileH handle);
C_EXPORT QFileH QFile_Create2(PWideString name);
C_EXPORT QFileH QFile_Create3(QObjectH parent);
C_EXPORT QFileH QFile_Create4(PWideString name, QObjectH parent);
C_EXPORT void QFile_fileName(QFileH handle, PWideString retval);
C_EXPORT void QFile_setFileName(QFileH handle, PWideString name);
C_EXPORT void QFile_encodeName(QByteArrayH retval, PWideString fileName);
C_EXPORT void QFile_decodeName(PWideString retval, const QByteArrayH localFileName);
C_EXPORT void QFile_decodeName2(PWideString retval, const char* localFileName);
C_EXPORT bool QFile_exists(QFileH handle);
C_EXPORT bool QFile_exists2(PWideString fileName);
C_EXPORT void QFile_symLinkTarget(QFileH handle, PWideString retval);
C_EXPORT void QFile_symLinkTarget2(PWideString retval, PWideString fileName);
C_EXPORT bool QFile_remove(QFileH handle);
C_EXPORT bool QFile_remove2(PWideString fileName);
C_EXPORT bool QFile_rename(QFileH handle, PWideString newName);
C_EXPORT bool QFile_rename2(PWideString oldName, PWideString newName);
C_EXPORT bool QFile_link(QFileH handle, PWideString newName);
C_EXPORT bool QFile_link2(PWideString oldname, PWideString newName);
C_EXPORT bool QFile_copy(QFileH handle, PWideString newName);
C_EXPORT bool QFile_copy2(PWideString fileName, PWideString newName);
C_EXPORT bool QFile_open(QFileH handle, unsigned int flags);
C_EXPORT bool QFile_open2(QFileH handle, int fd, unsigned int ioFlags, unsigned int handleFlags);
C_EXPORT qint64 QFile_size(QFileH handle);
C_EXPORT bool QFile_resize(QFileH handle, qint64 sz);
C_EXPORT bool QFile_resize2(PWideString filename, qint64 sz);
C_EXPORT unsigned int QFile_permissions(QFileH handle);
C_EXPORT unsigned int QFile_permissions2(PWideString filename);
C_EXPORT bool QFile_setPermissions(QFileH handle, unsigned int permissionSpec);
C_EXPORT bool QFile_setPermissions2(PWideString filename, unsigned int permissionSpec);

#endif
