//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QFILEDEVICE_C_H
#define QFILEDEVICE_C_H

#include <QtCore>
#include "pascalbind.h"

C_EXPORT QFileDevice::FileError QFileDevice_error(QFileDeviceH handle);
C_EXPORT void QFileDevice_unsetError(QFileDeviceH handle);
C_EXPORT void QFileDevice_close(QFileDeviceH handle);
C_EXPORT bool QFileDevice_isSequential(QFileDeviceH handle);
C_EXPORT int QFileDevice_handle(QFileDeviceH handle);
C_EXPORT void QFileDevice_fileName(QFileDeviceH handle, PWideString retval);
C_EXPORT qint64 QFileDevice_pos(QFileDeviceH handle);
C_EXPORT bool QFileDevice_seek(QFileDeviceH handle, qint64 offset);
C_EXPORT bool QFileDevice_atEnd(QFileDeviceH handle);
C_EXPORT bool QFileDevice_flush(QFileDeviceH handle);
C_EXPORT qint64 QFileDevice_size(QFileDeviceH handle);
C_EXPORT bool QFileDevice_resize(QFileDeviceH handle, qint64 sz);
C_EXPORT unsigned int QFileDevice_permissions(QFileDeviceH handle);
C_EXPORT bool QFileDevice_setPermissions(QFileDeviceH handle, unsigned int permissionSpec);
C_EXPORT uchar* QFileDevice_map(QFileDeviceH handle, qint64 offset, qint64 size, QFileDevice::MemoryMapFlags flags);
C_EXPORT bool QFileDevice_unmap(QFileDeviceH handle, uchar* address);

#endif
