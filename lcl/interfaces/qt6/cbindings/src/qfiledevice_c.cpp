//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qfiledevice_c.h"

QFileDevice::FileError QFileDevice_error(QFileDeviceH handle)
{
	return (QFileDevice::FileError) ((QFileDevice *)handle)->error();
}

void QFileDevice_unsetError(QFileDeviceH handle)
{
	((QFileDevice *)handle)->unsetError();
}

void QFileDevice_close(QFileDeviceH handle)
{
	((QFileDevice *)handle)->close();
}

bool QFileDevice_isSequential(QFileDeviceH handle)
{
	return (bool) ((QFileDevice *)handle)->isSequential();
}

int QFileDevice_handle(QFileDeviceH handle)
{
	return (int) ((QFileDevice *)handle)->handle();
}

void QFileDevice_fileName(QFileDeviceH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QFileDevice *)handle)->fileName();
	copyQStringToPWideString(t_retval, retval);
}

qint64 QFileDevice_pos(QFileDeviceH handle)
{
	return (qint64) ((QFileDevice *)handle)->pos();
}

bool QFileDevice_seek(QFileDeviceH handle, qint64 offset)
{
	return (bool) ((QFileDevice *)handle)->seek(offset);
}

bool QFileDevice_atEnd(QFileDeviceH handle)
{
	return (bool) ((QFileDevice *)handle)->atEnd();
}

bool QFileDevice_flush(QFileDeviceH handle)
{
	return (bool) ((QFileDevice *)handle)->flush();
}

qint64 QFileDevice_size(QFileDeviceH handle)
{
	return (qint64) ((QFileDevice *)handle)->size();
}

bool QFileDevice_resize(QFileDeviceH handle, qint64 sz)
{
	return (bool) ((QFileDevice *)handle)->resize(sz);
}

unsigned int QFileDevice_permissions(QFileDeviceH handle)
{
	return (unsigned int) ((QFileDevice *)handle)->permissions();
}

bool QFileDevice_setPermissions(QFileDeviceH handle, unsigned int permissionSpec)
{
	return (bool) ((QFileDevice *)handle)->setPermissions((QFileDevice::Permissions)permissionSpec);
}

uchar* QFileDevice_map(QFileDeviceH handle, qint64 offset, qint64 size, QFileDevice::MemoryMapFlags flags)
{
	return (uchar*) ((QFileDevice *)handle)->map(offset, size, flags);
}

bool QFileDevice_unmap(QFileDeviceH handle, uchar* address)
{
	return (bool) ((QFileDevice *)handle)->unmap(address);
}

