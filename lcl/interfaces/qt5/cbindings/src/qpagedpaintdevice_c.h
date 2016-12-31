//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QPAGEDPAINTDEVICE_C_H
#define QPAGEDPAINTDEVICE_C_H

#include <QtGui>
#include "pascalbind.h"

C_EXPORT bool QPagedPaintDevice_newPage(QPagedPaintDeviceH handle);
C_EXPORT void QPagedPaintDevice_setPageSize(QPagedPaintDeviceH handle, QPagedPaintDevice::PageSize size);
C_EXPORT QPagedPaintDevice::PageSize QPagedPaintDevice_pageSize(QPagedPaintDeviceH handle);
C_EXPORT void QPagedPaintDevice_setPageSizeMM(QPagedPaintDeviceH handle, const QSizeFH size);
C_EXPORT void QPagedPaintDevice_pageSizeMM(QPagedPaintDeviceH handle, QSizeFH retval);

#endif
