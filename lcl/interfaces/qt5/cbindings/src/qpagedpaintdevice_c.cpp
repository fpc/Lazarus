//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qpagedpaintdevice_c.h"

bool QPagedPaintDevice_newPage(QPagedPaintDeviceH handle)
{
	return (bool) ((QPagedPaintDevice *)handle)->newPage();
}

void QPagedPaintDevice_setPageSize(QPagedPaintDeviceH handle, QPagedPaintDevice::PageSize size)
{
	((QPagedPaintDevice *)handle)->setPageSize(size);
}

QPagedPaintDevice::PageSize QPagedPaintDevice_pageSize(QPagedPaintDeviceH handle)
{
	return (QPagedPaintDevice::PageSize) ((QPagedPaintDevice *)handle)->pageSize();
}

void QPagedPaintDevice_setPageSizeMM(QPagedPaintDeviceH handle, const QSizeFH size)
{
	((QPagedPaintDevice *)handle)->setPageSizeMM(*(const QSizeF*)size);
}

void QPagedPaintDevice_pageSizeMM(QPagedPaintDeviceH handle, QSizeFH retval)
{
	*(QSizeF *)retval = ((QPagedPaintDevice *)handle)->pageSizeMM();
}

