//******************************************************************************
//  Copyright (c) 2017 by Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qbackingstore_c.h"

QBackingStoreH QBackingStore_Create(QWindowH window)
{
  return (QBackingStoreH) new QBackingStore((QWindow*)window);
}

void QBackingStore_Destroy(QBackingStoreH handle)
{
  delete (QBackingStore *)handle;
}

QWindowH QBackingStore_window(QBackingStoreH handle)
{
  return (QWindowH) ((QBackingStore *)handle)->window();
}

QPaintDeviceH QBackingStore_paintDevice(QBackingStoreH handle)
{
  return (QPaintDeviceH) ((QBackingStore *)handle)->paintDevice();
}

void QBackingStore_flush(QBackingStoreH handle, const QRegionH region, QWindowH window, const QPointH offset)
{
  ((QBackingStore *)handle)->flush(*(const QRegion*) region, (QWindow *)window, *(const QPoint*) offset);
}

void QBackingStore_resize(QBackingStoreH handle, const QSizeH AnonParam1)
{
  ((QBackingStore *)handle)->resize(*(const QSize*)AnonParam1);
}

void QBackingStore_size(QBackingStoreH handle, PSize retval)
{
  *(QSize *)retval = ((QBackingStore *)handle)->size();
}

bool QBackingStore_scroll(QBackingStoreH handle, const QRegionH area, int dx, int dy)
{
  return ((QBackingStore *)handle)->scroll(*(const QRegion*)area, dx, dy);
}

void QBackingStore_beginPaint(QBackingStoreH handle, const QRegionH AnonParam1)
{
  ((QBackingStore *)handle)->beginPaint(*(const QRegion*)AnonParam1);
}

void QBackingStore_endPaint(QBackingStoreH handle)
{
  ((QBackingStore *)handle)->endPaint();
}

void QBackingStore_setStaticContents(QBackingStoreH handle, const QRegionH AnonParam1)
{
  ((QBackingStore *)handle)->setStaticContents(*(const QRegion*)AnonParam1);
}

void QBackingStore_staticContents(QBackingStoreH handle, QRegionH retval)
{
	*(QRegion *)retval = ((QBackingStore *)handle)->staticContents();
}

bool QBackingStore_hasStaticContents(QBackingStoreH handle)
{
  return (bool) ((QBackingStore *)handle)->hasStaticContents();
}

