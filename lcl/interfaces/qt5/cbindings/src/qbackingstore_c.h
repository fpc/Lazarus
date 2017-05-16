//******************************************************************************
//  Copyright (c) 2017 by Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QBACKINGSTORE_C_H
#define QBACKINGSTORE_C_H

#include <QtGui>
#include "pascalbind.h"

C_EXPORT QBackingStoreH QBackingStore_Create(QWindowH window);
C_EXPORT void QBackingStore_Destroy(QBackingStoreH handle);
C_EXPORT QWindowH QBackingStore_window(QBackingStoreH handle);
C_EXPORT QPaintDeviceH QBackingStore_paintDevice(QBackingStoreH handle);
C_EXPORT void QBackingStore_flush(QBackingStoreH handle, const QRegionH region, QWindowH window, const QPointH offset);
C_EXPORT void QBackingStore_resize(QBackingStoreH handle, const QSizeH AnonParam1);
C_EXPORT void QBackingStore_size(QBackingStoreH handle, PSize retval);
C_EXPORT bool QBackingStore_scroll(QBackingStoreH handle, const QRegionH area, int dx, int dy);
C_EXPORT void QBackingStore_beginPaint(QBackingStoreH handle, const QRegionH AnonParam1);
C_EXPORT void QBackingStore_endPaint(QBackingStoreH handle);
C_EXPORT void QBackingStore_setStaticContents(QBackingStoreH handle, const QRegionH AnonParam1);
C_EXPORT void QBackingStore_staticContents(QBackingStoreH handle, QRegionH retval);
C_EXPORT bool QBackingStore_hasStaticContents(QBackingStoreH handle);

#endif
