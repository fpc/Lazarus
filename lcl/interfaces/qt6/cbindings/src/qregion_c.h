//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QREGION_C_H
#define QREGION_C_H

#include <QtGui>
#include "pascalbind.h"

C_EXPORT QRegionH QRegion_Create();
C_EXPORT void QRegion_Destroy(QRegionH handle);
C_EXPORT QRegionH QRegion_Create2(int x, int y, int w, int h, QRegion::RegionType t);
C_EXPORT QRegionH QRegion_Create3(PRect r, QRegion::RegionType t);
C_EXPORT QRegionH QRegion_Create4(const QPolygonH pa, Qt::FillRule fillRule);
C_EXPORT QRegionH QRegion_Create5(const QRegionH region);
C_EXPORT QRegionH QRegion_Create6(const QBitmapH bitmap);
C_EXPORT void QRegion_swap(QRegionH handle, QRegionH other);
C_EXPORT bool QRegion_isEmpty(QRegionH handle);
C_EXPORT bool QRegion_isNull(QRegionH handle);
C_EXPORT bool QRegion_contains(QRegionH handle, const QPointH p);
C_EXPORT bool QRegion_contains2(QRegionH handle, PRect r);
C_EXPORT void QRegion_translate(QRegionH handle, int dx, int dy);
C_EXPORT void QRegion_translate2(QRegionH handle, const QPointH p);
C_EXPORT void QRegion_translated(QRegionH handle, QRegionH retval, int dx, int dy);
C_EXPORT void QRegion_translated2(QRegionH handle, QRegionH retval, const QPointH p);
C_EXPORT void QRegion_united(QRegionH handle, QRegionH retval, const QRegionH r);
C_EXPORT void QRegion_united2(QRegionH handle, QRegionH retval, PRect r);
C_EXPORT void QRegion_intersected(QRegionH handle, QRegionH retval, const QRegionH r);
C_EXPORT void QRegion_intersected2(QRegionH handle, QRegionH retval, PRect r);
C_EXPORT void QRegion_subtracted(QRegionH handle, QRegionH retval, const QRegionH r);
C_EXPORT void QRegion_xored(QRegionH handle, QRegionH retval, const QRegionH r);
C_EXPORT bool QRegion_intersects(QRegionH handle, const QRegionH r);
C_EXPORT bool QRegion_intersects2(QRegionH handle, PRect r);
C_EXPORT void QRegion_boundingRect(QRegionH handle, PRect retval);
C_EXPORT void QRegion_setRects(QRegionH handle, PRect rect, int num);
C_EXPORT int QRegion_rectCount(QRegionH handle);

#endif
