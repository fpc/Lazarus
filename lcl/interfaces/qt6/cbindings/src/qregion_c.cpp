//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qregion_c.h"

QRegionH QRegion_Create()
{
	return (QRegionH) new QRegion();
}

void QRegion_Destroy(QRegionH handle)
{
	delete (QRegion *)handle;
}

QRegionH QRegion_Create2(int x, int y, int w, int h, QRegion::RegionType t)
{
	return (QRegionH) new QRegion(x, y, w, h, t);
}

QRegionH QRegion_Create3(PRect r, QRegion::RegionType t)
{
	QRect t_r;
	copyPRectToQRect(r, t_r);
	return (QRegionH) new QRegion(t_r, t);
}

QRegionH QRegion_Create4(const QPolygonH pa, Qt::FillRule fillRule)
{
	return (QRegionH) new QRegion(*(const QPolygon*)pa, fillRule);
}

QRegionH QRegion_Create5(const QRegionH region)
{
	return (QRegionH) new QRegion(*(const QRegion*)region);
}

QRegionH QRegion_Create6(const QBitmapH bitmap)
{
	return (QRegionH) new QRegion(*(const QBitmap*)bitmap);
}

void QRegion_swap(QRegionH handle, QRegionH other)
{
	((QRegion *)handle)->swap(*(QRegion*)other);
}

bool QRegion_isEmpty(QRegionH handle)
{
	return (bool) ((QRegion *)handle)->isEmpty();
}

bool QRegion_isNull(QRegionH handle)
{
	return (bool) ((QRegion *)handle)->isNull();
}

bool QRegion_contains(QRegionH handle, const QPointH p)
{
	return (bool) ((QRegion *)handle)->contains(*(const QPoint*)p);
}

bool QRegion_contains2(QRegionH handle, PRect r)
{
	QRect t_r;
	copyPRectToQRect(r, t_r);
	return (bool) ((QRegion *)handle)->contains(t_r);
}

void QRegion_translate(QRegionH handle, int dx, int dy)
{
	((QRegion *)handle)->translate(dx, dy);
}

void QRegion_translate2(QRegionH handle, const QPointH p)
{
	((QRegion *)handle)->translate(*(const QPoint*)p);
}

void QRegion_translated(QRegionH handle, QRegionH retval, int dx, int dy)
{
	*(QRegion *)retval = ((QRegion *)handle)->translated(dx, dy);
}

void QRegion_translated2(QRegionH handle, QRegionH retval, const QPointH p)
{
	*(QRegion *)retval = ((QRegion *)handle)->translated(*(const QPoint*)p);
}

void QRegion_united(QRegionH handle, QRegionH retval, const QRegionH r)
{
	*(QRegion *)retval = ((QRegion *)handle)->united(*(const QRegion*)r);
}

void QRegion_united2(QRegionH handle, QRegionH retval, PRect r)
{
	QRect t_r;
	copyPRectToQRect(r, t_r);
	*(QRegion *)retval = ((QRegion *)handle)->united(t_r);
}

void QRegion_intersected(QRegionH handle, QRegionH retval, const QRegionH r)
{
	*(QRegion *)retval = ((QRegion *)handle)->intersected(*(const QRegion*)r);
}

void QRegion_intersected2(QRegionH handle, QRegionH retval, PRect r)
{
	QRect t_r;
	copyPRectToQRect(r, t_r);
	*(QRegion *)retval = ((QRegion *)handle)->intersected(t_r);
}

void QRegion_subtracted(QRegionH handle, QRegionH retval, const QRegionH r)
{
	*(QRegion *)retval = ((QRegion *)handle)->subtracted(*(const QRegion*)r);
}

void QRegion_xored(QRegionH handle, QRegionH retval, const QRegionH r)
{
	*(QRegion *)retval = ((QRegion *)handle)->xored(*(const QRegion*)r);
}

bool QRegion_intersects(QRegionH handle, const QRegionH r)
{
	return (bool) ((QRegion *)handle)->intersects(*(const QRegion*)r);
}

bool QRegion_intersects2(QRegionH handle, PRect r)
{
	QRect t_r;
	copyPRectToQRect(r, t_r);
	return (bool) ((QRegion *)handle)->intersects(t_r);
}

void QRegion_boundingRect(QRegionH handle, PRect retval)
{
	QRect t_retval;
	t_retval = ((QRegion *)handle)->boundingRect();
	copyQRectToPRect(t_retval, retval);
}

void QRegion_setRects(QRegionH handle, PRect rect, int num)
{
	QRect t_rect;
	if ( rect )
		copyPRectToQRect(rect, t_rect);
	((QRegion *)handle)->setRects(rect ? &t_rect : NULL, num);
}

int QRegion_rectCount(QRegionH handle)
{
	return (int) ((QRegion *)handle)->rectCount();
}

