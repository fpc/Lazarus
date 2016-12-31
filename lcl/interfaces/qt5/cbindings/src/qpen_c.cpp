//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qpen_c.h"

QPenH QPen_Create()
{
	return (QPenH) new QPen();
}

void QPen_Destroy(QPenH handle)
{
	delete (QPen *)handle;
}

QPenH QPen_Create2(Qt::PenStyle AnonParam1)
{
	return (QPenH) new QPen(AnonParam1);
}

QPenH QPen_Create3(const QColorH color)
{
	return (QPenH) new QPen(*(const QColor*)color);
}

QPenH QPen_Create4(const QBrushH brush, qreal width, Qt::PenStyle s, Qt::PenCapStyle c, Qt::PenJoinStyle j)
{
	return (QPenH) new QPen(*(const QBrush*)brush, width, s, c, j);
}

QPenH QPen_Create5(const QPenH pen)
{
	return (QPenH) new QPen(*(const QPen*)pen);
}

void QPen_swap(QPenH handle, QPenH other)
{
	((QPen *)handle)->swap(*(QPen*)other);
}

Qt::PenStyle QPen_style(QPenH handle)
{
	return (Qt::PenStyle) ((QPen *)handle)->style();
}

void QPen_setStyle(QPenH handle, Qt::PenStyle AnonParam1)
{
	((QPen *)handle)->setStyle(AnonParam1);
}

void QPen_dashPattern(QPenH handle, PQRealArray retval)
{
	QVector<qreal> t_retval;
	t_retval = ((QPen *)handle)->dashPattern();
	copyQVectorQRealToQRealArray(t_retval, retval);
}

void QPen_setDashPattern(QPenH handle, PQRealArray pattern)
{
	QVector<qreal> t_pattern;
	copyQRealArrayToQVectorQReal(pattern, t_pattern);
	((QPen *)handle)->setDashPattern(t_pattern);
}

qreal QPen_dashOffset(QPenH handle)
{
	return (qreal) ((QPen *)handle)->dashOffset();
}

void QPen_setDashOffset(QPenH handle, qreal doffset)
{
	((QPen *)handle)->setDashOffset(doffset);
}

qreal QPen_miterLimit(QPenH handle)
{
	return (qreal) ((QPen *)handle)->miterLimit();
}

void QPen_setMiterLimit(QPenH handle, qreal limit)
{
	((QPen *)handle)->setMiterLimit(limit);
}

qreal QPen_widthF(QPenH handle)
{
	return (qreal) ((QPen *)handle)->widthF();
}

void QPen_setWidthF(QPenH handle, qreal width)
{
	((QPen *)handle)->setWidthF(width);
}

int QPen_width(QPenH handle)
{
	return (int) ((QPen *)handle)->width();
}

void QPen_setWidth(QPenH handle, int width)
{
	((QPen *)handle)->setWidth(width);
}

void QPen_color(QPenH handle, PQColor retval)
{
	*(QColor *)retval = ((QPen *)handle)->color();
}

void QPen_setColor(QPenH handle, const QColorH color)
{
	((QPen *)handle)->setColor(*(const QColor*)color);
}

void QPen_brush(QPenH handle, QBrushH retval)
{
	*(QBrush *)retval = ((QPen *)handle)->brush();
}

void QPen_setBrush(QPenH handle, const QBrushH brush)
{
	((QPen *)handle)->setBrush(*(const QBrush*)brush);
}

bool QPen_isSolid(QPenH handle)
{
	return (bool) ((QPen *)handle)->isSolid();
}

Qt::PenCapStyle QPen_capStyle(QPenH handle)
{
	return (Qt::PenCapStyle) ((QPen *)handle)->capStyle();
}

void QPen_setCapStyle(QPenH handle, Qt::PenCapStyle pcs)
{
	((QPen *)handle)->setCapStyle(pcs);
}

Qt::PenJoinStyle QPen_joinStyle(QPenH handle)
{
	return (Qt::PenJoinStyle) ((QPen *)handle)->joinStyle();
}

void QPen_setJoinStyle(QPenH handle, Qt::PenJoinStyle pcs)
{
	((QPen *)handle)->setJoinStyle(pcs);
}

bool QPen_isCosmetic(QPenH handle)
{
	return (bool) ((QPen *)handle)->isCosmetic();
}

void QPen_setCosmetic(QPenH handle, bool cosmetic)
{
	((QPen *)handle)->setCosmetic(cosmetic);
}

bool QPen_isDetached(QPenH handle)
{
	return (bool) ((QPen *)handle)->isDetached();
}

