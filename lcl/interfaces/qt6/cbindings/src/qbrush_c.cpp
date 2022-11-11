//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qbrush_c.h"

QBrushH QBrush_Create()
{
	return (QBrushH) new QBrush();
}

void QBrush_Destroy(QBrushH handle)
{
	delete (QBrush *)handle;
}

QBrushH QBrush_Create2(Qt::BrushStyle bs)
{
	return (QBrushH) new QBrush(bs);
}

QBrushH QBrush_Create3(const QColorH color, Qt::BrushStyle bs)
{
	return (QBrushH) new QBrush(*(const QColor*)color, bs);
}

QBrushH QBrush_Create4(Qt::GlobalColor color, Qt::BrushStyle bs)
{
	return (QBrushH) new QBrush(color, bs);
}

QBrushH QBrush_Create5(const QColorH color, const QPixmapH pixmap)
{
	return (QBrushH) new QBrush(*(const QColor*)color, *(const QPixmap*)pixmap);
}

QBrushH QBrush_Create6(Qt::GlobalColor color, const QPixmapH pixmap)
{
	return (QBrushH) new QBrush(color, *(const QPixmap*)pixmap);
}

QBrushH QBrush_Create7(const QPixmapH pixmap)
{
	return (QBrushH) new QBrush(*(const QPixmap*)pixmap);
}

QBrushH QBrush_Create8(const QImageH image)
{
	return (QBrushH) new QBrush(*(const QImage*)image);
}

QBrushH QBrush_Create9(const QBrushH brush)
{
	return (QBrushH) new QBrush(*(const QBrush*)brush);
}

QBrushH QBrush_Create10(const QGradientH gradient)
{
	return (QBrushH) new QBrush(*(const QGradient*)gradient);
}

void QBrush_swap(QBrushH handle, QBrushH other)
{
	((QBrush *)handle)->swap(*(QBrush*)other);
}

Qt::BrushStyle QBrush_style(QBrushH handle)
{
	return (Qt::BrushStyle) ((QBrush *)handle)->style();
}

void QBrush_setStyle(QBrushH handle, Qt::BrushStyle AnonParam1)
{
	((QBrush *)handle)->setStyle(AnonParam1);
}

void QBrush_transform(QBrushH handle, QTransformH retval)
{
	*(QTransform *)retval = ((QBrush *)handle)->transform();
}

void QBrush_setTransform(QBrushH handle, const QTransformH AnonParam1)
{
	((QBrush *)handle)->setTransform(*(const QTransform*)AnonParam1);
}

void QBrush_texture(QBrushH handle, QPixmapH retval)
{
	*(QPixmap *)retval = ((QBrush *)handle)->texture();
}

void QBrush_setTexture(QBrushH handle, const QPixmapH pixmap)
{
	((QBrush *)handle)->setTexture(*(const QPixmap*)pixmap);
}

void QBrush_textureImage(QBrushH handle, QImageH retval)
{
	*(QImage *)retval = ((QBrush *)handle)->textureImage();
}

void QBrush_setTextureImage(QBrushH handle, const QImageH image)
{
	((QBrush *)handle)->setTextureImage(*(const QImage*)image);
}

const QColorH QBrush_color(QBrushH handle)
{
	return (const QColorH) &((QBrush *)handle)->color();
}

void QBrush_setColor(QBrushH handle, const QColorH color)
{
	((QBrush *)handle)->setColor(*(const QColor*)color);
}

void QBrush_setColor2(QBrushH handle, Qt::GlobalColor color)
{
	((QBrush *)handle)->setColor(color);
}

const QGradientH QBrush_gradient(QBrushH handle)
{
	return (const QGradientH) ((QBrush *)handle)->gradient();
}

bool QBrush_isOpaque(QBrushH handle)
{
	return (bool) ((QBrush *)handle)->isOpaque();
}

bool QBrush_isDetached(QBrushH handle)
{
	return (bool) ((QBrush *)handle)->isDetached();
}

QGradientH QGradient_Create()
{
	return (QGradientH) new QGradient();
}

void QGradient_Destroy(QGradientH handle)
{
	delete (QGradient *)handle;
}

QGradient::Type QGradient_type(QGradientH handle)
{
	return (QGradient::Type) ((QGradient *)handle)->type();
}

void QGradient_setSpread(QGradientH handle, QGradient::Spread spread)
{
	((QGradient *)handle)->setSpread(spread);
}

QGradient::Spread QGradient_spread(QGradientH handle)
{
	return (QGradient::Spread) ((QGradient *)handle)->spread();
}

void QGradient_setColorAt(QGradientH handle, qreal pos, const QColorH color)
{
	((QGradient *)handle)->setColorAt(pos, *(const QColor*)color);
}

QGradient::CoordinateMode QGradient_coordinateMode(QGradientH handle)
{
	return (QGradient::CoordinateMode) ((QGradient *)handle)->coordinateMode();
}

void QGradient_setCoordinateMode(QGradientH handle, QGradient::CoordinateMode mode)
{
	((QGradient *)handle)->setCoordinateMode(mode);
}

QGradient::InterpolationMode QGradient_interpolationMode(QGradientH handle)
{
	return (QGradient::InterpolationMode) ((QGradient *)handle)->interpolationMode();
}

void QGradient_setInterpolationMode(QGradientH handle, QGradient::InterpolationMode mode)
{
	((QGradient *)handle)->setInterpolationMode(mode);
}

QLinearGradientH QLinearGradient_Create()
{
	return (QLinearGradientH) new QLinearGradient();
}

void QLinearGradient_Destroy(QLinearGradientH handle)
{
	delete (QLinearGradient *)handle;
}

QLinearGradientH QLinearGradient_Create2(const QPointFH start, const QPointFH finalStop)
{
	return (QLinearGradientH) new QLinearGradient(*(const QPointF*)start, *(const QPointF*)finalStop);
}

QLinearGradientH QLinearGradient_Create3(qreal xStart, qreal yStart, qreal xFinalStop, qreal yFinalStop)
{
	return (QLinearGradientH) new QLinearGradient(xStart, yStart, xFinalStop, yFinalStop);
}

void QLinearGradient_start(QLinearGradientH handle, PQtPointF retval)
{
	*(QPointF *)retval = ((QLinearGradient *)handle)->start();
}

void QLinearGradient_setStart(QLinearGradientH handle, const QPointFH start)
{
	((QLinearGradient *)handle)->setStart(*(const QPointF*)start);
}

void QLinearGradient_setStart2(QLinearGradientH handle, qreal x, qreal y)
{
	((QLinearGradient *)handle)->setStart(x, y);
}

void QLinearGradient_finalStop(QLinearGradientH handle, PQtPointF retval)
{
	*(QPointF *)retval = ((QLinearGradient *)handle)->finalStop();
}

void QLinearGradient_setFinalStop(QLinearGradientH handle, const QPointFH stop)
{
	((QLinearGradient *)handle)->setFinalStop(*(const QPointF*)stop);
}

void QLinearGradient_setFinalStop2(QLinearGradientH handle, qreal x, qreal y)
{
	((QLinearGradient *)handle)->setFinalStop(x, y);
}

QRadialGradientH QRadialGradient_Create()
{
	return (QRadialGradientH) new QRadialGradient();
}

void QRadialGradient_Destroy(QRadialGradientH handle)
{
	delete (QRadialGradient *)handle;
}

QRadialGradientH QRadialGradient_Create2(const QPointFH center, qreal radius, const QPointFH focalPoint)
{
	return (QRadialGradientH) new QRadialGradient(*(const QPointF*)center, radius, *(const QPointF*)focalPoint);
}

QRadialGradientH QRadialGradient_Create3(qreal cx, qreal cy, qreal radius, qreal fx, qreal fy)
{
	return (QRadialGradientH) new QRadialGradient(cx, cy, radius, fx, fy);
}

QRadialGradientH QRadialGradient_Create4(const QPointFH center, qreal radius)
{
	return (QRadialGradientH) new QRadialGradient(*(const QPointF*)center, radius);
}

QRadialGradientH QRadialGradient_Create5(qreal cx, qreal cy, qreal radius)
{
	return (QRadialGradientH) new QRadialGradient(cx, cy, radius);
}

QRadialGradientH QRadialGradient_Create6(const QPointFH center, qreal centerRadius, const QPointFH focalPoint, qreal focalRadius)
{
	return (QRadialGradientH) new QRadialGradient(*(const QPointF*)center, centerRadius, *(const QPointF*)focalPoint, focalRadius);
}

QRadialGradientH QRadialGradient_Create7(qreal cx, qreal cy, qreal centerRadius, qreal fx, qreal fy, qreal focalRadius)
{
	return (QRadialGradientH) new QRadialGradient(cx, cy, centerRadius, fx, fy, focalRadius);
}

void QRadialGradient_center(QRadialGradientH handle, PQtPointF retval)
{
	*(QPointF *)retval = ((QRadialGradient *)handle)->center();
}

void QRadialGradient_setCenter(QRadialGradientH handle, const QPointFH center)
{
	((QRadialGradient *)handle)->setCenter(*(const QPointF*)center);
}

void QRadialGradient_setCenter2(QRadialGradientH handle, qreal x, qreal y)
{
	((QRadialGradient *)handle)->setCenter(x, y);
}

void QRadialGradient_focalPoint(QRadialGradientH handle, PQtPointF retval)
{
	*(QPointF *)retval = ((QRadialGradient *)handle)->focalPoint();
}

void QRadialGradient_setFocalPoint(QRadialGradientH handle, const QPointFH focalPoint)
{
	((QRadialGradient *)handle)->setFocalPoint(*(const QPointF*)focalPoint);
}

void QRadialGradient_setFocalPoint2(QRadialGradientH handle, qreal x, qreal y)
{
	((QRadialGradient *)handle)->setFocalPoint(x, y);
}

qreal QRadialGradient_radius(QRadialGradientH handle)
{
	return (qreal) ((QRadialGradient *)handle)->radius();
}

void QRadialGradient_setRadius(QRadialGradientH handle, qreal radius)
{
	((QRadialGradient *)handle)->setRadius(radius);
}

qreal QRadialGradient_centerRadius(QRadialGradientH handle)
{
	return (qreal) ((QRadialGradient *)handle)->centerRadius();
}

void QRadialGradient_setCenterRadius(QRadialGradientH handle, qreal radius)
{
	((QRadialGradient *)handle)->setCenterRadius(radius);
}

qreal QRadialGradient_focalRadius(QRadialGradientH handle)
{
	return (qreal) ((QRadialGradient *)handle)->focalRadius();
}

void QRadialGradient_setFocalRadius(QRadialGradientH handle, qreal radius)
{
	((QRadialGradient *)handle)->setFocalRadius(radius);
}

QConicalGradientH QConicalGradient_Create()
{
	return (QConicalGradientH) new QConicalGradient();
}

void QConicalGradient_Destroy(QConicalGradientH handle)
{
	delete (QConicalGradient *)handle;
}

QConicalGradientH QConicalGradient_Create2(const QPointFH center, qreal startAngle)
{
	return (QConicalGradientH) new QConicalGradient(*(const QPointF*)center, startAngle);
}

QConicalGradientH QConicalGradient_Create3(qreal cx, qreal cy, qreal startAngle)
{
	return (QConicalGradientH) new QConicalGradient(cx, cy, startAngle);
}

void QConicalGradient_center(QConicalGradientH handle, PQtPointF retval)
{
	*(QPointF *)retval = ((QConicalGradient *)handle)->center();
}

void QConicalGradient_setCenter(QConicalGradientH handle, const QPointFH center)
{
	((QConicalGradient *)handle)->setCenter(*(const QPointF*)center);
}

void QConicalGradient_setCenter2(QConicalGradientH handle, qreal x, qreal y)
{
	((QConicalGradient *)handle)->setCenter(x, y);
}

qreal QConicalGradient_angle(QConicalGradientH handle)
{
	return (qreal) ((QConicalGradient *)handle)->angle();
}

void QConicalGradient_setAngle(QConicalGradientH handle, qreal angle)
{
	((QConicalGradient *)handle)->setAngle(angle);
}

