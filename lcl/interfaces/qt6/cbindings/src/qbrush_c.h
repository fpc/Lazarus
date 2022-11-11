//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QBRUSH_C_H
#define QBRUSH_C_H

#include <QtGui>
#include "pascalbind.h"

C_EXPORT QBrushH QBrush_Create();
C_EXPORT void QBrush_Destroy(QBrushH handle);
C_EXPORT QBrushH QBrush_Create2(Qt::BrushStyle bs);
C_EXPORT QBrushH QBrush_Create3(const QColorH color, Qt::BrushStyle bs);
C_EXPORT QBrushH QBrush_Create4(Qt::GlobalColor color, Qt::BrushStyle bs);
C_EXPORT QBrushH QBrush_Create5(const QColorH color, const QPixmapH pixmap);
C_EXPORT QBrushH QBrush_Create6(Qt::GlobalColor color, const QPixmapH pixmap);
C_EXPORT QBrushH QBrush_Create7(const QPixmapH pixmap);
C_EXPORT QBrushH QBrush_Create8(const QImageH image);
C_EXPORT QBrushH QBrush_Create9(const QBrushH brush);
C_EXPORT QBrushH QBrush_Create10(const QGradientH gradient);
C_EXPORT void QBrush_swap(QBrushH handle, QBrushH other);
C_EXPORT Qt::BrushStyle QBrush_style(QBrushH handle);
C_EXPORT void QBrush_setStyle(QBrushH handle, Qt::BrushStyle AnonParam1);
C_EXPORT void QBrush_transform(QBrushH handle, QTransformH retval);
C_EXPORT void QBrush_setTransform(QBrushH handle, const QTransformH AnonParam1);
C_EXPORT void QBrush_texture(QBrushH handle, QPixmapH retval);
C_EXPORT void QBrush_setTexture(QBrushH handle, const QPixmapH pixmap);
C_EXPORT void QBrush_textureImage(QBrushH handle, QImageH retval);
C_EXPORT void QBrush_setTextureImage(QBrushH handle, const QImageH image);
C_EXPORT const QColorH QBrush_color(QBrushH handle);
C_EXPORT void QBrush_setColor(QBrushH handle, const QColorH color);
C_EXPORT void QBrush_setColor2(QBrushH handle, Qt::GlobalColor color);
C_EXPORT const QGradientH QBrush_gradient(QBrushH handle);
C_EXPORT bool QBrush_isOpaque(QBrushH handle);
C_EXPORT bool QBrush_isDetached(QBrushH handle);
C_EXPORT QGradientH QGradient_Create();
C_EXPORT void QGradient_Destroy(QGradientH handle);
C_EXPORT QGradient::Type QGradient_type(QGradientH handle);
C_EXPORT void QGradient_setSpread(QGradientH handle, QGradient::Spread spread);
C_EXPORT QGradient::Spread QGradient_spread(QGradientH handle);
C_EXPORT void QGradient_setColorAt(QGradientH handle, qreal pos, const QColorH color);
C_EXPORT QGradient::CoordinateMode QGradient_coordinateMode(QGradientH handle);
C_EXPORT void QGradient_setCoordinateMode(QGradientH handle, QGradient::CoordinateMode mode);
C_EXPORT QGradient::InterpolationMode QGradient_interpolationMode(QGradientH handle);
C_EXPORT void QGradient_setInterpolationMode(QGradientH handle, QGradient::InterpolationMode mode);
C_EXPORT QLinearGradientH QLinearGradient_Create();
C_EXPORT void QLinearGradient_Destroy(QLinearGradientH handle);
C_EXPORT QLinearGradientH QLinearGradient_Create2(const QPointFH start, const QPointFH finalStop);
C_EXPORT QLinearGradientH QLinearGradient_Create3(qreal xStart, qreal yStart, qreal xFinalStop, qreal yFinalStop);
C_EXPORT void QLinearGradient_start(QLinearGradientH handle, PQtPointF retval);
C_EXPORT void QLinearGradient_setStart(QLinearGradientH handle, const QPointFH start);
C_EXPORT void QLinearGradient_setStart2(QLinearGradientH handle, qreal x, qreal y);
C_EXPORT void QLinearGradient_finalStop(QLinearGradientH handle, PQtPointF retval);
C_EXPORT void QLinearGradient_setFinalStop(QLinearGradientH handle, const QPointFH stop);
C_EXPORT void QLinearGradient_setFinalStop2(QLinearGradientH handle, qreal x, qreal y);
C_EXPORT QRadialGradientH QRadialGradient_Create();
C_EXPORT void QRadialGradient_Destroy(QRadialGradientH handle);
C_EXPORT QRadialGradientH QRadialGradient_Create2(const QPointFH center, qreal radius, const QPointFH focalPoint);
C_EXPORT QRadialGradientH QRadialGradient_Create3(qreal cx, qreal cy, qreal radius, qreal fx, qreal fy);
C_EXPORT QRadialGradientH QRadialGradient_Create4(const QPointFH center, qreal radius);
C_EXPORT QRadialGradientH QRadialGradient_Create5(qreal cx, qreal cy, qreal radius);
C_EXPORT QRadialGradientH QRadialGradient_Create6(const QPointFH center, qreal centerRadius, const QPointFH focalPoint, qreal focalRadius);
C_EXPORT QRadialGradientH QRadialGradient_Create7(qreal cx, qreal cy, qreal centerRadius, qreal fx, qreal fy, qreal focalRadius);
C_EXPORT void QRadialGradient_center(QRadialGradientH handle, PQtPointF retval);
C_EXPORT void QRadialGradient_setCenter(QRadialGradientH handle, const QPointFH center);
C_EXPORT void QRadialGradient_setCenter2(QRadialGradientH handle, qreal x, qreal y);
C_EXPORT void QRadialGradient_focalPoint(QRadialGradientH handle, PQtPointF retval);
C_EXPORT void QRadialGradient_setFocalPoint(QRadialGradientH handle, const QPointFH focalPoint);
C_EXPORT void QRadialGradient_setFocalPoint2(QRadialGradientH handle, qreal x, qreal y);
C_EXPORT qreal QRadialGradient_radius(QRadialGradientH handle);
C_EXPORT void QRadialGradient_setRadius(QRadialGradientH handle, qreal radius);
C_EXPORT qreal QRadialGradient_centerRadius(QRadialGradientH handle);
C_EXPORT void QRadialGradient_setCenterRadius(QRadialGradientH handle, qreal radius);
C_EXPORT qreal QRadialGradient_focalRadius(QRadialGradientH handle);
C_EXPORT void QRadialGradient_setFocalRadius(QRadialGradientH handle, qreal radius);
C_EXPORT QConicalGradientH QConicalGradient_Create();
C_EXPORT void QConicalGradient_Destroy(QConicalGradientH handle);
C_EXPORT QConicalGradientH QConicalGradient_Create2(const QPointFH center, qreal startAngle);
C_EXPORT QConicalGradientH QConicalGradient_Create3(qreal cx, qreal cy, qreal startAngle);
C_EXPORT void QConicalGradient_center(QConicalGradientH handle, PQtPointF retval);
C_EXPORT void QConicalGradient_setCenter(QConicalGradientH handle, const QPointFH center);
C_EXPORT void QConicalGradient_setCenter2(QConicalGradientH handle, qreal x, qreal y);
C_EXPORT qreal QConicalGradient_angle(QConicalGradientH handle);
C_EXPORT void QConicalGradient_setAngle(QConicalGradientH handle, qreal angle);

#endif
