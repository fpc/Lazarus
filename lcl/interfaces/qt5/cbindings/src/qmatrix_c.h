//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QMATRIX_C_H
#define QMATRIX_C_H

#include <QtGui>
#include "pascalbind.h"

C_EXPORT QMatrixH QMatrix_Create(Qt::Initialization AnonParam1);
C_EXPORT void QMatrix_Destroy(QMatrixH handle);
C_EXPORT QMatrixH QMatrix_Create2();
C_EXPORT QMatrixH QMatrix_Create3(qreal m11, qreal m12, qreal m21, qreal m22, qreal dx, qreal dy);
C_EXPORT QMatrixH QMatrix_Create4(const QMatrixH matrix);
C_EXPORT void QMatrix_setMatrix(QMatrixH handle, qreal m11, qreal m12, qreal m21, qreal m22, qreal dx, qreal dy);
C_EXPORT qreal QMatrix_m11(QMatrixH handle);
C_EXPORT qreal QMatrix_m12(QMatrixH handle);
C_EXPORT qreal QMatrix_m21(QMatrixH handle);
C_EXPORT qreal QMatrix_m22(QMatrixH handle);
C_EXPORT qreal QMatrix_dx(QMatrixH handle);
C_EXPORT qreal QMatrix_dy(QMatrixH handle);
C_EXPORT void QMatrix_map(QMatrixH handle, int x, int y, int* tx, int* ty);
C_EXPORT void QMatrix_map2(QMatrixH handle, qreal x, qreal y, qreal* tx, qreal* ty);
C_EXPORT void QMatrix_mapRect(QMatrixH handle, PRect retval, PRect AnonParam1);
C_EXPORT void QMatrix_mapRect2(QMatrixH handle, QRectFH retval, const QRectFH AnonParam1);
C_EXPORT void QMatrix_map3(QMatrixH handle, PQtPoint retval, const QPointH p);
C_EXPORT void QMatrix_map4(QMatrixH handle, PQtPointF retval, const QPointFH p);
C_EXPORT void QMatrix_map5(QMatrixH handle, QLineH retval, const QLineH l);
C_EXPORT void QMatrix_map6(QMatrixH handle, QLineFH retval, const QLineFH l);
C_EXPORT void QMatrix_map7(QMatrixH handle, QPolygonFH retval, const QPolygonFH a);
C_EXPORT void QMatrix_map8(QMatrixH handle, QPolygonH retval, const QPolygonH a);
C_EXPORT void QMatrix_map9(QMatrixH handle, QRegionH retval, const QRegionH r);
C_EXPORT void QMatrix_map10(QMatrixH handle, QPainterPathH retval, const QPainterPathH p);
C_EXPORT void QMatrix_mapToPolygon(QMatrixH handle, QPolygonH retval, PRect r);
C_EXPORT void QMatrix_reset(QMatrixH handle);
C_EXPORT bool QMatrix_isIdentity(QMatrixH handle);
C_EXPORT QMatrixH QMatrix_translate(QMatrixH handle, qreal dx, qreal dy);
C_EXPORT QMatrixH QMatrix_scale(QMatrixH handle, qreal sx, qreal sy);
C_EXPORT QMatrixH QMatrix_shear(QMatrixH handle, qreal sh, qreal sv);
C_EXPORT QMatrixH QMatrix_rotate(QMatrixH handle, qreal a);
C_EXPORT bool QMatrix_isInvertible(QMatrixH handle);
C_EXPORT qreal QMatrix_determinant(QMatrixH handle);
C_EXPORT void QMatrix_inverted(QMatrixH handle, QMatrixH retval, bool* invertible);

#endif
