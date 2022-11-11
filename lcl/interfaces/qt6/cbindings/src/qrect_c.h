//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QRECT_C_H
#define QRECT_C_H

#include <QtCore>
#include "pascalbind.h"

C_EXPORT QRectH QRect_Create();
C_EXPORT void QRect_Destroy(QRectH handle);
C_EXPORT QRectH QRect_Create2(const QPointH topleft, const QPointH bottomright);
C_EXPORT QRectH QRect_Create3(const QPointH topleft, const QSizeH size);
C_EXPORT QRectH QRect_Create4(int left, int top, int width, int height);
C_EXPORT bool QRect_isNull(QRectH handle);
C_EXPORT bool QRect_isEmpty(QRectH handle);
C_EXPORT bool QRect_isValid(QRectH handle);
C_EXPORT int QRect_left(QRectH handle);
C_EXPORT int QRect_top(QRectH handle);
C_EXPORT int QRect_right(QRectH handle);
C_EXPORT int QRect_bottom(QRectH handle);
C_EXPORT void QRect_normalized(QRectH handle, PRect retval);
C_EXPORT int QRect_x(QRectH handle);
C_EXPORT int QRect_y(QRectH handle);
C_EXPORT void QRect_setLeft(QRectH handle, int pos);
C_EXPORT void QRect_setTop(QRectH handle, int pos);
C_EXPORT void QRect_setRight(QRectH handle, int pos);
C_EXPORT void QRect_setBottom(QRectH handle, int pos);
C_EXPORT void QRect_setX(QRectH handle, int x);
C_EXPORT void QRect_setY(QRectH handle, int y);
C_EXPORT void QRect_setTopLeft(QRectH handle, const QPointH p);
C_EXPORT void QRect_setBottomRight(QRectH handle, const QPointH p);
C_EXPORT void QRect_setTopRight(QRectH handle, const QPointH p);
C_EXPORT void QRect_setBottomLeft(QRectH handle, const QPointH p);
C_EXPORT void QRect_topLeft(QRectH handle, PQtPoint retval);
C_EXPORT void QRect_bottomRight(QRectH handle, PQtPoint retval);
C_EXPORT void QRect_topRight(QRectH handle, PQtPoint retval);
C_EXPORT void QRect_bottomLeft(QRectH handle, PQtPoint retval);
C_EXPORT void QRect_center(QRectH handle, PQtPoint retval);
C_EXPORT void QRect_moveLeft(QRectH handle, int pos);
C_EXPORT void QRect_moveTop(QRectH handle, int pos);
C_EXPORT void QRect_moveRight(QRectH handle, int pos);
C_EXPORT void QRect_moveBottom(QRectH handle, int pos);
C_EXPORT void QRect_moveTopLeft(QRectH handle, const QPointH p);
C_EXPORT void QRect_moveBottomRight(QRectH handle, const QPointH p);
C_EXPORT void QRect_moveTopRight(QRectH handle, const QPointH p);
C_EXPORT void QRect_moveBottomLeft(QRectH handle, const QPointH p);
C_EXPORT void QRect_moveCenter(QRectH handle, const QPointH p);
C_EXPORT void QRect_translate(QRectH handle, int dx, int dy);
C_EXPORT void QRect_translate2(QRectH handle, const QPointH p);
C_EXPORT void QRect_translated(QRectH handle, PRect retval, int dx, int dy);
C_EXPORT void QRect_translated2(QRectH handle, PRect retval, const QPointH p);
C_EXPORT void QRect_moveTo(QRectH handle, int x, int t);
C_EXPORT void QRect_moveTo2(QRectH handle, const QPointH p);
C_EXPORT void QRect_setRect(QRectH handle, int x, int y, int w, int h);
C_EXPORT void QRect_getRect(QRectH handle, int* x, int* y, int* w, int* h);
C_EXPORT void QRect_setCoords(QRectH handle, int x1, int y1, int x2, int y2);
C_EXPORT void QRect_getCoords(QRectH handle, int* x1, int* y1, int* x2, int* y2);
C_EXPORT void QRect_adjust(QRectH handle, int x1, int y1, int x2, int y2);
C_EXPORT void QRect_adjusted(QRectH handle, PRect retval, int x1, int y1, int x2, int y2);
C_EXPORT void QRect_size(QRectH handle, PSize retval);
C_EXPORT int QRect_width(QRectH handle);
C_EXPORT int QRect_height(QRectH handle);
C_EXPORT void QRect_setWidth(QRectH handle, int w);
C_EXPORT void QRect_setHeight(QRectH handle, int h);
C_EXPORT void QRect_setSize(QRectH handle, const QSizeH s);
C_EXPORT bool QRect_contains(QRectH handle, PRect r, bool proper);
C_EXPORT bool QRect_contains2(QRectH handle, const QPointH p, bool proper);
C_EXPORT bool QRect_contains3(QRectH handle, int x, int y);
C_EXPORT bool QRect_contains4(QRectH handle, int x, int y, bool proper);
C_EXPORT void QRect_united(QRectH handle, PRect retval, PRect other);
C_EXPORT void QRect_intersected(QRectH handle, PRect retval, PRect other);
C_EXPORT bool QRect_intersects(QRectH handle, PRect r);
C_EXPORT void QRect_marginsAdded(QRectH handle, PRect retval, const QMarginsH margins);
C_EXPORT void QRect_marginsRemoved(QRectH handle, PRect retval, const QMarginsH margins);
C_EXPORT QRectFH QRectF_Create();
C_EXPORT void QRectF_Destroy(QRectFH handle);
C_EXPORT QRectFH QRectF_Create2(const QPointFH topleft, const QSizeFH size);
C_EXPORT QRectFH QRectF_Create3(const QPointFH topleft, const QPointFH bottomRight);
C_EXPORT QRectFH QRectF_Create4(qreal left, qreal top, qreal width, qreal height);
C_EXPORT QRectFH QRectF_Create5(PRect rect);
C_EXPORT bool QRectF_isNull(QRectFH handle);
C_EXPORT bool QRectF_isEmpty(QRectFH handle);
C_EXPORT bool QRectF_isValid(QRectFH handle);
C_EXPORT void QRectF_normalized(QRectFH handle, QRectFH retval);
C_EXPORT qreal QRectF_left(QRectFH handle);
C_EXPORT qreal QRectF_top(QRectFH handle);
C_EXPORT qreal QRectF_right(QRectFH handle);
C_EXPORT qreal QRectF_bottom(QRectFH handle);
C_EXPORT qreal QRectF_x(QRectFH handle);
C_EXPORT qreal QRectF_y(QRectFH handle);
C_EXPORT void QRectF_setLeft(QRectFH handle, qreal pos);
C_EXPORT void QRectF_setTop(QRectFH handle, qreal pos);
C_EXPORT void QRectF_setRight(QRectFH handle, qreal pos);
C_EXPORT void QRectF_setBottom(QRectFH handle, qreal pos);
C_EXPORT void QRectF_setX(QRectFH handle, qreal pos);
C_EXPORT void QRectF_setY(QRectFH handle, qreal pos);
C_EXPORT void QRectF_topLeft(QRectFH handle, PQtPointF retval);
C_EXPORT void QRectF_bottomRight(QRectFH handle, PQtPointF retval);
C_EXPORT void QRectF_topRight(QRectFH handle, PQtPointF retval);
C_EXPORT void QRectF_bottomLeft(QRectFH handle, PQtPointF retval);
C_EXPORT void QRectF_center(QRectFH handle, PQtPointF retval);
C_EXPORT void QRectF_setTopLeft(QRectFH handle, const QPointFH p);
C_EXPORT void QRectF_setBottomRight(QRectFH handle, const QPointFH p);
C_EXPORT void QRectF_setTopRight(QRectFH handle, const QPointFH p);
C_EXPORT void QRectF_setBottomLeft(QRectFH handle, const QPointFH p);
C_EXPORT void QRectF_moveLeft(QRectFH handle, qreal pos);
C_EXPORT void QRectF_moveTop(QRectFH handle, qreal pos);
C_EXPORT void QRectF_moveRight(QRectFH handle, qreal pos);
C_EXPORT void QRectF_moveBottom(QRectFH handle, qreal pos);
C_EXPORT void QRectF_moveTopLeft(QRectFH handle, const QPointFH p);
C_EXPORT void QRectF_moveBottomRight(QRectFH handle, const QPointFH p);
C_EXPORT void QRectF_moveTopRight(QRectFH handle, const QPointFH p);
C_EXPORT void QRectF_moveBottomLeft(QRectFH handle, const QPointFH p);
C_EXPORT void QRectF_moveCenter(QRectFH handle, const QPointFH p);
C_EXPORT void QRectF_translate(QRectFH handle, qreal dx, qreal dy);
C_EXPORT void QRectF_translate2(QRectFH handle, const QPointFH p);
C_EXPORT void QRectF_translated(QRectFH handle, QRectFH retval, qreal dx, qreal dy);
C_EXPORT void QRectF_translated2(QRectFH handle, QRectFH retval, const QPointFH p);
C_EXPORT void QRectF_moveTo(QRectFH handle, qreal x, qreal t);
C_EXPORT void QRectF_moveTo2(QRectFH handle, const QPointFH p);
C_EXPORT void QRectF_setRect(QRectFH handle, qreal x, qreal y, qreal w, qreal h);
C_EXPORT void QRectF_getRect(QRectFH handle, qreal* x, qreal* y, qreal* w, qreal* h);
C_EXPORT void QRectF_setCoords(QRectFH handle, qreal x1, qreal y1, qreal x2, qreal y2);
C_EXPORT void QRectF_getCoords(QRectFH handle, qreal* x1, qreal* y1, qreal* x2, qreal* y2);
C_EXPORT void QRectF_adjust(QRectFH handle, qreal x1, qreal y1, qreal x2, qreal y2);
C_EXPORT void QRectF_adjusted(QRectFH handle, QRectFH retval, qreal x1, qreal y1, qreal x2, qreal y2);
C_EXPORT void QRectF_size(QRectFH handle, QSizeFH retval);
C_EXPORT qreal QRectF_width(QRectFH handle);
C_EXPORT qreal QRectF_height(QRectFH handle);
C_EXPORT void QRectF_setWidth(QRectFH handle, qreal w);
C_EXPORT void QRectF_setHeight(QRectFH handle, qreal h);
C_EXPORT void QRectF_setSize(QRectFH handle, const QSizeFH s);
C_EXPORT bool QRectF_contains(QRectFH handle, const QRectFH r);
C_EXPORT bool QRectF_contains2(QRectFH handle, const QPointFH p);
C_EXPORT bool QRectF_contains3(QRectFH handle, qreal x, qreal y);
C_EXPORT void QRectF_united(QRectFH handle, QRectFH retval, const QRectFH other);
C_EXPORT void QRectF_intersected(QRectFH handle, QRectFH retval, const QRectFH other);
C_EXPORT bool QRectF_intersects(QRectFH handle, const QRectFH r);
C_EXPORT void QRectF_toRect(QRectFH handle, PRect retval);
C_EXPORT void QRectF_toAlignedRect(QRectFH handle, PRect retval);

#endif
