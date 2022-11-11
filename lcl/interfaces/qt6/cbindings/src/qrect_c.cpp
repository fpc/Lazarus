//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qrect_c.h"

QRectH QRect_Create()
{
	return (QRectH) new QRect();
}

void QRect_Destroy(QRectH handle)
{
	delete (QRect *)handle;
}

QRectH QRect_Create2(const QPointH topleft, const QPointH bottomright)
{
	return (QRectH) new QRect(*(const QPoint*)topleft, *(const QPoint*)bottomright);
}

QRectH QRect_Create3(const QPointH topleft, const QSizeH size)
{
	return (QRectH) new QRect(*(const QPoint*)topleft, *(const QSize*)size);
}

QRectH QRect_Create4(int left, int top, int width, int height)
{
	return (QRectH) new QRect(left, top, width, height);
}

bool QRect_isNull(QRectH handle)
{
	return (bool) ((QRect *)handle)->isNull();
}

bool QRect_isEmpty(QRectH handle)
{
	return (bool) ((QRect *)handle)->isEmpty();
}

bool QRect_isValid(QRectH handle)
{
	return (bool) ((QRect *)handle)->isValid();
}

int QRect_left(QRectH handle)
{
	return (int) ((QRect *)handle)->left();
}

int QRect_top(QRectH handle)
{
	return (int) ((QRect *)handle)->top();
}

int QRect_right(QRectH handle)
{
	return (int) ((QRect *)handle)->right();
}

int QRect_bottom(QRectH handle)
{
	return (int) ((QRect *)handle)->bottom();
}

void QRect_normalized(QRectH handle, PRect retval)
{
	QRect t_retval;
	t_retval = ((QRect *)handle)->normalized();
	copyQRectToPRect(t_retval, retval);
}

int QRect_x(QRectH handle)
{
	return (int) ((QRect *)handle)->x();
}

int QRect_y(QRectH handle)
{
	return (int) ((QRect *)handle)->y();
}

void QRect_setLeft(QRectH handle, int pos)
{
	((QRect *)handle)->setLeft(pos);
}

void QRect_setTop(QRectH handle, int pos)
{
	((QRect *)handle)->setTop(pos);
}

void QRect_setRight(QRectH handle, int pos)
{
	((QRect *)handle)->setRight(pos);
}

void QRect_setBottom(QRectH handle, int pos)
{
	((QRect *)handle)->setBottom(pos);
}

void QRect_setX(QRectH handle, int x)
{
	((QRect *)handle)->setX(x);
}

void QRect_setY(QRectH handle, int y)
{
	((QRect *)handle)->setY(y);
}

void QRect_setTopLeft(QRectH handle, const QPointH p)
{
	((QRect *)handle)->setTopLeft(*(const QPoint*)p);
}

void QRect_setBottomRight(QRectH handle, const QPointH p)
{
	((QRect *)handle)->setBottomRight(*(const QPoint*)p);
}

void QRect_setTopRight(QRectH handle, const QPointH p)
{
	((QRect *)handle)->setTopRight(*(const QPoint*)p);
}

void QRect_setBottomLeft(QRectH handle, const QPointH p)
{
	((QRect *)handle)->setBottomLeft(*(const QPoint*)p);
}

void QRect_topLeft(QRectH handle, PQtPoint retval)
{
	*(QPoint *)retval = ((QRect *)handle)->topLeft();
}

void QRect_bottomRight(QRectH handle, PQtPoint retval)
{
	*(QPoint *)retval = ((QRect *)handle)->bottomRight();
}

void QRect_topRight(QRectH handle, PQtPoint retval)
{
	*(QPoint *)retval = ((QRect *)handle)->topRight();
}

void QRect_bottomLeft(QRectH handle, PQtPoint retval)
{
	*(QPoint *)retval = ((QRect *)handle)->bottomLeft();
}

void QRect_center(QRectH handle, PQtPoint retval)
{
	*(QPoint *)retval = ((QRect *)handle)->center();
}

void QRect_moveLeft(QRectH handle, int pos)
{
	((QRect *)handle)->moveLeft(pos);
}

void QRect_moveTop(QRectH handle, int pos)
{
	((QRect *)handle)->moveTop(pos);
}

void QRect_moveRight(QRectH handle, int pos)
{
	((QRect *)handle)->moveRight(pos);
}

void QRect_moveBottom(QRectH handle, int pos)
{
	((QRect *)handle)->moveBottom(pos);
}

void QRect_moveTopLeft(QRectH handle, const QPointH p)
{
	((QRect *)handle)->moveTopLeft(*(const QPoint*)p);
}

void QRect_moveBottomRight(QRectH handle, const QPointH p)
{
	((QRect *)handle)->moveBottomRight(*(const QPoint*)p);
}

void QRect_moveTopRight(QRectH handle, const QPointH p)
{
	((QRect *)handle)->moveTopRight(*(const QPoint*)p);
}

void QRect_moveBottomLeft(QRectH handle, const QPointH p)
{
	((QRect *)handle)->moveBottomLeft(*(const QPoint*)p);
}

void QRect_moveCenter(QRectH handle, const QPointH p)
{
	((QRect *)handle)->moveCenter(*(const QPoint*)p);
}

void QRect_translate(QRectH handle, int dx, int dy)
{
	((QRect *)handle)->translate(dx, dy);
}

void QRect_translate2(QRectH handle, const QPointH p)
{
	((QRect *)handle)->translate(*(const QPoint*)p);
}

void QRect_translated(QRectH handle, PRect retval, int dx, int dy)
{
	QRect t_retval;
	t_retval = ((QRect *)handle)->translated(dx, dy);
	copyQRectToPRect(t_retval, retval);
}

void QRect_translated2(QRectH handle, PRect retval, const QPointH p)
{
	QRect t_retval;
	t_retval = ((QRect *)handle)->translated(*(const QPoint*)p);
	copyQRectToPRect(t_retval, retval);
}

void QRect_moveTo(QRectH handle, int x, int t)
{
	((QRect *)handle)->moveTo(x, t);
}

void QRect_moveTo2(QRectH handle, const QPointH p)
{
	((QRect *)handle)->moveTo(*(const QPoint*)p);
}

void QRect_setRect(QRectH handle, int x, int y, int w, int h)
{
	((QRect *)handle)->setRect(x, y, w, h);
}

void QRect_getRect(QRectH handle, int* x, int* y, int* w, int* h)
{
	((QRect *)handle)->getRect(x, y, w, h);
}

void QRect_setCoords(QRectH handle, int x1, int y1, int x2, int y2)
{
	((QRect *)handle)->setCoords(x1, y1, x2, y2);
}

void QRect_getCoords(QRectH handle, int* x1, int* y1, int* x2, int* y2)
{
	((QRect *)handle)->getCoords(x1, y1, x2, y2);
}

void QRect_adjust(QRectH handle, int x1, int y1, int x2, int y2)
{
	((QRect *)handle)->adjust(x1, y1, x2, y2);
}

void QRect_adjusted(QRectH handle, PRect retval, int x1, int y1, int x2, int y2)
{
	QRect t_retval;
	t_retval = ((QRect *)handle)->adjusted(x1, y1, x2, y2);
	copyQRectToPRect(t_retval, retval);
}

void QRect_size(QRectH handle, PSize retval)
{
	*(QSize *)retval = ((QRect *)handle)->size();
}

int QRect_width(QRectH handle)
{
	return (int) ((QRect *)handle)->width();
}

int QRect_height(QRectH handle)
{
	return (int) ((QRect *)handle)->height();
}

void QRect_setWidth(QRectH handle, int w)
{
	((QRect *)handle)->setWidth(w);
}

void QRect_setHeight(QRectH handle, int h)
{
	((QRect *)handle)->setHeight(h);
}

void QRect_setSize(QRectH handle, const QSizeH s)
{
	((QRect *)handle)->setSize(*(const QSize*)s);
}

bool QRect_contains(QRectH handle, PRect r, bool proper)
{
	QRect t_r;
	copyPRectToQRect(r, t_r);
	return (bool) ((QRect *)handle)->contains(t_r, proper);
}

bool QRect_contains2(QRectH handle, const QPointH p, bool proper)
{
	return (bool) ((QRect *)handle)->contains(*(const QPoint*)p, proper);
}

bool QRect_contains3(QRectH handle, int x, int y)
{
	return (bool) ((QRect *)handle)->contains(x, y);
}

bool QRect_contains4(QRectH handle, int x, int y, bool proper)
{
	return (bool) ((QRect *)handle)->contains(x, y, proper);
}

void QRect_united(QRectH handle, PRect retval, PRect other)
{
	QRect t_retval;
	QRect t_other;
	copyPRectToQRect(other, t_other);
	t_retval = ((QRect *)handle)->united(t_other);
	copyQRectToPRect(t_retval, retval);
}

void QRect_intersected(QRectH handle, PRect retval, PRect other)
{
	QRect t_retval;
	QRect t_other;
	copyPRectToQRect(other, t_other);
	t_retval = ((QRect *)handle)->intersected(t_other);
	copyQRectToPRect(t_retval, retval);
}

bool QRect_intersects(QRectH handle, PRect r)
{
	QRect t_r;
	copyPRectToQRect(r, t_r);
	return (bool) ((QRect *)handle)->intersects(t_r);
}

void QRect_marginsAdded(QRectH handle, PRect retval, const QMarginsH margins)
{
	QRect t_retval;
	t_retval = ((QRect *)handle)->marginsAdded(*(const QMargins*)margins);
	copyQRectToPRect(t_retval, retval);
}

void QRect_marginsRemoved(QRectH handle, PRect retval, const QMarginsH margins)
{
	QRect t_retval;
	t_retval = ((QRect *)handle)->marginsRemoved(*(const QMargins*)margins);
	copyQRectToPRect(t_retval, retval);
}

QRectFH QRectF_Create()
{
	return (QRectFH) new QRectF();
}

void QRectF_Destroy(QRectFH handle)
{
	delete (QRectF *)handle;
}

QRectFH QRectF_Create2(const QPointFH topleft, const QSizeFH size)
{
	return (QRectFH) new QRectF(*(const QPointF*)topleft, *(const QSizeF*)size);
}

QRectFH QRectF_Create3(const QPointFH topleft, const QPointFH bottomRight)
{
	return (QRectFH) new QRectF(*(const QPointF*)topleft, *(const QPointF*)bottomRight);
}

QRectFH QRectF_Create4(qreal left, qreal top, qreal width, qreal height)
{
	return (QRectFH) new QRectF(left, top, width, height);
}

QRectFH QRectF_Create5(PRect rect)
{
	QRect t_rect;
	copyPRectToQRect(rect, t_rect);
	return (QRectFH) new QRectF(t_rect);
}

bool QRectF_isNull(QRectFH handle)
{
	return (bool) ((QRectF *)handle)->isNull();
}

bool QRectF_isEmpty(QRectFH handle)
{
	return (bool) ((QRectF *)handle)->isEmpty();
}

bool QRectF_isValid(QRectFH handle)
{
	return (bool) ((QRectF *)handle)->isValid();
}

void QRectF_normalized(QRectFH handle, QRectFH retval)
{
	*(QRectF *)retval = ((QRectF *)handle)->normalized();
}

qreal QRectF_left(QRectFH handle)
{
	return (qreal) ((QRectF *)handle)->left();
}

qreal QRectF_top(QRectFH handle)
{
	return (qreal) ((QRectF *)handle)->top();
}

qreal QRectF_right(QRectFH handle)
{
	return (qreal) ((QRectF *)handle)->right();
}

qreal QRectF_bottom(QRectFH handle)
{
	return (qreal) ((QRectF *)handle)->bottom();
}

qreal QRectF_x(QRectFH handle)
{
	return (qreal) ((QRectF *)handle)->x();
}

qreal QRectF_y(QRectFH handle)
{
	return (qreal) ((QRectF *)handle)->y();
}

void QRectF_setLeft(QRectFH handle, qreal pos)
{
	((QRectF *)handle)->setLeft(pos);
}

void QRectF_setTop(QRectFH handle, qreal pos)
{
	((QRectF *)handle)->setTop(pos);
}

void QRectF_setRight(QRectFH handle, qreal pos)
{
	((QRectF *)handle)->setRight(pos);
}

void QRectF_setBottom(QRectFH handle, qreal pos)
{
	((QRectF *)handle)->setBottom(pos);
}

void QRectF_setX(QRectFH handle, qreal pos)
{
	((QRectF *)handle)->setX(pos);
}

void QRectF_setY(QRectFH handle, qreal pos)
{
	((QRectF *)handle)->setY(pos);
}

void QRectF_topLeft(QRectFH handle, PQtPointF retval)
{
	*(QPointF *)retval = ((QRectF *)handle)->topLeft();
}

void QRectF_bottomRight(QRectFH handle, PQtPointF retval)
{
	*(QPointF *)retval = ((QRectF *)handle)->bottomRight();
}

void QRectF_topRight(QRectFH handle, PQtPointF retval)
{
	*(QPointF *)retval = ((QRectF *)handle)->topRight();
}

void QRectF_bottomLeft(QRectFH handle, PQtPointF retval)
{
	*(QPointF *)retval = ((QRectF *)handle)->bottomLeft();
}

void QRectF_center(QRectFH handle, PQtPointF retval)
{
	*(QPointF *)retval = ((QRectF *)handle)->center();
}

void QRectF_setTopLeft(QRectFH handle, const QPointFH p)
{
	((QRectF *)handle)->setTopLeft(*(const QPointF*)p);
}

void QRectF_setBottomRight(QRectFH handle, const QPointFH p)
{
	((QRectF *)handle)->setBottomRight(*(const QPointF*)p);
}

void QRectF_setTopRight(QRectFH handle, const QPointFH p)
{
	((QRectF *)handle)->setTopRight(*(const QPointF*)p);
}

void QRectF_setBottomLeft(QRectFH handle, const QPointFH p)
{
	((QRectF *)handle)->setBottomLeft(*(const QPointF*)p);
}

void QRectF_moveLeft(QRectFH handle, qreal pos)
{
	((QRectF *)handle)->moveLeft(pos);
}

void QRectF_moveTop(QRectFH handle, qreal pos)
{
	((QRectF *)handle)->moveTop(pos);
}

void QRectF_moveRight(QRectFH handle, qreal pos)
{
	((QRectF *)handle)->moveRight(pos);
}

void QRectF_moveBottom(QRectFH handle, qreal pos)
{
	((QRectF *)handle)->moveBottom(pos);
}

void QRectF_moveTopLeft(QRectFH handle, const QPointFH p)
{
	((QRectF *)handle)->moveTopLeft(*(const QPointF*)p);
}

void QRectF_moveBottomRight(QRectFH handle, const QPointFH p)
{
	((QRectF *)handle)->moveBottomRight(*(const QPointF*)p);
}

void QRectF_moveTopRight(QRectFH handle, const QPointFH p)
{
	((QRectF *)handle)->moveTopRight(*(const QPointF*)p);
}

void QRectF_moveBottomLeft(QRectFH handle, const QPointFH p)
{
	((QRectF *)handle)->moveBottomLeft(*(const QPointF*)p);
}

void QRectF_moveCenter(QRectFH handle, const QPointFH p)
{
	((QRectF *)handle)->moveCenter(*(const QPointF*)p);
}

void QRectF_translate(QRectFH handle, qreal dx, qreal dy)
{
	((QRectF *)handle)->translate(dx, dy);
}

void QRectF_translate2(QRectFH handle, const QPointFH p)
{
	((QRectF *)handle)->translate(*(const QPointF*)p);
}

void QRectF_translated(QRectFH handle, QRectFH retval, qreal dx, qreal dy)
{
	*(QRectF *)retval = ((QRectF *)handle)->translated(dx, dy);
}

void QRectF_translated2(QRectFH handle, QRectFH retval, const QPointFH p)
{
	*(QRectF *)retval = ((QRectF *)handle)->translated(*(const QPointF*)p);
}

void QRectF_moveTo(QRectFH handle, qreal x, qreal t)
{
	((QRectF *)handle)->moveTo(x, t);
}

void QRectF_moveTo2(QRectFH handle, const QPointFH p)
{
	((QRectF *)handle)->moveTo(*(const QPointF*)p);
}

void QRectF_setRect(QRectFH handle, qreal x, qreal y, qreal w, qreal h)
{
	((QRectF *)handle)->setRect(x, y, w, h);
}

void QRectF_getRect(QRectFH handle, qreal* x, qreal* y, qreal* w, qreal* h)
{
	((QRectF *)handle)->getRect(x, y, w, h);
}

void QRectF_setCoords(QRectFH handle, qreal x1, qreal y1, qreal x2, qreal y2)
{
	((QRectF *)handle)->setCoords(x1, y1, x2, y2);
}

void QRectF_getCoords(QRectFH handle, qreal* x1, qreal* y1, qreal* x2, qreal* y2)
{
	((QRectF *)handle)->getCoords(x1, y1, x2, y2);
}

void QRectF_adjust(QRectFH handle, qreal x1, qreal y1, qreal x2, qreal y2)
{
	((QRectF *)handle)->adjust(x1, y1, x2, y2);
}

void QRectF_adjusted(QRectFH handle, QRectFH retval, qreal x1, qreal y1, qreal x2, qreal y2)
{
	*(QRectF *)retval = ((QRectF *)handle)->adjusted(x1, y1, x2, y2);
}

void QRectF_size(QRectFH handle, QSizeFH retval)
{
	*(QSizeF *)retval = ((QRectF *)handle)->size();
}

qreal QRectF_width(QRectFH handle)
{
	return (qreal) ((QRectF *)handle)->width();
}

qreal QRectF_height(QRectFH handle)
{
	return (qreal) ((QRectF *)handle)->height();
}

void QRectF_setWidth(QRectFH handle, qreal w)
{
	((QRectF *)handle)->setWidth(w);
}

void QRectF_setHeight(QRectFH handle, qreal h)
{
	((QRectF *)handle)->setHeight(h);
}

void QRectF_setSize(QRectFH handle, const QSizeFH s)
{
	((QRectF *)handle)->setSize(*(const QSizeF*)s);
}

bool QRectF_contains(QRectFH handle, const QRectFH r)
{
	return (bool) ((QRectF *)handle)->contains(*(const QRectF*)r);
}

bool QRectF_contains2(QRectFH handle, const QPointFH p)
{
	return (bool) ((QRectF *)handle)->contains(*(const QPointF*)p);
}

bool QRectF_contains3(QRectFH handle, qreal x, qreal y)
{
	return (bool) ((QRectF *)handle)->contains(x, y);
}

void QRectF_united(QRectFH handle, QRectFH retval, const QRectFH other)
{
	*(QRectF *)retval = ((QRectF *)handle)->united(*(const QRectF*)other);
}

void QRectF_intersected(QRectFH handle, QRectFH retval, const QRectFH other)
{
	*(QRectF *)retval = ((QRectF *)handle)->intersected(*(const QRectF*)other);
}

bool QRectF_intersects(QRectFH handle, const QRectFH r)
{
	return (bool) ((QRectF *)handle)->intersects(*(const QRectF*)r);
}

void QRectF_toRect(QRectFH handle, PRect retval)
{
	QRect t_retval;
	t_retval = ((QRectF *)handle)->toRect();
	copyQRectToPRect(t_retval, retval);
}

void QRectF_toAlignedRect(QRectFH handle, PRect retval)
{
	QRect t_retval;
	t_retval = ((QRectF *)handle)->toAlignedRect();
	copyQRectToPRect(t_retval, retval);
}

