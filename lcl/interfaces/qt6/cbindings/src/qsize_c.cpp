//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qsize_c.h"

QSizeH QSize_Create()
{
	return (QSizeH) new QSize();
}

void QSize_Destroy(QSizeH handle)
{
	delete (QSize *)handle;
}

QSizeH QSize_Create2(int w, int h)
{
	return (QSizeH) new QSize(w, h);
}

bool QSize_isNull(QSizeH handle)
{
	return (bool) ((QSize *)handle)->isNull();
}

bool QSize_isEmpty(QSizeH handle)
{
	return (bool) ((QSize *)handle)->isEmpty();
}

bool QSize_isValid(QSizeH handle)
{
	return (bool) ((QSize *)handle)->isValid();
}

int QSize_width(QSizeH handle)
{
	return (int) ((QSize *)handle)->width();
}

int QSize_height(QSizeH handle)
{
	return (int) ((QSize *)handle)->height();
}

void QSize_setWidth(QSizeH handle, int w)
{
	((QSize *)handle)->setWidth(w);
}

void QSize_setHeight(QSizeH handle, int h)
{
	((QSize *)handle)->setHeight(h);
}

void QSize_transpose(QSizeH handle)
{
	((QSize *)handle)->transpose();
}

void QSize_transposed(QSizeH handle, PSize retval)
{
	*(QSize *)retval = ((QSize *)handle)->transposed();
}

void QSize_scale(QSizeH handle, int w, int h, Qt::AspectRatioMode mode)
{
	((QSize *)handle)->scale(w, h, mode);
}

void QSize_scale2(QSizeH handle, const QSizeH s, Qt::AspectRatioMode mode)
{
	((QSize *)handle)->scale(*(const QSize*)s, mode);
}

void QSize_scaled(QSizeH handle, PSize retval, int w, int h, Qt::AspectRatioMode mode)
{
	*(QSize *)retval = ((QSize *)handle)->scaled(w, h, mode);
}

void QSize_scaled2(QSizeH handle, PSize retval, const QSizeH s, Qt::AspectRatioMode mode)
{
	*(QSize *)retval = ((QSize *)handle)->scaled(*(const QSize*)s, mode);
}

void QSize_expandedTo(QSizeH handle, PSize retval, const QSizeH AnonParam1)
{
	*(QSize *)retval = ((QSize *)handle)->expandedTo(*(const QSize*)AnonParam1);
}

void QSize_boundedTo(QSizeH handle, PSize retval, const QSizeH AnonParam1)
{
	*(QSize *)retval = ((QSize *)handle)->boundedTo(*(const QSize*)AnonParam1);
}

int* QSize_rwidth(QSizeH handle)
{
	return (int*) &((QSize *)handle)->rwidth();
}

int* QSize_rheight(QSizeH handle)
{
	return (int*) &((QSize *)handle)->rheight();
}

QSizeFH QSizeF_Create()
{
	return (QSizeFH) new QSizeF();
}

void QSizeF_Destroy(QSizeFH handle)
{
	delete (QSizeF *)handle;
}

QSizeFH QSizeF_Create2(const QSizeH sz)
{
	return (QSizeFH) new QSizeF(*(const QSize*)sz);
}

QSizeFH QSizeF_Create3(qreal w, qreal h)
{
	return (QSizeFH) new QSizeF(w, h);
}

bool QSizeF_isNull(QSizeFH handle)
{
	return (bool) ((QSizeF *)handle)->isNull();
}

bool QSizeF_isEmpty(QSizeFH handle)
{
	return (bool) ((QSizeF *)handle)->isEmpty();
}

bool QSizeF_isValid(QSizeFH handle)
{
	return (bool) ((QSizeF *)handle)->isValid();
}

qreal QSizeF_width(QSizeFH handle)
{
	return (qreal) ((QSizeF *)handle)->width();
}

qreal QSizeF_height(QSizeFH handle)
{
	return (qreal) ((QSizeF *)handle)->height();
}

void QSizeF_setWidth(QSizeFH handle, qreal w)
{
	((QSizeF *)handle)->setWidth(w);
}

void QSizeF_setHeight(QSizeFH handle, qreal h)
{
	((QSizeF *)handle)->setHeight(h);
}

void QSizeF_transpose(QSizeFH handle)
{
	((QSizeF *)handle)->transpose();
}

void QSizeF_transposed(QSizeFH handle, QSizeFH retval)
{
	*(QSizeF *)retval = ((QSizeF *)handle)->transposed();
}

void QSizeF_scale(QSizeFH handle, qreal w, qreal h, Qt::AspectRatioMode mode)
{
	((QSizeF *)handle)->scale(w, h, mode);
}

void QSizeF_scale2(QSizeFH handle, const QSizeFH s, Qt::AspectRatioMode mode)
{
	((QSizeF *)handle)->scale(*(const QSizeF*)s, mode);
}

void QSizeF_scaled(QSizeFH handle, QSizeFH retval, qreal w, qreal h, Qt::AspectRatioMode mode)
{
	*(QSizeF *)retval = ((QSizeF *)handle)->scaled(w, h, mode);
}

void QSizeF_scaled2(QSizeFH handle, QSizeFH retval, const QSizeFH s, Qt::AspectRatioMode mode)
{
	*(QSizeF *)retval = ((QSizeF *)handle)->scaled(*(const QSizeF*)s, mode);
}

void QSizeF_expandedTo(QSizeFH handle, QSizeFH retval, const QSizeFH AnonParam1)
{
	*(QSizeF *)retval = ((QSizeF *)handle)->expandedTo(*(const QSizeF*)AnonParam1);
}

void QSizeF_boundedTo(QSizeFH handle, QSizeFH retval, const QSizeFH AnonParam1)
{
	*(QSizeF *)retval = ((QSizeF *)handle)->boundedTo(*(const QSizeF*)AnonParam1);
}

qreal* QSizeF_rwidth(QSizeFH handle)
{
	return (qreal*) &((QSizeF *)handle)->rwidth();
}

qreal* QSizeF_rheight(QSizeFH handle)
{
	return (qreal*) &((QSizeF *)handle)->rheight();
}

void QSizeF_toSize(QSizeFH handle, PSize retval)
{
	*(QSize *)retval = ((QSizeF *)handle)->toSize();
}

