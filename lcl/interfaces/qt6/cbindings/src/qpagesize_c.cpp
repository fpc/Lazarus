//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qpagesize_c.h"

QPageSizeH QPageSize_Create()
{
  return (QPageSizeH) new QPageSize();
}

QPageSizeH QPageSize_Create2(const QPageSizeH other)
{
  return (QPageSizeH) new QPageSize(*(const QPageSize*) other);
}

QPageSizeH QPageSize_Create3(QPageSize::PageSizeId pageSize)
{
  return (QPageSizeH) new QPageSize((QPageSize::PageSizeId) pageSize);
}

QPageSizeH QPageSize_Create4(const QSizeFH size, QPageSize::Unit units, const PWideString name, QPageSize::SizeMatchPolicy matchPolicy)
{
	QString t_s;
	copyPWideStringToQString(name, t_s);
  return (QPageSizeH) new QPageSize(*(const QSizeF*) size, (QPageSize::Unit) units, t_s, (QPageSize::SizeMatchPolicy) matchPolicy);
}

QPageSizeH QPageSize_Create5(const QSizeH pointsize, const PWideString name, QPageSize::SizeMatchPolicy matchPolicy)
{
	QString t_s;
	copyPWideStringToQString(name, t_s);
  return (QPageSizeH) new QPageSize(*(const QSize*) pointsize, t_s, (QPageSize::SizeMatchPolicy) matchPolicy);
}

void QPageSize_Destroy(QPageSizeH handle)
{
  delete (QPageSize *)handle;
}

void QPageSize_definitionSize(QPageSizeH handle, QSizeFH retval)
{
  *(QSizeF *)retval = ((QPageSize *)handle)->definitionSize();
}

QPageSize::Unit QPageSize_definitionUnits(QPageSizeH handle)
{
  return (QPageSize::Unit) ((QPageSize *)handle)->definitionUnits();
}

QPageSize::PageSizeId QPageSize_id(QPageSizeH handle)
{
  return (QPageSize::PageSizeId) ((QPageSize *)handle)->id();
}

bool QPageSize_isEquivalentTo(QPageSizeH handle, const QPageSizeH other)
{
  return (bool) ((QPageSize *)handle)->isEquivalentTo(*(const QPageSize*)other);
}

bool QPageSize_isValid(QPageSizeH handle)
{
  return (bool) ((QPageSize *)handle)->isValid();
}

void QPageSize_key(QPageSizeH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QPageSize *)handle)->key();
  copyQStringToPWideString(t_retval, retval);
}

void QPageSize_name(QPageSizeH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QPageSize *)handle)->name();
  copyQStringToPWideString(t_retval, retval);
}

void QPageSize_rect(QPageSizeH handle, QRectFH retval, QPageSize::Unit units)
{
  *(QRectF *)retval = ((QPageSize *)handle)->rect((QPageSize::Unit)units);
}

void QPageSize_rect2(QPageSizeH handle, PRect retval, QPageSize::Unit units)
{
	QRect t_retval;
  t_retval = ((QPageSize *)handle)->rect((QPageSize::Unit)units).toRect();
	copyQRectToPRect(t_retval, retval);
}

void QPageSize_rectPixels(QPageSizeH handle, PRect retval, int resolution)
{
	QRect t_retval;
	t_retval = ((QPageSize *)handle)->rectPixels(resolution);
	copyQRectToPRect(t_retval, retval);
}

void QPageSize_rectPoints(QPageSizeH handle, PRect retval)
{
	QRect t_retval;
	t_retval = ((QPageSize *)handle)->rectPoints();
	copyQRectToPRect(t_retval, retval);
}

void QPageSize_size(QPageSizeH handle, QSizeFH retval, QPageSize::Unit units)
{
  *(QSizeF *)retval = ((QPageSize *)handle)->size((QPageSize::Unit)units);
}

void QPageSize_sizePixels(QPageSizeH handle, QSizeH retval, int resolution)
{
  *(QSize *)retval = ((QPageSize *)handle)->sizePixels(resolution);
}

void QPageSize_sizePoints(QPageSizeH handle, QSizeH retval)
{
  *(QSize *)retval = ((QPageSize *)handle)->sizePoints();
}

void QPageSize_swap(QPageSizeH handle, QPageSizeH other)
{
  ((QPageSize *)handle)->swap(*(QPageSize *)other);
}

int QPageSize_windowsId(QPageSizeH handle)
{
  return (int) ((QPageSize *)handle)->windowsId();
}

void QPageSize_definitionSize2(QPageSize::PageSizeId pageSizeId, QSizeFH retval)
{
  *(QSizeF *)retval = QPageSize::definitionSize((QPageSize::PageSizeId)pageSizeId);
}

QPageSize::Unit QPageSize_definitionUnits2(QPageSize::PageSizeId pageSizeId)
{
  return (QPageSize::Unit) QPageSize::definitionUnits((QPageSize::PageSizeId)pageSizeId);
}

QPageSize::PageSizeId QPageSize_id2(const QSizeH pointSize, QPageSize::SizeMatchPolicy matchpolicy)
{
  return (QPageSize::PageSizeId) QPageSize::id(*(QSize*)pointSize, (QPageSize::SizeMatchPolicy) matchpolicy);
}

QPageSize::PageSizeId QPageSize_id3(const QSizeFH size, QPageSize::Unit units, QPageSize::SizeMatchPolicy matchpolicy)
{
  return (QPageSize::PageSizeId) QPageSize::id(*(QSizeF*)size, units, (QPageSize::SizeMatchPolicy) matchpolicy);
}

void QPageSize_key2(QPageSize::PageSizeId pageSizeId, PWideString retval)
{
	QString t_retval;
	t_retval = QPageSize::key((QPageSize::PageSizeId)pageSizeId);
  copyQStringToPWideString(t_retval, retval);
}

void QPageSize_name2(QPageSize::PageSizeId pageSizeId, PWideString retval)
{
	QString t_retval;
	t_retval = QPageSize::name((QPageSize::PageSizeId)pageSizeId);
  copyQStringToPWideString(t_retval, retval);
}

void QPageSize_size2(QPageSize::PageSizeId pageSizeId, QSizeFH retval, QPageSize::Unit units)
{
  *(QSizeF *)retval = QPageSize::size((QPageSize::PageSizeId)pageSizeId, (QPageSize::Unit)units);
}

void QPageSize_sizePixels2(QPageSize::PageSizeId pageSizeId, QSizeH retval, int resolution)
{
  *(QSize *)retval = QPageSize::sizePixels((QPageSize::PageSizeId)pageSizeId, resolution);
}

void QPageSize_sizePoints2(QPageSize::PageSizeId pageSizeId, QSizeH retval)
{
  *(QSize *)retval = QPageSize::sizePoints((QPageSize::PageSizeId)pageSizeId);
}

int QPageSize_windowsId2(QPageSize::PageSizeId pageSizeId)
{
  return (int) QPageSize::windowsId((QPageSize::PageSizeId)pageSizeId);
}

