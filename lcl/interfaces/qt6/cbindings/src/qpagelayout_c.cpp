//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qpagelayout_c.h"

QPageLayoutH QPageLayout_Create()
{
  return (QPageLayoutH) new QPageLayout();
}

QPageLayoutH QPageLayout_Create2(const QPageLayoutH other)
{
  return (QPageLayoutH) new QPageLayout(*(const QPageLayout *)other);
}

QPageLayoutH QPageLayout_Create3(const QPageSizeH pageSize, QPageLayout::Orientation orientation, const QMarginsFH margins, QPageLayout::Unit units, const QMarginsFH minMargins)
{
  return (QPageLayoutH) new QPageLayout(*(const QPageSize *)pageSize, (QPageLayout::Orientation)orientation,*(const QMarginsF *)margins,(QPageLayout::Unit)units,*(const QMarginsF *)minMargins);
}

void QPageLayout_Destroy(QPageLayoutH handle)
{
  delete (QPageLayout *)handle;
}

void QPageLayout_fullRect(QPageLayoutH handle, QRectFH retval)
{
  *(QRectF *)retval = ((QPageLayout *)handle)->fullRect();
}

void QPageLayout_fullRect2(QPageLayoutH handle, QPageLayout::Unit units, QRectFH retval)
{
  *(QRectF *)retval = ((QPageLayout *)handle)->fullRect((QPageLayout::Unit)units);
}

void QPageLayout_fullRectPixels(QPageLayoutH handle, int resolution, PRect retval)
{
	QRect t_retval;
	t_retval = ((QPageLayout *)handle)->fullRectPixels(resolution);
	copyQRectToPRect(t_retval, retval);
}

void QPageLayout_fullRectPoints(QPageLayoutH handle, PRect retval)
{
	QRect t_retval;
	t_retval = ((QPageLayout *)handle)->fullRectPoints();
	copyQRectToPRect(t_retval, retval);
}

bool QPageLayout_isEquivalentTo(QPageLayoutH handle, const QPageLayoutH other)
{
  return (bool) ((QPageLayout *)handle)->isEquivalentTo(*(const QPageLayout*)other);
}

bool QPageLayout_isValid(QPageLayoutH handle)
{
  return (bool) ((QPageLayout *)handle)->isValid();
}

void QPageLayout_margins(QPageLayoutH handle, QMarginsFH retval)
{
  *(QMarginsF *)retval = ((QPageLayout *)handle)->margins();
}

void QPageLayout_margins2(QPageLayoutH handle, QPageLayout::Unit units, QMarginsFH retval)
{
  *(QMarginsF *)retval = ((QPageLayout *)handle)->margins((QPageLayout::Unit)units);
}

void QPageLayout_marginsPixels(QPageLayoutH handle, int resolution, QMarginsH retval)
{
  *(QMargins *)retval = ((QPageLayout *)handle)->marginsPixels(resolution);
}

void QPageLayout_marginsPoints(QPageLayoutH handle, QMarginsH retval)
{
  *(QMargins *)retval = ((QPageLayout *)handle)->marginsPoints();
}

void QPageLayout_maximumMargins(QPageLayoutH handle, QMarginsFH retval)
{
  *(QMarginsF *)retval = ((QPageLayout *)handle)->maximumMargins();
}

void QPageLayout_minimumMargins(QPageLayoutH handle, QMarginsFH retval)
{
  *(QMarginsF *)retval = ((QPageLayout *)handle)->minimumMargins();
}

QPageLayout::Mode QPageLayout_mode(QPageLayoutH handle)
{
  return (QPageLayout::Mode) ((QPageLayout *)handle)->mode();
}

QPageLayout::Orientation QPageLayout_orientation(QPageLayoutH handle)
{
  return (QPageLayout::Orientation) ((QPageLayout *)handle)->orientation();
}

void QPageLayout_pageSize(QPageLayoutH handle, QPageSizeH retval)
{
  *(QPageSize *)retval = ((QPageLayout *)handle)->pageSize();
}

void QPageLayout_paintRect(QPageLayoutH handle, QRectFH retval)
{
  *(QRectF *)retval = ((QPageLayout *)handle)->paintRect();
}

void QPageLayout_paintRect2(QPageLayoutH handle, QPageLayout::Unit units, QRectFH retval)
{
  *(QRectF *)retval = ((QPageLayout *)handle)->paintRect((QPageLayout::Unit)units);
}

void QPageLayout_paintRect3(QPageLayoutH handle, PRect retval)
{
	QRect t_retval;
	t_retval = ((QPageLayout *)handle)->paintRect().toRect();
	copyQRectToPRect(t_retval, retval);
}

void QPageLayout_paintRectPixels(QPageLayoutH handle, int resolution, PRect retval)
{
	QRect t_retval;
	t_retval = ((QPageLayout *)handle)->paintRectPixels(resolution);
	copyQRectToPRect(t_retval, retval);
}

void QPageLayout_paintRectPoints(QPageLayoutH handle, PRect retval)
{
	QRect t_retval;
	t_retval = ((QPageLayout *)handle)->paintRectPoints();
	copyQRectToPRect(t_retval, retval);
}

bool QPageLayout_setBottomMargin(QPageLayoutH handle, qreal bottomMargin)
{
  return (bool) ((QPageLayout *)handle)->setBottomMargin(bottomMargin);
}

bool QPageLayout_setLeftMargin(QPageLayoutH handle, qreal leftMargin)
{
  return (bool) ((QPageLayout *)handle)->setLeftMargin(leftMargin);
}

void QPageLayout_setMargins(QPageLayoutH handle, const QMarginsFH margins)
{
  ((QPageLayout *)handle)->setMargins(*(const QMarginsF *)margins);
}

void QPageLayout_setMinimumMargins(QPageLayoutH handle, const QMarginsFH margins)
{
  ((QPageLayout *)handle)->setMinimumMargins(*(const QMarginsF *)margins);
}

void QPageLayout_setMode(QPageLayoutH handle, QPageLayout::Mode mode)
{
  ((QPageLayout *)handle)->setMode((QPageLayout::Mode)mode);
}

void QPageLayout_setOrientation(QPageLayoutH handle, QPageLayout::Orientation orientation)
{
  ((QPageLayout *)handle)->setOrientation((QPageLayout::Orientation)orientation);
}

void QPageLayout_setPageSize(QPageLayoutH handle, const QPageSizeH pageSize, const QMarginsFH minMargins)
{
  ((QPageLayout *)handle)->setPageSize(*(const QPageSize *)pageSize, *(const QMarginsF*)minMargins);
}

bool QPageLayout_setRightMargin(QPageLayoutH handle, qreal rightMargin)
{
  return (bool) ((QPageLayout *)handle)->setRightMargin(rightMargin);
}

bool QPageLayout_setTopMargin(QPageLayoutH handle, qreal topMargin)
{
  return (bool) ((QPageLayout *)handle)->setTopMargin(topMargin);
}

void QPageLayout_setUnits(QPageLayoutH handle, QPageLayout::Unit units)
{
  ((QPageLayout *)handle)->setUnits((QPageLayout::Unit)units);
}

void QPageLayout_swap(QPageLayoutH handle, const QPageLayoutH other)
{
  ((QPageLayout *)handle)->swap(*(QPageLayout *)other);
}


