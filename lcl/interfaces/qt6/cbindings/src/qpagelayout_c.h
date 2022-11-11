//******************************************************************************
//  Copyright (c) 2022 by Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QPAGELAYOUT_C_H
#define QPAGELAYOUT_C_H

#include <QtGui>
#include "pascalbind.h"

C_EXPORT QPageLayoutH QPageLayout_Create();
C_EXPORT QPageLayoutH QPageLayout_Create2(const QPageLayoutH other);
C_EXPORT QPageLayoutH QPageLayout_Create3(const QPageSizeH pageSize, QPageLayout::Orientation orientation, const QMarginsFH margins, QPageLayout::Unit units, const QMarginsFH minMargins);
C_EXPORT void QPageLayout_Destroy(QPageLayoutH handle);
C_EXPORT void QPageLayout_fullRect(QPageLayoutH handle, QRectFH retval);
C_EXPORT void QPageLayout_fullRect2(QPageLayoutH handle, QPageLayout::Unit units, QRectFH retval);
C_EXPORT void QPageLayout_fullRectPixels(QPageLayoutH handle, int resolution, PRect retval);
C_EXPORT void QPageLayout_fullRectPoints(QPageLayoutH handle, PRect retval);
C_EXPORT bool QPageLayout_isEquivalentTo(QPageLayoutH handle, const QPageLayoutH other);
C_EXPORT bool QPageLayout_isValid(QPageLayoutH handle);
C_EXPORT void QPageLayout_margins(QPageLayoutH handle, QMarginsFH retval);
C_EXPORT void QPageLayout_margins2(QPageLayoutH handle, QPageLayout::Unit units, QMarginsFH retval);
C_EXPORT void QPageLayout_marginsPixels(QPageLayoutH handle, int resolution, QMarginsH retval);
C_EXPORT void QPageLayout_marginsPoints(QPageLayoutH handle, QMarginsH retval);
C_EXPORT void QPageLayout_maximumMargins(QPageLayoutH handle, QMarginsFH retval);
C_EXPORT void QPageLayout_minimumMargins(QPageLayoutH handle, QMarginsFH retval);
C_EXPORT QPageLayout::Mode QPageLayout_mode(QPageLayoutH handle);
C_EXPORT QPageLayout::Orientation QPageLayout_orientation(QPageLayoutH handle);
C_EXPORT void QPageLayout_pageSize(QPageLayoutH handle, QPageSizeH retval);
C_EXPORT void QPageLayout_paintRect(QPageLayoutH handle, QRectFH retval);
C_EXPORT void QPageLayout_paintRect2(QPageLayoutH handle, QPageLayout::Unit units, QRectFH retval);
C_EXPORT void QPageLayout_paintRect3(QPageLayoutH handle, PRect retval);
C_EXPORT void QPageLayout_paintRectPixels(QPageLayoutH handle, int resolution, PRect retval);
C_EXPORT void QPageLayout_paintRectPoints(QPageLayoutH handle, PRect retval);
C_EXPORT bool QPageLayout_setBottomMargin(QPageLayoutH handle, qreal bottomMargin);
C_EXPORT bool QPageLayout_setLeftMargin(QPageLayoutH handle, qreal leftMargin);
C_EXPORT void QPageLayout_setMargins(QPageLayoutH handle, const QMarginsFH margins);
C_EXPORT void QPageLayout_setMinimumMargins(QPageLayoutH handle, const QMarginsFH margins);
C_EXPORT void QPageLayout_setMode(QPageLayoutH handle, QPageLayout::Mode mode);
C_EXPORT void QPageLayout_setOrientation(QPageLayoutH handle, QPageLayout::Orientation orientation);
C_EXPORT void QPageLayout_setPageSize(QPageLayoutH handle, const QPageSizeH pageSize, const QMarginsFH minMargins);
C_EXPORT bool QPageLayout_setRightMargin(QPageLayoutH handle, qreal rightMargin);
C_EXPORT bool QPageLayout_setTopMargin(QPageLayoutH handle, qreal topMargin);
C_EXPORT void QPageLayout_setUnits(QPageLayoutH handle, QPageLayout::Unit units);
C_EXPORT void QPageLayout_swap(QPageLayoutH handle, const QPageLayoutH other);

#endif

