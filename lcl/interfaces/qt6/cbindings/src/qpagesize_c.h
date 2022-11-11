//******************************************************************************
//  Copyright (c) 2022 by Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QPAGESIZE_C_H
#define QPAGESIZE_C_H

#include <QtGui>
#include "pascalbind.h"
C_EXPORT QPageSizeH QPageSize_Create(); // null pagesize
C_EXPORT QPageSizeH QPageSize_Create2(const QPageSizeH other);
C_EXPORT QPageSizeH QPageSize_Create3(QPageSize::PageSizeId pageSize);
C_EXPORT QPageSizeH QPageSize_Create4(const QSizeFH size, QPageSize::Unit units, const PWideString name, QPageSize::SizeMatchPolicy matchPolicy);
C_EXPORT QPageSizeH QPageSize_Create5(const QSizeH pointsize, const PWideString name, QPageSize::SizeMatchPolicy matchPolicy);
C_EXPORT void QPageSize_Destroy(QPageSizeH handle);

C_EXPORT void QPageSize_definitionSize(QPageSizeH handle, QSizeFH retval);
C_EXPORT QPageSize::Unit QPageSize_definitionUnits(QPageSizeH handle);
C_EXPORT QPageSize::PageSizeId QPageSize_id(QPageSizeH handle);
C_EXPORT bool QPageSize_isEquivalentTo(QPageSizeH handle, const QPageSizeH other);
C_EXPORT bool QPageSize_isValid(QPageSizeH handle);
C_EXPORT void QPageSize_key(QPageSizeH handle, PWideString retval);
C_EXPORT void QPageSize_name(QPageSizeH handle, PWideString retval);
C_EXPORT void QPageSize_rect(QPageSizeH handle, QRectFH retval, QPageSize::Unit units);
C_EXPORT void QPageSize_rect2(QPageSizeH handle, PRect retval, QPageSize::Unit units);
C_EXPORT void QPageSize_rectPixels(QPageSizeH handle, PRect retval, int resolution);
C_EXPORT void QPageSize_rectPoints(QPageSizeH handle, PRect retval);
C_EXPORT void QPageSize_size(QPageSizeH handle, QSizeFH retval, QPageSize::Unit units);
C_EXPORT void QPageSize_sizePixels(QPageSizeH handle, QSizeH retval, int resolution);
C_EXPORT void QPageSize_sizePoints(QPageSizeH handle, QSizeH retval);
C_EXPORT void QPageSize_swap(QPageSizeH handle, QPageSizeH other);
C_EXPORT int QPageSize_windowsId(QPageSizeH handle);

C_EXPORT void QPageSize_definitionSize2(QPageSize::PageSizeId pageSizeId, QSizeFH retval);
C_EXPORT QPageSize::Unit QPageSize_definitionUnits2(QPageSize::PageSizeId pageSizeId);
C_EXPORT QPageSize::PageSizeId QPageSize_id2(const QSizeH pointSize, QPageSize::SizeMatchPolicy matchpolicy);
C_EXPORT QPageSize::PageSizeId QPageSize_id3(const QSizeFH size, QPageSize::Unit units, QPageSize::SizeMatchPolicy matchpolicy);
C_EXPORT void QPageSize_key2(QPageSize::PageSizeId pageSizeId, PWideString retval);
C_EXPORT void QPageSize_name2(QPageSize::PageSizeId pageSizeId, PWideString retval);
C_EXPORT void QPageSize_size2(QPageSize::PageSizeId pageSizeId, QSizeFH retval, QPageSize::Unit units);
C_EXPORT void QPageSize_sizePixels2(QPageSize::PageSizeId pageSizeId, QSizeH retval, int resolution);
C_EXPORT void QPageSize_sizePoints2(QPageSize::PageSizeId pageSizeId, QSizeH retval);
C_EXPORT int QPageSize_windowsId2(QPageSize::PageSizeId pageSizeId);

#endif
