//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************

/* TODO setPageRanges(), pageRanges(), setPageMargins() */
#ifndef QPAGEDPAINTDEVICE_C_H
#define QPAGEDPAINTDEVICE_C_H

#include <QtGui>
#include "pascalbind.h"

C_EXPORT bool QPagedPaintDevice_newPage(QPagedPaintDeviceH handle);
C_EXPORT void QPagedPaintDevice_pageLayout(QPagedPaintDeviceH handle, QPageLayoutH retval);
C_EXPORT void QPagedPaintDevice_pageRanges(QPagedPaintDeviceH handle, QPageRangesH retval);
C_EXPORT bool QPagedPaintDevice_setPageLayout(QPagedPaintDeviceH handle, const QPageLayoutH layout);
C_EXPORT bool QPagedPaintDevice_setPageMargins(QPagedPaintDeviceH handle, const QMarginsFH margins, QPageLayout::Unit units = QPageLayout::Millimeter);
C_EXPORT bool QPagedPaintDevice_setPageOrientation(QPagedPaintDeviceH handle, QPageLayout::Orientation orientation);
C_EXPORT void QPagedPaintDevice_setPageRanges(QPagedPaintDeviceH handle, const QPageRangesH pageRanges);
C_EXPORT bool QPagedPaintDevice_setPageSize(QPagedPaintDeviceH handle, const QPageSizeH size);

C_EXPORT QPageRangesH QPageRanges_Create();
C_EXPORT QPageRangesH QPageRanges_Create2(const QPageRangesH other);
C_EXPORT void QPageRanges_Destroy(QPageRangesH handle);
C_EXPORT void QPageRanges_addPage(QPageRangesH handle, int pageNumber);
C_EXPORT void QPageRanges_addRange(QPageRangesH handle, int fromPage, int toPage);
C_EXPORT void QPageRanges_clear(QPageRangesH handle);
C_EXPORT bool QPageRanges_contains(QPageRangesH handle, int pageNumber);
C_EXPORT int QPageRanges_firstPage(QPageRangesH handle);
C_EXPORT bool QPageRanges_isEmtpy(QPageRangesH handle);
C_EXPORT int QPageRanges_lastPage(QPageRangesH handle);
C_EXPORT void QPageRanges_toRangeList(QPageRangesH handle, PPtrIntArray retval);
C_EXPORT void QPageRanges_toString(QPageRangesH handle, PWideString retval);
C_EXPORT void QPageRanges_fromString(PWideString from, QPageRangesH retval);

#endif
