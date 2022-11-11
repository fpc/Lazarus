//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qpagedpaintdevice_c.h"

bool QPagedPaintDevice_newPage(QPagedPaintDeviceH handle)
{
	return (bool) ((QPagedPaintDevice *)handle)->newPage();
}

void QPagedPaintDevice_pageLayout(QPagedPaintDeviceH handle, QPageLayoutH retval)
{
	*(QPageLayout *)retval = ((QPagedPaintDevice *)handle)->pageLayout();
}

void QPagedPaintDevice_pageRanges(QPagedPaintDeviceH handle, QPageRangesH retval)
{
	*(QPageRanges*)retval = ((QPagedPaintDevice *)handle)->pageRanges();
}

bool QPagedPaintDevice_setPageLayout(QPagedPaintDeviceH handle, const QPageLayoutH layout)
{
  return (bool) ((QPagedPaintDevice *)handle)->setPageLayout(*(const QPageLayout*) layout);
}

bool QPagedPaintDevice_setPageMargins(QPagedPaintDeviceH handle, const QMarginsFH margins, QPageLayout::Unit units)
{
  return (bool) ((QPagedPaintDevice *)handle)->setPageMargins(*(const QMarginsF*) margins, (QPageLayout::Unit)units);
}

bool QPagedPaintDevice_setPageOrientation(QPagedPaintDeviceH handle, QPageLayout::Orientation orientation)
{
  return (bool) ((QPagedPaintDevice *)handle)->setPageOrientation(orientation);
}

void QPagedPaintDevice_setPageRanges(QPagedPaintDeviceH handle, const QPageRangesH pageRanges)
{
  ((QPagedPaintDevice *)handle)->setPageRanges(*(const QPageRanges*) pageRanges);
}

bool QPagedPaintDevice_setPageSize(QPagedPaintDeviceH handle, const QPageSizeH size)
{
	return (bool) ((QPagedPaintDevice *)handle)->setPageSize(*(const QPageSize *)size);
}


QPageRangesH QPageRanges_Create()
{
  return (QPageRangesH) new QPageRanges();
}
QPageRangesH QPageRanges_Create2(const QPageRangesH other)
{
  return (QPageRangesH) new QPageRanges(*(const QPageRanges*)other);
}

void QPageRanges_Destroy(QPageRangesH handle)
{
 	delete (QPageRanges *)handle;
}

void QPageRanges_addPage(QPageRangesH handle, int pageNumber)
{
  ((QPageRanges *)handle)->addPage(pageNumber);
}

void QPageRanges_addRange(QPageRangesH handle, int fromPage, int toPage)
{
  ((QPageRanges *)handle)->addRange(fromPage, toPage);
}

void QPageRanges_clear(QPageRangesH handle)
{
  ((QPageRanges *)handle)->clear();
}

bool QPageRanges_contains(QPageRangesH handle, int pageNumber)
{
  return (bool) ((QPageRanges *)handle)->contains(pageNumber);
}

int QPageRanges_firstPage(QPageRangesH handle)
{
  return (int) ((QPageRanges *)handle)->firstPage();
}

bool QPageRanges_isEmtpy(QPageRangesH handle)
{
  return (bool) ((QPageRanges *)handle)->isEmpty();
}

int QPageRanges_lastPage(QPageRangesH handle)
{
  return (int) ((QPageRanges *)handle)->lastPage();
}

void QPageRanges_toRangeList(QPageRangesH handle, PPtrIntArray retval)
{
	QList<QPageRanges::Range> t_retval;
	t_retval = ((QPageRanges *)handle)->toRangeList();
	copyQListTemplateToPtrIntArrayWithNew(t_retval, retval);
}

void QPageRanges_toString(QPageRangesH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QPageRanges *)handle)->toString();
  copyQStringToPWideString(t_retval, retval);
}

void QPageRanges_fromString(PWideString from, QPageRangesH retval)
{
	QString t_s;
	copyPWideStringToQString(from, t_s);
  *(QPageRanges *) retval = QPageRanges::fromString(t_s);
}





