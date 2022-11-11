//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qprintpreviewwidget_c.h"

QPrintPreviewWidgetH QPrintPreviewWidget_Create(QPrinterH printer, QWidgetH parent, unsigned int flags)
{
	return (QPrintPreviewWidgetH) new QPrintPreviewWidget((QPrinter*)printer, (QWidget*)parent, (Qt::WindowFlags)flags);
}

void QPrintPreviewWidget_Destroy(QPrintPreviewWidgetH handle)
{
	delete (QPrintPreviewWidget *)handle;
}

QPrintPreviewWidgetH QPrintPreviewWidget_Create2(QWidgetH parent, unsigned int flags)
{
	return (QPrintPreviewWidgetH) new QPrintPreviewWidget((QWidget*)parent, (Qt::WindowFlags)flags);
}

qreal QPrintPreviewWidget_zoomFactor(QPrintPreviewWidgetH handle)
{
	return (qreal) ((QPrintPreviewWidget *)handle)->zoomFactor();
}

QPageLayout::Orientation QPrintPreviewWidget_orientation(QPrintPreviewWidgetH handle)
{
	return (QPageLayout::Orientation) ((QPrintPreviewWidget *)handle)->orientation();
}

QPrintPreviewWidget::ViewMode QPrintPreviewWidget_viewMode(QPrintPreviewWidgetH handle)
{
	return (QPrintPreviewWidget::ViewMode) ((QPrintPreviewWidget *)handle)->viewMode();
}

QPrintPreviewWidget::ZoomMode QPrintPreviewWidget_zoomMode(QPrintPreviewWidgetH handle)
{
	return (QPrintPreviewWidget::ZoomMode) ((QPrintPreviewWidget *)handle)->zoomMode();
}

int QPrintPreviewWidget_currentPage(QPrintPreviewWidgetH handle)
{
	return (int) ((QPrintPreviewWidget *)handle)->currentPage();
}

int QPrintPreviewWidget_pageCount(QPrintPreviewWidgetH handle)
{
	return (int) ((QPrintPreviewWidget *)handle)->pageCount();
}

void QPrintPreviewWidget_setVisible(QPrintPreviewWidgetH handle, bool visible)
{
	((QPrintPreviewWidget *)handle)->setVisible(visible);
}

void QPrintPreviewWidget_print(QPrintPreviewWidgetH handle)
{
	((QPrintPreviewWidget *)handle)->print();
}

void QPrintPreviewWidget_zoomIn(QPrintPreviewWidgetH handle, qreal zoom)
{
	((QPrintPreviewWidget *)handle)->zoomIn(zoom);
}

void QPrintPreviewWidget_zoomOut(QPrintPreviewWidgetH handle, qreal zoom)
{
	((QPrintPreviewWidget *)handle)->zoomOut(zoom);
}

void QPrintPreviewWidget_setZoomFactor(QPrintPreviewWidgetH handle, qreal zoomFactor)
{
	((QPrintPreviewWidget *)handle)->setZoomFactor(zoomFactor);
}

void QPrintPreviewWidget_setOrientation(QPrintPreviewWidgetH handle, QPageLayout::Orientation orientation)
{
	((QPrintPreviewWidget *)handle)->setOrientation(orientation);
}

void QPrintPreviewWidget_setViewMode(QPrintPreviewWidgetH handle, QPrintPreviewWidget::ViewMode viewMode)
{
	((QPrintPreviewWidget *)handle)->setViewMode(viewMode);
}

void QPrintPreviewWidget_setZoomMode(QPrintPreviewWidgetH handle, QPrintPreviewWidget::ZoomMode zoomMode)
{
	((QPrintPreviewWidget *)handle)->setZoomMode(zoomMode);
}

void QPrintPreviewWidget_setCurrentPage(QPrintPreviewWidgetH handle, int pageNumber)
{
	((QPrintPreviewWidget *)handle)->setCurrentPage(pageNumber);
}

void QPrintPreviewWidget_fitToWidth(QPrintPreviewWidgetH handle)
{
	((QPrintPreviewWidget *)handle)->fitToWidth();
}

void QPrintPreviewWidget_fitInView(QPrintPreviewWidgetH handle)
{
	((QPrintPreviewWidget *)handle)->fitInView();
}

void QPrintPreviewWidget_setLandscapeOrientation(QPrintPreviewWidgetH handle)
{
	((QPrintPreviewWidget *)handle)->setLandscapeOrientation();
}

void QPrintPreviewWidget_setPortraitOrientation(QPrintPreviewWidgetH handle)
{
	((QPrintPreviewWidget *)handle)->setPortraitOrientation();
}

void QPrintPreviewWidget_setSinglePageViewMode(QPrintPreviewWidgetH handle)
{
	((QPrintPreviewWidget *)handle)->setSinglePageViewMode();
}

void QPrintPreviewWidget_setFacingPagesViewMode(QPrintPreviewWidgetH handle)
{
	((QPrintPreviewWidget *)handle)->setFacingPagesViewMode();
}

void QPrintPreviewWidget_setAllPagesViewMode(QPrintPreviewWidgetH handle)
{
	((QPrintPreviewWidget *)handle)->setAllPagesViewMode();
}

void QPrintPreviewWidget_updatePreview(QPrintPreviewWidgetH handle)
{
	((QPrintPreviewWidget *)handle)->updatePreview();
}

