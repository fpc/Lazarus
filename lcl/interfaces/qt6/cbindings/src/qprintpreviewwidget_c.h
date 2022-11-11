//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QPRINTPREVIEWWIDGET_C_H
#define QPRINTPREVIEWWIDGET_C_H

#include <QtPrintSupport>
#include "pascalbind.h"

C_EXPORT QPrintPreviewWidgetH QPrintPreviewWidget_Create(QPrinterH printer, QWidgetH parent, unsigned int flags);
C_EXPORT void QPrintPreviewWidget_Destroy(QPrintPreviewWidgetH handle);
C_EXPORT QPrintPreviewWidgetH QPrintPreviewWidget_Create2(QWidgetH parent, unsigned int flags);
C_EXPORT qreal QPrintPreviewWidget_zoomFactor(QPrintPreviewWidgetH handle);
C_EXPORT QPageLayout::Orientation QPrintPreviewWidget_orientation(QPrintPreviewWidgetH handle);
C_EXPORT QPrintPreviewWidget::ViewMode QPrintPreviewWidget_viewMode(QPrintPreviewWidgetH handle);
C_EXPORT QPrintPreviewWidget::ZoomMode QPrintPreviewWidget_zoomMode(QPrintPreviewWidgetH handle);
C_EXPORT int QPrintPreviewWidget_currentPage(QPrintPreviewWidgetH handle);
C_EXPORT int QPrintPreviewWidget_pageCount(QPrintPreviewWidgetH handle);
C_EXPORT void QPrintPreviewWidget_setVisible(QPrintPreviewWidgetH handle, bool visible);
C_EXPORT void QPrintPreviewWidget_print(QPrintPreviewWidgetH handle);
C_EXPORT void QPrintPreviewWidget_zoomIn(QPrintPreviewWidgetH handle, qreal zoom);
C_EXPORT void QPrintPreviewWidget_zoomOut(QPrintPreviewWidgetH handle, qreal zoom);
C_EXPORT void QPrintPreviewWidget_setZoomFactor(QPrintPreviewWidgetH handle, qreal zoomFactor);
C_EXPORT void QPrintPreviewWidget_setOrientation(QPrintPreviewWidgetH handle, QPageLayout::Orientation orientation);
C_EXPORT void QPrintPreviewWidget_setViewMode(QPrintPreviewWidgetH handle, QPrintPreviewWidget::ViewMode viewMode);
C_EXPORT void QPrintPreviewWidget_setZoomMode(QPrintPreviewWidgetH handle, QPrintPreviewWidget::ZoomMode zoomMode);
C_EXPORT void QPrintPreviewWidget_setCurrentPage(QPrintPreviewWidgetH handle, int pageNumber);
C_EXPORT void QPrintPreviewWidget_fitToWidth(QPrintPreviewWidgetH handle);
C_EXPORT void QPrintPreviewWidget_fitInView(QPrintPreviewWidgetH handle);
C_EXPORT void QPrintPreviewWidget_setLandscapeOrientation(QPrintPreviewWidgetH handle);
C_EXPORT void QPrintPreviewWidget_setPortraitOrientation(QPrintPreviewWidgetH handle);
C_EXPORT void QPrintPreviewWidget_setSinglePageViewMode(QPrintPreviewWidgetH handle);
C_EXPORT void QPrintPreviewWidget_setFacingPagesViewMode(QPrintPreviewWidgetH handle);
C_EXPORT void QPrintPreviewWidget_setAllPagesViewMode(QPrintPreviewWidgetH handle);
C_EXPORT void QPrintPreviewWidget_updatePreview(QPrintPreviewWidgetH handle);

#endif
