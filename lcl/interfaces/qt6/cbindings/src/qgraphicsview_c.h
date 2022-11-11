//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QGRAPHICSVIEW_C_H
#define QGRAPHICSVIEW_C_H

#include <QtWidgets>
#include "pascalbind.h"

C_EXPORT QGraphicsViewH QGraphicsView_Create(QWidgetH parent);
C_EXPORT void QGraphicsView_Destroy(QGraphicsViewH handle);
C_EXPORT QGraphicsViewH QGraphicsView_Create2(QGraphicsSceneH scene, QWidgetH parent);
C_EXPORT void QGraphicsView_sizeHint(QGraphicsViewH handle, PSize retval);
C_EXPORT unsigned int QGraphicsView_renderHints(QGraphicsViewH handle);
C_EXPORT void QGraphicsView_setRenderHint(QGraphicsViewH handle, QPainter::RenderHint hint, bool enabled);
C_EXPORT void QGraphicsView_setRenderHints(QGraphicsViewH handle, unsigned int hints);
C_EXPORT unsigned int QGraphicsView_alignment(QGraphicsViewH handle);
C_EXPORT void QGraphicsView_setAlignment(QGraphicsViewH handle, unsigned int alignment);
C_EXPORT QGraphicsView::ViewportAnchor QGraphicsView_transformationAnchor(QGraphicsViewH handle);
C_EXPORT void QGraphicsView_setTransformationAnchor(QGraphicsViewH handle, QGraphicsView::ViewportAnchor anchor);
C_EXPORT QGraphicsView::ViewportAnchor QGraphicsView_resizeAnchor(QGraphicsViewH handle);
C_EXPORT void QGraphicsView_setResizeAnchor(QGraphicsViewH handle, QGraphicsView::ViewportAnchor anchor);
C_EXPORT QGraphicsView::ViewportUpdateMode QGraphicsView_viewportUpdateMode(QGraphicsViewH handle);
C_EXPORT void QGraphicsView_setViewportUpdateMode(QGraphicsViewH handle, QGraphicsView::ViewportUpdateMode mode);
C_EXPORT unsigned int QGraphicsView_optimizationFlags(QGraphicsViewH handle);
C_EXPORT void QGraphicsView_setOptimizationFlag(QGraphicsViewH handle, QGraphicsView::OptimizationFlag flag, bool enabled);
C_EXPORT void QGraphicsView_setOptimizationFlags(QGraphicsViewH handle, unsigned int flags);
C_EXPORT QGraphicsView::DragMode QGraphicsView_dragMode(QGraphicsViewH handle);
C_EXPORT void QGraphicsView_setDragMode(QGraphicsViewH handle, QGraphicsView::DragMode mode);
C_EXPORT Qt::ItemSelectionMode QGraphicsView_rubberBandSelectionMode(QGraphicsViewH handle);
C_EXPORT void QGraphicsView_setRubberBandSelectionMode(QGraphicsViewH handle, Qt::ItemSelectionMode mode);
C_EXPORT void QGraphicsView_rubberBandRect(QGraphicsViewH handle, PRect retval);
C_EXPORT unsigned int QGraphicsView_cacheMode(QGraphicsViewH handle);
C_EXPORT void QGraphicsView_setCacheMode(QGraphicsViewH handle, unsigned int mode);
C_EXPORT void QGraphicsView_resetCachedContent(QGraphicsViewH handle);
C_EXPORT bool QGraphicsView_isInteractive(QGraphicsViewH handle);
C_EXPORT void QGraphicsView_setInteractive(QGraphicsViewH handle, bool allowed);
C_EXPORT QGraphicsSceneH QGraphicsView_scene(QGraphicsViewH handle);
C_EXPORT void QGraphicsView_setScene(QGraphicsViewH handle, QGraphicsSceneH scene);
C_EXPORT void QGraphicsView_sceneRect(QGraphicsViewH handle, QRectFH retval);
C_EXPORT void QGraphicsView_setSceneRect(QGraphicsViewH handle, const QRectFH rect);
C_EXPORT void QGraphicsView_setSceneRect2(QGraphicsViewH handle, qreal x, qreal y, qreal w, qreal h);
C_EXPORT void QGraphicsView_transform(QGraphicsViewH handle, QTransformH retval);
C_EXPORT void QGraphicsView_viewportTransform(QGraphicsViewH handle, QTransformH retval);
C_EXPORT bool QGraphicsView_isTransformed(QGraphicsViewH handle);
C_EXPORT void QGraphicsView_setTransform(QGraphicsViewH handle, const QTransformH matrix, bool combine);
C_EXPORT void QGraphicsView_resetTransform(QGraphicsViewH handle);
C_EXPORT void QGraphicsView_rotate(QGraphicsViewH handle, qreal angle);
C_EXPORT void QGraphicsView_scale(QGraphicsViewH handle, qreal sx, qreal sy);
C_EXPORT void QGraphicsView_shear(QGraphicsViewH handle, qreal sh, qreal sv);
C_EXPORT void QGraphicsView_translate(QGraphicsViewH handle, qreal dx, qreal dy);
C_EXPORT void QGraphicsView_centerOn(QGraphicsViewH handle, const QPointFH pos);
C_EXPORT void QGraphicsView_centerOn2(QGraphicsViewH handle, qreal x, qreal y);
C_EXPORT void QGraphicsView_centerOn3(QGraphicsViewH handle, const QGraphicsItemH item);
C_EXPORT void QGraphicsView_ensureVisible(QGraphicsViewH handle, const QRectFH rect, int xmargin, int ymargin);
C_EXPORT void QGraphicsView_ensureVisible2(QGraphicsViewH handle, qreal x, qreal y, qreal w, qreal h, int xmargin, int ymargin);
C_EXPORT void QGraphicsView_ensureVisible3(QGraphicsViewH handle, const QGraphicsItemH item, int xmargin, int ymargin);
C_EXPORT void QGraphicsView_fitInView(QGraphicsViewH handle, const QRectFH rect, Qt::AspectRatioMode aspectRadioMode);
C_EXPORT void QGraphicsView_fitInView2(QGraphicsViewH handle, qreal x, qreal y, qreal w, qreal h, Qt::AspectRatioMode aspectRadioMode);
C_EXPORT void QGraphicsView_fitInView3(QGraphicsViewH handle, const QGraphicsItemH item, Qt::AspectRatioMode aspectRadioMode);
C_EXPORT void QGraphicsView_render(QGraphicsViewH handle, QPainterH painter, const QRectFH target, PRect source, Qt::AspectRatioMode aspectRatioMode);
C_EXPORT void QGraphicsView_items(QGraphicsViewH handle, PPtrIntArray retval);
C_EXPORT void QGraphicsView_items2(QGraphicsViewH handle, PPtrIntArray retval, const QPointH pos);
C_EXPORT void QGraphicsView_items3(QGraphicsViewH handle, PPtrIntArray retval, int x, int y);
C_EXPORT void QGraphicsView_items4(QGraphicsViewH handle, PPtrIntArray retval, PRect rect, Qt::ItemSelectionMode mode);
C_EXPORT void QGraphicsView_items5(QGraphicsViewH handle, PPtrIntArray retval, int x, int y, int w, int h, Qt::ItemSelectionMode mode);
C_EXPORT void QGraphicsView_items6(QGraphicsViewH handle, PPtrIntArray retval, const QPolygonH polygon, Qt::ItemSelectionMode mode);
C_EXPORT void QGraphicsView_items7(QGraphicsViewH handle, PPtrIntArray retval, const QPainterPathH path, Qt::ItemSelectionMode mode);
C_EXPORT QGraphicsItemH QGraphicsView_itemAt(QGraphicsViewH handle, const QPointH pos);
C_EXPORT QGraphicsItemH QGraphicsView_itemAt2(QGraphicsViewH handle, int x, int y);
C_EXPORT void QGraphicsView_mapToScene(QGraphicsViewH handle, PQtPointF retval, const QPointH point);
C_EXPORT void QGraphicsView_mapToScene2(QGraphicsViewH handle, QPolygonFH retval, PRect rect);
C_EXPORT void QGraphicsView_mapToScene3(QGraphicsViewH handle, QPolygonFH retval, const QPolygonH polygon);
C_EXPORT void QGraphicsView_mapToScene4(QGraphicsViewH handle, QPainterPathH retval, const QPainterPathH path);
C_EXPORT void QGraphicsView_mapFromScene(QGraphicsViewH handle, PQtPoint retval, const QPointFH point);
C_EXPORT void QGraphicsView_mapFromScene2(QGraphicsViewH handle, QPolygonH retval, const QRectFH rect);
C_EXPORT void QGraphicsView_mapFromScene3(QGraphicsViewH handle, QPolygonH retval, const QPolygonFH polygon);
C_EXPORT void QGraphicsView_mapFromScene4(QGraphicsViewH handle, QPainterPathH retval, const QPainterPathH path);
C_EXPORT void QGraphicsView_mapToScene5(QGraphicsViewH handle, PQtPointF retval, int x, int y);
C_EXPORT void QGraphicsView_mapToScene6(QGraphicsViewH handle, QPolygonFH retval, int x, int y, int w, int h);
C_EXPORT void QGraphicsView_mapFromScene5(QGraphicsViewH handle, PQtPoint retval, qreal x, qreal y);
C_EXPORT void QGraphicsView_mapFromScene6(QGraphicsViewH handle, QPolygonH retval, qreal x, qreal y, qreal w, qreal h);
C_EXPORT void QGraphicsView_inputMethodQuery(QGraphicsViewH handle, QVariantH retval, Qt::InputMethodQuery query);
C_EXPORT void QGraphicsView_backgroundBrush(QGraphicsViewH handle, QBrushH retval);
C_EXPORT void QGraphicsView_setBackgroundBrush(QGraphicsViewH handle, const QBrushH brush);
C_EXPORT void QGraphicsView_foregroundBrush(QGraphicsViewH handle, QBrushH retval);
C_EXPORT void QGraphicsView_setForegroundBrush(QGraphicsViewH handle, const QBrushH brush);
C_EXPORT void QGraphicsView_invalidateScene(QGraphicsViewH handle, const QRectFH rect, unsigned int layers);
C_EXPORT void QGraphicsView_updateSceneRect(QGraphicsViewH handle, const QRectFH rect);

#endif
