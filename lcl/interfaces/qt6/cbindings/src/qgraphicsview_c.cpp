//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qgraphicsview_c.h"

QGraphicsViewH QGraphicsView_Create(QWidgetH parent)
{
	return (QGraphicsViewH) new QGraphicsView((QWidget*)parent);
}

void QGraphicsView_Destroy(QGraphicsViewH handle)
{
	delete (QGraphicsView *)handle;
}

QGraphicsViewH QGraphicsView_Create2(QGraphicsSceneH scene, QWidgetH parent)
{
	return (QGraphicsViewH) new QGraphicsView((QGraphicsScene*)scene, (QWidget*)parent);
}

void QGraphicsView_sizeHint(QGraphicsViewH handle, PSize retval)
{
	*(QSize *)retval = ((QGraphicsView *)handle)->sizeHint();
}

unsigned int QGraphicsView_renderHints(QGraphicsViewH handle)
{
	return (unsigned int) ((QGraphicsView *)handle)->renderHints();
}

void QGraphicsView_setRenderHint(QGraphicsViewH handle, QPainter::RenderHint hint, bool enabled)
{
	((QGraphicsView *)handle)->setRenderHint(hint, enabled);
}

void QGraphicsView_setRenderHints(QGraphicsViewH handle, unsigned int hints)
{
	((QGraphicsView *)handle)->setRenderHints((QPainter::RenderHints)hints);
}

unsigned int QGraphicsView_alignment(QGraphicsViewH handle)
{
	return (unsigned int) ((QGraphicsView *)handle)->alignment();
}

void QGraphicsView_setAlignment(QGraphicsViewH handle, unsigned int alignment)
{
	((QGraphicsView *)handle)->setAlignment((Qt::Alignment)alignment);
}

QGraphicsView::ViewportAnchor QGraphicsView_transformationAnchor(QGraphicsViewH handle)
{
	return (QGraphicsView::ViewportAnchor) ((QGraphicsView *)handle)->transformationAnchor();
}

void QGraphicsView_setTransformationAnchor(QGraphicsViewH handle, QGraphicsView::ViewportAnchor anchor)
{
	((QGraphicsView *)handle)->setTransformationAnchor(anchor);
}

QGraphicsView::ViewportAnchor QGraphicsView_resizeAnchor(QGraphicsViewH handle)
{
	return (QGraphicsView::ViewportAnchor) ((QGraphicsView *)handle)->resizeAnchor();
}

void QGraphicsView_setResizeAnchor(QGraphicsViewH handle, QGraphicsView::ViewportAnchor anchor)
{
	((QGraphicsView *)handle)->setResizeAnchor(anchor);
}

QGraphicsView::ViewportUpdateMode QGraphicsView_viewportUpdateMode(QGraphicsViewH handle)
{
	return (QGraphicsView::ViewportUpdateMode) ((QGraphicsView *)handle)->viewportUpdateMode();
}

void QGraphicsView_setViewportUpdateMode(QGraphicsViewH handle, QGraphicsView::ViewportUpdateMode mode)
{
	((QGraphicsView *)handle)->setViewportUpdateMode(mode);
}

unsigned int QGraphicsView_optimizationFlags(QGraphicsViewH handle)
{
	return (unsigned int) ((QGraphicsView *)handle)->optimizationFlags();
}

void QGraphicsView_setOptimizationFlag(QGraphicsViewH handle, QGraphicsView::OptimizationFlag flag, bool enabled)
{
	((QGraphicsView *)handle)->setOptimizationFlag(flag, enabled);
}

void QGraphicsView_setOptimizationFlags(QGraphicsViewH handle, unsigned int flags)
{
	((QGraphicsView *)handle)->setOptimizationFlags((QGraphicsView::OptimizationFlags)flags);
}

QGraphicsView::DragMode QGraphicsView_dragMode(QGraphicsViewH handle)
{
	return (QGraphicsView::DragMode) ((QGraphicsView *)handle)->dragMode();
}

void QGraphicsView_setDragMode(QGraphicsViewH handle, QGraphicsView::DragMode mode)
{
	((QGraphicsView *)handle)->setDragMode(mode);
}

Qt::ItemSelectionMode QGraphicsView_rubberBandSelectionMode(QGraphicsViewH handle)
{
	return (Qt::ItemSelectionMode) ((QGraphicsView *)handle)->rubberBandSelectionMode();
}

void QGraphicsView_setRubberBandSelectionMode(QGraphicsViewH handle, Qt::ItemSelectionMode mode)
{
	((QGraphicsView *)handle)->setRubberBandSelectionMode(mode);
}

void QGraphicsView_rubberBandRect(QGraphicsViewH handle, PRect retval)
{
	QRect t_retval;
	t_retval = ((QGraphicsView *)handle)->rubberBandRect();
	copyQRectToPRect(t_retval, retval);
}

unsigned int QGraphicsView_cacheMode(QGraphicsViewH handle)
{
	return (unsigned int) ((QGraphicsView *)handle)->cacheMode();
}

void QGraphicsView_setCacheMode(QGraphicsViewH handle, unsigned int mode)
{
	((QGraphicsView *)handle)->setCacheMode((QGraphicsView::CacheMode)mode);
}

void QGraphicsView_resetCachedContent(QGraphicsViewH handle)
{
	((QGraphicsView *)handle)->resetCachedContent();
}

bool QGraphicsView_isInteractive(QGraphicsViewH handle)
{
	return (bool) ((QGraphicsView *)handle)->isInteractive();
}

void QGraphicsView_setInteractive(QGraphicsViewH handle, bool allowed)
{
	((QGraphicsView *)handle)->setInteractive(allowed);
}

QGraphicsSceneH QGraphicsView_scene(QGraphicsViewH handle)
{
	return (QGraphicsSceneH) ((QGraphicsView *)handle)->scene();
}

void QGraphicsView_setScene(QGraphicsViewH handle, QGraphicsSceneH scene)
{
	((QGraphicsView *)handle)->setScene((QGraphicsScene*)scene);
}

void QGraphicsView_sceneRect(QGraphicsViewH handle, QRectFH retval)
{
	*(QRectF *)retval = ((QGraphicsView *)handle)->sceneRect();
}

void QGraphicsView_setSceneRect(QGraphicsViewH handle, const QRectFH rect)
{
	((QGraphicsView *)handle)->setSceneRect(*(const QRectF*)rect);
}

void QGraphicsView_setSceneRect2(QGraphicsViewH handle, qreal x, qreal y, qreal w, qreal h)
{
	((QGraphicsView *)handle)->setSceneRect(x, y, w, h);
}

void QGraphicsView_transform(QGraphicsViewH handle, QTransformH retval)
{
	*(QTransform *)retval = ((QGraphicsView *)handle)->transform();
}

void QGraphicsView_viewportTransform(QGraphicsViewH handle, QTransformH retval)
{
	*(QTransform *)retval = ((QGraphicsView *)handle)->viewportTransform();
}

bool QGraphicsView_isTransformed(QGraphicsViewH handle)
{
	return (bool) ((QGraphicsView *)handle)->isTransformed();
}

void QGraphicsView_setTransform(QGraphicsViewH handle, const QTransformH matrix, bool combine)
{
	((QGraphicsView *)handle)->setTransform(*(const QTransform*)matrix, combine);
}

void QGraphicsView_resetTransform(QGraphicsViewH handle)
{
	((QGraphicsView *)handle)->resetTransform();
}

void QGraphicsView_rotate(QGraphicsViewH handle, qreal angle)
{
	((QGraphicsView *)handle)->rotate(angle);
}

void QGraphicsView_scale(QGraphicsViewH handle, qreal sx, qreal sy)
{
	((QGraphicsView *)handle)->scale(sx, sy);
}

void QGraphicsView_shear(QGraphicsViewH handle, qreal sh, qreal sv)
{
	((QGraphicsView *)handle)->shear(sh, sv);
}

void QGraphicsView_translate(QGraphicsViewH handle, qreal dx, qreal dy)
{
	((QGraphicsView *)handle)->translate(dx, dy);
}

void QGraphicsView_centerOn(QGraphicsViewH handle, const QPointFH pos)
{
	((QGraphicsView *)handle)->centerOn(*(const QPointF*)pos);
}

void QGraphicsView_centerOn2(QGraphicsViewH handle, qreal x, qreal y)
{
	((QGraphicsView *)handle)->centerOn(x, y);
}

void QGraphicsView_centerOn3(QGraphicsViewH handle, const QGraphicsItemH item)
{
	((QGraphicsView *)handle)->centerOn((const QGraphicsItem*)item);
}

void QGraphicsView_ensureVisible(QGraphicsViewH handle, const QRectFH rect, int xmargin, int ymargin)
{
	((QGraphicsView *)handle)->ensureVisible(*(const QRectF*)rect, xmargin, ymargin);
}

void QGraphicsView_ensureVisible2(QGraphicsViewH handle, qreal x, qreal y, qreal w, qreal h, int xmargin, int ymargin)
{
	((QGraphicsView *)handle)->ensureVisible(x, y, w, h, xmargin, ymargin);
}

void QGraphicsView_ensureVisible3(QGraphicsViewH handle, const QGraphicsItemH item, int xmargin, int ymargin)
{
	((QGraphicsView *)handle)->ensureVisible((const QGraphicsItem*)item, xmargin, ymargin);
}

void QGraphicsView_fitInView(QGraphicsViewH handle, const QRectFH rect, Qt::AspectRatioMode aspectRadioMode)
{
	((QGraphicsView *)handle)->fitInView(*(const QRectF*)rect, aspectRadioMode);
}

void QGraphicsView_fitInView2(QGraphicsViewH handle, qreal x, qreal y, qreal w, qreal h, Qt::AspectRatioMode aspectRadioMode)
{
	((QGraphicsView *)handle)->fitInView(x, y, w, h, aspectRadioMode);
}

void QGraphicsView_fitInView3(QGraphicsViewH handle, const QGraphicsItemH item, Qt::AspectRatioMode aspectRadioMode)
{
	((QGraphicsView *)handle)->fitInView((const QGraphicsItem*)item, aspectRadioMode);
}

void QGraphicsView_render(QGraphicsViewH handle, QPainterH painter, const QRectFH target, PRect source, Qt::AspectRatioMode aspectRatioMode)
{
	QRect t_source;
	copyPRectToQRect(source, t_source);
	((QGraphicsView *)handle)->render((QPainter*)painter, *(const QRectF*)target, t_source, aspectRatioMode);
}

void QGraphicsView_items(QGraphicsViewH handle, PPtrIntArray retval)
{
	QList<QGraphicsItem*> t_retval;
	t_retval = ((QGraphicsView *)handle)->items();
	copyQListTemplateToPtrIntArray(t_retval, retval);
}

void QGraphicsView_items2(QGraphicsViewH handle, PPtrIntArray retval, const QPointH pos)
{
	QList<QGraphicsItem*> t_retval;
	t_retval = ((QGraphicsView *)handle)->items(*(const QPoint*)pos);
	copyQListTemplateToPtrIntArray(t_retval, retval);
}

void QGraphicsView_items3(QGraphicsViewH handle, PPtrIntArray retval, int x, int y)
{
	QList<QGraphicsItem*> t_retval;
	t_retval = ((QGraphicsView *)handle)->items(x, y);
	copyQListTemplateToPtrIntArray(t_retval, retval);
}

void QGraphicsView_items4(QGraphicsViewH handle, PPtrIntArray retval, PRect rect, Qt::ItemSelectionMode mode)
{
	QList<QGraphicsItem*> t_retval;
	QRect t_rect;
	copyPRectToQRect(rect, t_rect);
	t_retval = ((QGraphicsView *)handle)->items(t_rect, mode);
	copyQListTemplateToPtrIntArray(t_retval, retval);
}

void QGraphicsView_items5(QGraphicsViewH handle, PPtrIntArray retval, int x, int y, int w, int h, Qt::ItemSelectionMode mode)
{
	QList<QGraphicsItem*> t_retval;
	t_retval = ((QGraphicsView *)handle)->items(x, y, w, h, mode);
	copyQListTemplateToPtrIntArray(t_retval, retval);
}

void QGraphicsView_items6(QGraphicsViewH handle, PPtrIntArray retval, const QPolygonH polygon, Qt::ItemSelectionMode mode)
{
	QList<QGraphicsItem*> t_retval;
	t_retval = ((QGraphicsView *)handle)->items(*(const QPolygon*)polygon, mode);
	copyQListTemplateToPtrIntArray(t_retval, retval);
}

void QGraphicsView_items7(QGraphicsViewH handle, PPtrIntArray retval, const QPainterPathH path, Qt::ItemSelectionMode mode)
{
	QList<QGraphicsItem*> t_retval;
	t_retval = ((QGraphicsView *)handle)->items(*(const QPainterPath*)path, mode);
	copyQListTemplateToPtrIntArray(t_retval, retval);
}

QGraphicsItemH QGraphicsView_itemAt(QGraphicsViewH handle, const QPointH pos)
{
	return (QGraphicsItemH) ((QGraphicsView *)handle)->itemAt(*(const QPoint*)pos);
}

QGraphicsItemH QGraphicsView_itemAt2(QGraphicsViewH handle, int x, int y)
{
	return (QGraphicsItemH) ((QGraphicsView *)handle)->itemAt(x, y);
}

void QGraphicsView_mapToScene(QGraphicsViewH handle, PQtPointF retval, const QPointH point)
{
	*(QPointF *)retval = ((QGraphicsView *)handle)->mapToScene(*(const QPoint*)point);
}

void QGraphicsView_mapToScene2(QGraphicsViewH handle, QPolygonFH retval, PRect rect)
{
	QRect t_rect;
	copyPRectToQRect(rect, t_rect);
	*(QPolygonF *)retval = ((QGraphicsView *)handle)->mapToScene(t_rect);
}

void QGraphicsView_mapToScene3(QGraphicsViewH handle, QPolygonFH retval, const QPolygonH polygon)
{
	*(QPolygonF *)retval = ((QGraphicsView *)handle)->mapToScene(*(const QPolygon*)polygon);
}

void QGraphicsView_mapToScene4(QGraphicsViewH handle, QPainterPathH retval, const QPainterPathH path)
{
	*(QPainterPath *)retval = ((QGraphicsView *)handle)->mapToScene(*(const QPainterPath*)path);
}

void QGraphicsView_mapFromScene(QGraphicsViewH handle, PQtPoint retval, const QPointFH point)
{
	*(QPoint *)retval = ((QGraphicsView *)handle)->mapFromScene(*(const QPointF*)point);
}

void QGraphicsView_mapFromScene2(QGraphicsViewH handle, QPolygonH retval, const QRectFH rect)
{
	*(QPolygon *)retval = ((QGraphicsView *)handle)->mapFromScene(*(const QRectF*)rect);
}

void QGraphicsView_mapFromScene3(QGraphicsViewH handle, QPolygonH retval, const QPolygonFH polygon)
{
	*(QPolygon *)retval = ((QGraphicsView *)handle)->mapFromScene(*(const QPolygonF*)polygon);
}

void QGraphicsView_mapFromScene4(QGraphicsViewH handle, QPainterPathH retval, const QPainterPathH path)
{
	*(QPainterPath *)retval = ((QGraphicsView *)handle)->mapFromScene(*(const QPainterPath*)path);
}

void QGraphicsView_mapToScene5(QGraphicsViewH handle, PQtPointF retval, int x, int y)
{
	*(QPointF *)retval = ((QGraphicsView *)handle)->mapToScene(x, y);
}

void QGraphicsView_mapToScene6(QGraphicsViewH handle, QPolygonFH retval, int x, int y, int w, int h)
{
	*(QPolygonF *)retval = ((QGraphicsView *)handle)->mapToScene(x, y, w, h);
}

void QGraphicsView_mapFromScene5(QGraphicsViewH handle, PQtPoint retval, qreal x, qreal y)
{
	*(QPoint *)retval = ((QGraphicsView *)handle)->mapFromScene(x, y);
}

void QGraphicsView_mapFromScene6(QGraphicsViewH handle, QPolygonH retval, qreal x, qreal y, qreal w, qreal h)
{
	*(QPolygon *)retval = ((QGraphicsView *)handle)->mapFromScene(x, y, w, h);
}

void QGraphicsView_inputMethodQuery(QGraphicsViewH handle, QVariantH retval, Qt::InputMethodQuery query)
{
	*(QVariant *)retval = ((QGraphicsView *)handle)->inputMethodQuery(query);
}

void QGraphicsView_backgroundBrush(QGraphicsViewH handle, QBrushH retval)
{
	*(QBrush *)retval = ((QGraphicsView *)handle)->backgroundBrush();
}

void QGraphicsView_setBackgroundBrush(QGraphicsViewH handle, const QBrushH brush)
{
	((QGraphicsView *)handle)->setBackgroundBrush(*(const QBrush*)brush);
}

void QGraphicsView_foregroundBrush(QGraphicsViewH handle, QBrushH retval)
{
	*(QBrush *)retval = ((QGraphicsView *)handle)->foregroundBrush();
}

void QGraphicsView_setForegroundBrush(QGraphicsViewH handle, const QBrushH brush)
{
	((QGraphicsView *)handle)->setForegroundBrush(*(const QBrush*)brush);
}

void QGraphicsView_invalidateScene(QGraphicsViewH handle, const QRectFH rect, unsigned int layers)
{
	((QGraphicsView *)handle)->invalidateScene(*(const QRectF*)rect, (QGraphicsScene::SceneLayers)layers);
}

void QGraphicsView_updateSceneRect(QGraphicsViewH handle, const QRectFH rect)
{
	((QGraphicsView *)handle)->updateSceneRect(*(const QRectF*)rect);
}

