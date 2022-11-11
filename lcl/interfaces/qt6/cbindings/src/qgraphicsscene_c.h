//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QGRAPHICSSCENE_C_H
#define QGRAPHICSSCENE_C_H

#include <QtWidgets>
#include "pascalbind.h"

C_EXPORT QGraphicsSceneH QGraphicsScene_Create(QObjectH parent);
C_EXPORT void QGraphicsScene_Destroy(QGraphicsSceneH handle);
C_EXPORT QGraphicsSceneH QGraphicsScene_Create2(const QRectFH sceneRect, QObjectH parent);
C_EXPORT QGraphicsSceneH QGraphicsScene_Create3(qreal x, qreal y, qreal width, qreal height, QObjectH parent);
C_EXPORT void QGraphicsScene_sceneRect(QGraphicsSceneH handle, QRectFH retval);
C_EXPORT qreal QGraphicsScene_width(QGraphicsSceneH handle);
C_EXPORT qreal QGraphicsScene_height(QGraphicsSceneH handle);
C_EXPORT void QGraphicsScene_setSceneRect(QGraphicsSceneH handle, const QRectFH rect);
C_EXPORT void QGraphicsScene_setSceneRect2(QGraphicsSceneH handle, qreal x, qreal y, qreal w, qreal h);
C_EXPORT void QGraphicsScene_render(QGraphicsSceneH handle, QPainterH painter, const QRectFH target, const QRectFH source, Qt::AspectRatioMode aspectRatioMode);
C_EXPORT QGraphicsScene::ItemIndexMethod QGraphicsScene_itemIndexMethod(QGraphicsSceneH handle);
C_EXPORT void QGraphicsScene_setItemIndexMethod(QGraphicsSceneH handle, QGraphicsScene::ItemIndexMethod method);
C_EXPORT int QGraphicsScene_bspTreeDepth(QGraphicsSceneH handle);
C_EXPORT void QGraphicsScene_setBspTreeDepth(QGraphicsSceneH handle, int depth);
C_EXPORT void QGraphicsScene_itemsBoundingRect(QGraphicsSceneH handle, QRectFH retval);
C_EXPORT void QGraphicsScene_items(QGraphicsSceneH handle, PPtrIntArray retval, Qt::SortOrder order);
C_EXPORT void QGraphicsScene_items2(QGraphicsSceneH handle, PPtrIntArray retval, const QPointFH pos, Qt::ItemSelectionMode mode, Qt::SortOrder order, const QTransformH deviceTransform);
C_EXPORT void QGraphicsScene_items3(QGraphicsSceneH handle, PPtrIntArray retval, const QRectFH rect, Qt::ItemSelectionMode mode, Qt::SortOrder order, const QTransformH deviceTransform);
C_EXPORT void QGraphicsScene_items4(QGraphicsSceneH handle, PPtrIntArray retval, const QPolygonFH polygon, Qt::ItemSelectionMode mode, Qt::SortOrder order, const QTransformH deviceTransform);
C_EXPORT void QGraphicsScene_items5(QGraphicsSceneH handle, PPtrIntArray retval, const QPainterPathH path, Qt::ItemSelectionMode mode, Qt::SortOrder order, const QTransformH deviceTransform);
C_EXPORT void QGraphicsScene_collidingItems(QGraphicsSceneH handle, PPtrIntArray retval, const QGraphicsItemH item, Qt::ItemSelectionMode mode);
C_EXPORT QGraphicsItemH QGraphicsScene_itemAt(QGraphicsSceneH handle, const QPointFH pos, const QTransformH deviceTransform);
C_EXPORT void QGraphicsScene_items6(QGraphicsSceneH handle, PPtrIntArray retval, qreal x, qreal y, qreal w, qreal h, Qt::ItemSelectionMode mode, Qt::SortOrder order, const QTransformH deviceTransform);
C_EXPORT QGraphicsItemH QGraphicsScene_itemAt2(QGraphicsSceneH handle, qreal x, qreal y, const QTransformH deviceTransform);
C_EXPORT void QGraphicsScene_selectedItems(QGraphicsSceneH handle, PPtrIntArray retval);
C_EXPORT void QGraphicsScene_selectionArea(QGraphicsSceneH handle, QPainterPathH retval);
C_EXPORT void QGraphicsScene_setSelectionArea(QGraphicsSceneH handle, const QPainterPathH path, const QTransformH deviceTransform);
C_EXPORT void QGraphicsScene_setSelectionArea2(QGraphicsSceneH handle, const QPainterPathH path, Qt::ItemSelectionOperation selectionOperation, Qt::ItemSelectionMode mode, const QTransformH deviceTransform);
C_EXPORT QGraphicsItemGroupH QGraphicsScene_createItemGroup(QGraphicsSceneH handle, PPtrIntArray items);
C_EXPORT void QGraphicsScene_destroyItemGroup(QGraphicsSceneH handle, QGraphicsItemGroupH group);
C_EXPORT void QGraphicsScene_addItem(QGraphicsSceneH handle, QGraphicsItemH item);
C_EXPORT QGraphicsEllipseItemH QGraphicsScene_addEllipse(QGraphicsSceneH handle, const QRectFH rect, const QPenH pen, const QBrushH brush);
C_EXPORT QGraphicsLineItemH QGraphicsScene_addLine(QGraphicsSceneH handle, const QLineFH line, const QPenH pen);
C_EXPORT QGraphicsPathItemH QGraphicsScene_addPath(QGraphicsSceneH handle, const QPainterPathH path, const QPenH pen, const QBrushH brush);
C_EXPORT QGraphicsPixmapItemH QGraphicsScene_addPixmap(QGraphicsSceneH handle, const QPixmapH pixmap);
C_EXPORT QGraphicsPolygonItemH QGraphicsScene_addPolygon(QGraphicsSceneH handle, const QPolygonFH polygon, const QPenH pen, const QBrushH brush);
C_EXPORT QGraphicsRectItemH QGraphicsScene_addRect(QGraphicsSceneH handle, const QRectFH rect, const QPenH pen, const QBrushH brush);
C_EXPORT QGraphicsTextItemH QGraphicsScene_addText(QGraphicsSceneH handle, PWideString text, const QFontH font);
C_EXPORT QGraphicsSimpleTextItemH QGraphicsScene_addSimpleText(QGraphicsSceneH handle, PWideString text, const QFontH font);
C_EXPORT QGraphicsProxyWidgetH QGraphicsScene_addWidget(QGraphicsSceneH handle, QWidgetH widget, unsigned int wFlags);
C_EXPORT QGraphicsEllipseItemH QGraphicsScene_addEllipse2(QGraphicsSceneH handle, qreal x, qreal y, qreal w, qreal h, const QPenH pen, const QBrushH brush);
C_EXPORT QGraphicsLineItemH QGraphicsScene_addLine2(QGraphicsSceneH handle, qreal x1, qreal y1, qreal x2, qreal y2, const QPenH pen);
C_EXPORT QGraphicsRectItemH QGraphicsScene_addRect2(QGraphicsSceneH handle, qreal x, qreal y, qreal w, qreal h, const QPenH pen, const QBrushH brush);
C_EXPORT void QGraphicsScene_removeItem(QGraphicsSceneH handle, QGraphicsItemH item);
C_EXPORT QGraphicsItemH QGraphicsScene_focusItem(QGraphicsSceneH handle);
C_EXPORT void QGraphicsScene_setFocusItem(QGraphicsSceneH handle, QGraphicsItemH item, Qt::FocusReason focusReason);
C_EXPORT bool QGraphicsScene_hasFocus(QGraphicsSceneH handle);
C_EXPORT void QGraphicsScene_setFocus(QGraphicsSceneH handle, Qt::FocusReason focusReason);
C_EXPORT void QGraphicsScene_clearFocus(QGraphicsSceneH handle);
C_EXPORT void QGraphicsScene_setStickyFocus(QGraphicsSceneH handle, bool enabled);
C_EXPORT bool QGraphicsScene_stickyFocus(QGraphicsSceneH handle);
C_EXPORT QGraphicsItemH QGraphicsScene_mouseGrabberItem(QGraphicsSceneH handle);
C_EXPORT void QGraphicsScene_backgroundBrush(QGraphicsSceneH handle, QBrushH retval);
C_EXPORT void QGraphicsScene_setBackgroundBrush(QGraphicsSceneH handle, const QBrushH brush);
C_EXPORT void QGraphicsScene_foregroundBrush(QGraphicsSceneH handle, QBrushH retval);
C_EXPORT void QGraphicsScene_setForegroundBrush(QGraphicsSceneH handle, const QBrushH brush);
C_EXPORT void QGraphicsScene_inputMethodQuery(QGraphicsSceneH handle, QVariantH retval, Qt::InputMethodQuery query);
C_EXPORT void QGraphicsScene_views(QGraphicsSceneH handle, PPtrIntArray retval);
C_EXPORT void QGraphicsScene_update(QGraphicsSceneH handle, qreal x, qreal y, qreal w, qreal h);
C_EXPORT void QGraphicsScene_invalidate(QGraphicsSceneH handle, qreal x, qreal y, qreal w, qreal h, unsigned int layers);
C_EXPORT QStyleH QGraphicsScene_style(QGraphicsSceneH handle);
C_EXPORT void QGraphicsScene_setStyle(QGraphicsSceneH handle, QStyleH style);
C_EXPORT void QGraphicsScene_font(QGraphicsSceneH handle, QFontH retval);
C_EXPORT void QGraphicsScene_setFont(QGraphicsSceneH handle, const QFontH font);
C_EXPORT void QGraphicsScene_palette(QGraphicsSceneH handle, QPaletteH retval);
C_EXPORT void QGraphicsScene_setPalette(QGraphicsSceneH handle, const QPaletteH palette);
C_EXPORT bool QGraphicsScene_isActive(QGraphicsSceneH handle);
C_EXPORT QGraphicsItemH QGraphicsScene_activePanel(QGraphicsSceneH handle);
C_EXPORT void QGraphicsScene_setActivePanel(QGraphicsSceneH handle, QGraphicsItemH item);
C_EXPORT QGraphicsWidgetH QGraphicsScene_activeWindow(QGraphicsSceneH handle);
C_EXPORT void QGraphicsScene_setActiveWindow(QGraphicsSceneH handle, QGraphicsWidgetH widget);
C_EXPORT bool QGraphicsScene_sendEvent(QGraphicsSceneH handle, QGraphicsItemH item, QEventH event);
C_EXPORT void QGraphicsScene_update2(QGraphicsSceneH handle, const QRectFH rect);
C_EXPORT void QGraphicsScene_invalidate2(QGraphicsSceneH handle, const QRectFH rect, unsigned int layers);
C_EXPORT void QGraphicsScene_advance(QGraphicsSceneH handle);
C_EXPORT void QGraphicsScene_clearSelection(QGraphicsSceneH handle);
C_EXPORT void QGraphicsScene_clear(QGraphicsSceneH handle);

#endif
