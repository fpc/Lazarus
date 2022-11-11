//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qgraphicsscene_c.h"

QGraphicsSceneH QGraphicsScene_Create(QObjectH parent)
{
	return (QGraphicsSceneH) new QGraphicsScene((QObject*)parent);
}

void QGraphicsScene_Destroy(QGraphicsSceneH handle)
{
	delete (QGraphicsScene *)handle;
}

QGraphicsSceneH QGraphicsScene_Create2(const QRectFH sceneRect, QObjectH parent)
{
	return (QGraphicsSceneH) new QGraphicsScene(*(const QRectF*)sceneRect, (QObject*)parent);
}

QGraphicsSceneH QGraphicsScene_Create3(qreal x, qreal y, qreal width, qreal height, QObjectH parent)
{
	return (QGraphicsSceneH) new QGraphicsScene(x, y, width, height, (QObject*)parent);
}

void QGraphicsScene_sceneRect(QGraphicsSceneH handle, QRectFH retval)
{
	*(QRectF *)retval = ((QGraphicsScene *)handle)->sceneRect();
}

qreal QGraphicsScene_width(QGraphicsSceneH handle)
{
	return (qreal) ((QGraphicsScene *)handle)->width();
}

qreal QGraphicsScene_height(QGraphicsSceneH handle)
{
	return (qreal) ((QGraphicsScene *)handle)->height();
}

void QGraphicsScene_setSceneRect(QGraphicsSceneH handle, const QRectFH rect)
{
	((QGraphicsScene *)handle)->setSceneRect(*(const QRectF*)rect);
}

void QGraphicsScene_setSceneRect2(QGraphicsSceneH handle, qreal x, qreal y, qreal w, qreal h)
{
	((QGraphicsScene *)handle)->setSceneRect(x, y, w, h);
}

void QGraphicsScene_render(QGraphicsSceneH handle, QPainterH painter, const QRectFH target, const QRectFH source, Qt::AspectRatioMode aspectRatioMode)
{
	((QGraphicsScene *)handle)->render((QPainter*)painter, *(const QRectF*)target, *(const QRectF*)source, aspectRatioMode);
}

QGraphicsScene::ItemIndexMethod QGraphicsScene_itemIndexMethod(QGraphicsSceneH handle)
{
	return (QGraphicsScene::ItemIndexMethod) ((QGraphicsScene *)handle)->itemIndexMethod();
}

void QGraphicsScene_setItemIndexMethod(QGraphicsSceneH handle, QGraphicsScene::ItemIndexMethod method)
{
	((QGraphicsScene *)handle)->setItemIndexMethod(method);
}

int QGraphicsScene_bspTreeDepth(QGraphicsSceneH handle)
{
	return (int) ((QGraphicsScene *)handle)->bspTreeDepth();
}

void QGraphicsScene_setBspTreeDepth(QGraphicsSceneH handle, int depth)
{
	((QGraphicsScene *)handle)->setBspTreeDepth(depth);
}

void QGraphicsScene_itemsBoundingRect(QGraphicsSceneH handle, QRectFH retval)
{
	*(QRectF *)retval = ((QGraphicsScene *)handle)->itemsBoundingRect();
}

void QGraphicsScene_items(QGraphicsSceneH handle, PPtrIntArray retval, Qt::SortOrder order)
{
	QList<QGraphicsItem*> t_retval;
	t_retval = ((QGraphicsScene *)handle)->items(order);
	copyQListTemplateToPtrIntArray(t_retval, retval);
}

void QGraphicsScene_items2(QGraphicsSceneH handle, PPtrIntArray retval, const QPointFH pos, Qt::ItemSelectionMode mode, Qt::SortOrder order, const QTransformH deviceTransform)
{
	QList<QGraphicsItem*> t_retval;
	t_retval = ((QGraphicsScene *)handle)->items(*(const QPointF*)pos, mode, order, *(const QTransform*)deviceTransform);
	copyQListTemplateToPtrIntArray(t_retval, retval);
}

void QGraphicsScene_items3(QGraphicsSceneH handle, PPtrIntArray retval, const QRectFH rect, Qt::ItemSelectionMode mode, Qt::SortOrder order, const QTransformH deviceTransform)
{
	QList<QGraphicsItem*> t_retval;
	t_retval = ((QGraphicsScene *)handle)->items(*(const QRectF*)rect, mode, order, *(const QTransform*)deviceTransform);
	copyQListTemplateToPtrIntArray(t_retval, retval);
}

void QGraphicsScene_items4(QGraphicsSceneH handle, PPtrIntArray retval, const QPolygonFH polygon, Qt::ItemSelectionMode mode, Qt::SortOrder order, const QTransformH deviceTransform)
{
	QList<QGraphicsItem*> t_retval;
	t_retval = ((QGraphicsScene *)handle)->items(*(const QPolygonF*)polygon, mode, order, *(const QTransform*)deviceTransform);
	copyQListTemplateToPtrIntArray(t_retval, retval);
}

void QGraphicsScene_items5(QGraphicsSceneH handle, PPtrIntArray retval, const QPainterPathH path, Qt::ItemSelectionMode mode, Qt::SortOrder order, const QTransformH deviceTransform)
{
	QList<QGraphicsItem*> t_retval;
	t_retval = ((QGraphicsScene *)handle)->items(*(const QPainterPath*)path, mode, order, *(const QTransform*)deviceTransform);
	copyQListTemplateToPtrIntArray(t_retval, retval);
}

void QGraphicsScene_collidingItems(QGraphicsSceneH handle, PPtrIntArray retval, const QGraphicsItemH item, Qt::ItemSelectionMode mode)
{
	QList<QGraphicsItem*> t_retval;
	t_retval = ((QGraphicsScene *)handle)->collidingItems((const QGraphicsItem*)item, mode);
	copyQListTemplateToPtrIntArray(t_retval, retval);
}

QGraphicsItemH QGraphicsScene_itemAt(QGraphicsSceneH handle, const QPointFH pos, const QTransformH deviceTransform)
{
	return (QGraphicsItemH) ((QGraphicsScene *)handle)->itemAt(*(const QPointF*)pos, *(const QTransform*)deviceTransform);
}

void QGraphicsScene_items6(QGraphicsSceneH handle, PPtrIntArray retval, qreal x, qreal y, qreal w, qreal h, Qt::ItemSelectionMode mode, Qt::SortOrder order, const QTransformH deviceTransform)
{
	QList<QGraphicsItem*> t_retval;
	t_retval = ((QGraphicsScene *)handle)->items(x, y, w, h, mode, order, *(const QTransform*)deviceTransform);
	copyQListTemplateToPtrIntArray(t_retval, retval);
}

QGraphicsItemH QGraphicsScene_itemAt2(QGraphicsSceneH handle, qreal x, qreal y, const QTransformH deviceTransform)
{
	return (QGraphicsItemH) ((QGraphicsScene *)handle)->itemAt(x, y, *(const QTransform*)deviceTransform);
}

void QGraphicsScene_selectedItems(QGraphicsSceneH handle, PPtrIntArray retval)
{
	QList<QGraphicsItem*> t_retval;
	t_retval = ((QGraphicsScene *)handle)->selectedItems();
	copyQListTemplateToPtrIntArray(t_retval, retval);
}

void QGraphicsScene_selectionArea(QGraphicsSceneH handle, QPainterPathH retval)
{
	*(QPainterPath *)retval = ((QGraphicsScene *)handle)->selectionArea();
}

void QGraphicsScene_setSelectionArea(QGraphicsSceneH handle, const QPainterPathH path, const QTransformH deviceTransform)
{
	((QGraphicsScene *)handle)->setSelectionArea(*(const QPainterPath*)path, *(const QTransform*)deviceTransform);
}

void QGraphicsScene_setSelectionArea2(QGraphicsSceneH handle, const QPainterPathH path, Qt::ItemSelectionOperation selectionOperation, Qt::ItemSelectionMode mode, const QTransformH deviceTransform)
{
	((QGraphicsScene *)handle)->setSelectionArea(*(const QPainterPath*)path, selectionOperation, mode, *(const QTransform*)deviceTransform);
}

QGraphicsItemGroupH QGraphicsScene_createItemGroup(QGraphicsSceneH handle, PPtrIntArray items)
{
	QList<QGraphicsItem*> t_items;
	copyPtrIntArrayToQListTemplate(items, t_items);
	return (QGraphicsItemGroupH) ((QGraphicsScene *)handle)->createItemGroup(t_items);
}

void QGraphicsScene_destroyItemGroup(QGraphicsSceneH handle, QGraphicsItemGroupH group)
{
	((QGraphicsScene *)handle)->destroyItemGroup((QGraphicsItemGroup*)group);
}

void QGraphicsScene_addItem(QGraphicsSceneH handle, QGraphicsItemH item)
{
	((QGraphicsScene *)handle)->addItem((QGraphicsItem*)item);
}

QGraphicsEllipseItemH QGraphicsScene_addEllipse(QGraphicsSceneH handle, const QRectFH rect, const QPenH pen, const QBrushH brush)
{
	return (QGraphicsEllipseItemH) ((QGraphicsScene *)handle)->addEllipse(*(const QRectF*)rect, *(const QPen*)pen, *(const QBrush*)brush);
}

QGraphicsLineItemH QGraphicsScene_addLine(QGraphicsSceneH handle, const QLineFH line, const QPenH pen)
{
	return (QGraphicsLineItemH) ((QGraphicsScene *)handle)->addLine(*(const QLineF*)line, *(const QPen*)pen);
}

QGraphicsPathItemH QGraphicsScene_addPath(QGraphicsSceneH handle, const QPainterPathH path, const QPenH pen, const QBrushH brush)
{
	return (QGraphicsPathItemH) ((QGraphicsScene *)handle)->addPath(*(const QPainterPath*)path, *(const QPen*)pen, *(const QBrush*)brush);
}

QGraphicsPixmapItemH QGraphicsScene_addPixmap(QGraphicsSceneH handle, const QPixmapH pixmap)
{
	return (QGraphicsPixmapItemH) ((QGraphicsScene *)handle)->addPixmap(*(const QPixmap*)pixmap);
}

QGraphicsPolygonItemH QGraphicsScene_addPolygon(QGraphicsSceneH handle, const QPolygonFH polygon, const QPenH pen, const QBrushH brush)
{
	return (QGraphicsPolygonItemH) ((QGraphicsScene *)handle)->addPolygon(*(const QPolygonF*)polygon, *(const QPen*)pen, *(const QBrush*)brush);
}

QGraphicsRectItemH QGraphicsScene_addRect(QGraphicsSceneH handle, const QRectFH rect, const QPenH pen, const QBrushH brush)
{
	return (QGraphicsRectItemH) ((QGraphicsScene *)handle)->addRect(*(const QRectF*)rect, *(const QPen*)pen, *(const QBrush*)brush);
}

QGraphicsTextItemH QGraphicsScene_addText(QGraphicsSceneH handle, PWideString text, const QFontH font)
{
	QString t_text;
	copyPWideStringToQString(text, t_text);
	return (QGraphicsTextItemH) ((QGraphicsScene *)handle)->addText(t_text, *(const QFont*)font);
}

QGraphicsSimpleTextItemH QGraphicsScene_addSimpleText(QGraphicsSceneH handle, PWideString text, const QFontH font)
{
	QString t_text;
	copyPWideStringToQString(text, t_text);
	return (QGraphicsSimpleTextItemH) ((QGraphicsScene *)handle)->addSimpleText(t_text, *(const QFont*)font);
}

QGraphicsProxyWidgetH QGraphicsScene_addWidget(QGraphicsSceneH handle, QWidgetH widget, unsigned int wFlags)
{
	return (QGraphicsProxyWidgetH) ((QGraphicsScene *)handle)->addWidget((QWidget*)widget, (Qt::WindowFlags)wFlags);
}

QGraphicsEllipseItemH QGraphicsScene_addEllipse2(QGraphicsSceneH handle, qreal x, qreal y, qreal w, qreal h, const QPenH pen, const QBrushH brush)
{
	return (QGraphicsEllipseItemH) ((QGraphicsScene *)handle)->addEllipse(x, y, w, h, *(const QPen*)pen, *(const QBrush*)brush);
}

QGraphicsLineItemH QGraphicsScene_addLine2(QGraphicsSceneH handle, qreal x1, qreal y1, qreal x2, qreal y2, const QPenH pen)
{
	return (QGraphicsLineItemH) ((QGraphicsScene *)handle)->addLine(x1, y1, x2, y2, *(const QPen*)pen);
}

QGraphicsRectItemH QGraphicsScene_addRect2(QGraphicsSceneH handle, qreal x, qreal y, qreal w, qreal h, const QPenH pen, const QBrushH brush)
{
	return (QGraphicsRectItemH) ((QGraphicsScene *)handle)->addRect(x, y, w, h, *(const QPen*)pen, *(const QBrush*)brush);
}

void QGraphicsScene_removeItem(QGraphicsSceneH handle, QGraphicsItemH item)
{
	((QGraphicsScene *)handle)->removeItem((QGraphicsItem*)item);
}

QGraphicsItemH QGraphicsScene_focusItem(QGraphicsSceneH handle)
{
	return (QGraphicsItemH) ((QGraphicsScene *)handle)->focusItem();
}

void QGraphicsScene_setFocusItem(QGraphicsSceneH handle, QGraphicsItemH item, Qt::FocusReason focusReason)
{
	((QGraphicsScene *)handle)->setFocusItem((QGraphicsItem*)item, focusReason);
}

bool QGraphicsScene_hasFocus(QGraphicsSceneH handle)
{
	return (bool) ((QGraphicsScene *)handle)->hasFocus();
}

void QGraphicsScene_setFocus(QGraphicsSceneH handle, Qt::FocusReason focusReason)
{
	((QGraphicsScene *)handle)->setFocus(focusReason);
}

void QGraphicsScene_clearFocus(QGraphicsSceneH handle)
{
	((QGraphicsScene *)handle)->clearFocus();
}

void QGraphicsScene_setStickyFocus(QGraphicsSceneH handle, bool enabled)
{
	((QGraphicsScene *)handle)->setStickyFocus(enabled);
}

bool QGraphicsScene_stickyFocus(QGraphicsSceneH handle)
{
	return (bool) ((QGraphicsScene *)handle)->stickyFocus();
}

QGraphicsItemH QGraphicsScene_mouseGrabberItem(QGraphicsSceneH handle)
{
	return (QGraphicsItemH) ((QGraphicsScene *)handle)->mouseGrabberItem();
}

void QGraphicsScene_backgroundBrush(QGraphicsSceneH handle, QBrushH retval)
{
	*(QBrush *)retval = ((QGraphicsScene *)handle)->backgroundBrush();
}

void QGraphicsScene_setBackgroundBrush(QGraphicsSceneH handle, const QBrushH brush)
{
	((QGraphicsScene *)handle)->setBackgroundBrush(*(const QBrush*)brush);
}

void QGraphicsScene_foregroundBrush(QGraphicsSceneH handle, QBrushH retval)
{
	*(QBrush *)retval = ((QGraphicsScene *)handle)->foregroundBrush();
}

void QGraphicsScene_setForegroundBrush(QGraphicsSceneH handle, const QBrushH brush)
{
	((QGraphicsScene *)handle)->setForegroundBrush(*(const QBrush*)brush);
}

void QGraphicsScene_inputMethodQuery(QGraphicsSceneH handle, QVariantH retval, Qt::InputMethodQuery query)
{
	*(QVariant *)retval = ((QGraphicsScene *)handle)->inputMethodQuery(query);
}

void QGraphicsScene_views(QGraphicsSceneH handle, PPtrIntArray retval)
{
	QList<QGraphicsView*> t_retval;
	t_retval = ((QGraphicsScene *)handle)->views();
	copyQListTemplateToPtrIntArray(t_retval, retval);
}

void QGraphicsScene_update(QGraphicsSceneH handle, qreal x, qreal y, qreal w, qreal h)
{
	((QGraphicsScene *)handle)->update(x, y, w, h);
}

void QGraphicsScene_invalidate(QGraphicsSceneH handle, qreal x, qreal y, qreal w, qreal h, unsigned int layers)
{
	((QGraphicsScene *)handle)->invalidate(x, y, w, h, (QGraphicsScene::SceneLayers)layers);
}

QStyleH QGraphicsScene_style(QGraphicsSceneH handle)
{
	return (QStyleH) ((QGraphicsScene *)handle)->style();
}

void QGraphicsScene_setStyle(QGraphicsSceneH handle, QStyleH style)
{
	((QGraphicsScene *)handle)->setStyle((QStyle*)style);
}

void QGraphicsScene_font(QGraphicsSceneH handle, QFontH retval)
{
	*(QFont *)retval = ((QGraphicsScene *)handle)->font();
}

void QGraphicsScene_setFont(QGraphicsSceneH handle, const QFontH font)
{
	((QGraphicsScene *)handle)->setFont(*(const QFont*)font);
}

void QGraphicsScene_palette(QGraphicsSceneH handle, QPaletteH retval)
{
	*(QPalette *)retval = ((QGraphicsScene *)handle)->palette();
}

void QGraphicsScene_setPalette(QGraphicsSceneH handle, const QPaletteH palette)
{
	((QGraphicsScene *)handle)->setPalette(*(const QPalette*)palette);
}

bool QGraphicsScene_isActive(QGraphicsSceneH handle)
{
	return (bool) ((QGraphicsScene *)handle)->isActive();
}

QGraphicsItemH QGraphicsScene_activePanel(QGraphicsSceneH handle)
{
	return (QGraphicsItemH) ((QGraphicsScene *)handle)->activePanel();
}

void QGraphicsScene_setActivePanel(QGraphicsSceneH handle, QGraphicsItemH item)
{
	((QGraphicsScene *)handle)->setActivePanel((QGraphicsItem*)item);
}

QGraphicsWidgetH QGraphicsScene_activeWindow(QGraphicsSceneH handle)
{
	return (QGraphicsWidgetH) ((QGraphicsScene *)handle)->activeWindow();
}

void QGraphicsScene_setActiveWindow(QGraphicsSceneH handle, QGraphicsWidgetH widget)
{
	((QGraphicsScene *)handle)->setActiveWindow((QGraphicsWidget*)widget);
}

bool QGraphicsScene_sendEvent(QGraphicsSceneH handle, QGraphicsItemH item, QEventH event)
{
	return (bool) ((QGraphicsScene *)handle)->sendEvent((QGraphicsItem*)item, (QEvent*)event);
}

void QGraphicsScene_update2(QGraphicsSceneH handle, const QRectFH rect)
{
	((QGraphicsScene *)handle)->update(*(const QRectF*)rect);
}

void QGraphicsScene_invalidate2(QGraphicsSceneH handle, const QRectFH rect, unsigned int layers)
{
	((QGraphicsScene *)handle)->invalidate(*(const QRectF*)rect, (QGraphicsScene::SceneLayers)layers);
}

void QGraphicsScene_advance(QGraphicsSceneH handle)
{
	((QGraphicsScene *)handle)->advance();
}

void QGraphicsScene_clearSelection(QGraphicsSceneH handle)
{
	((QGraphicsScene *)handle)->clearSelection();
}

void QGraphicsScene_clear(QGraphicsSceneH handle)
{
	((QGraphicsScene *)handle)->clear();
}

