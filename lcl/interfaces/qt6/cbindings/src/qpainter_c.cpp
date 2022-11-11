//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qpainter_c.h"

QPainterH QPainter_Create()
{
	return (QPainterH) new QPainter();
}

void QPainter_Destroy(QPainterH handle)
{
	delete (QPainter *)handle;
}

QPainterH QPainter_Create2(QPaintDeviceH AnonParam1)
{
	return (QPainterH) new QPainter((QPaintDevice*)AnonParam1);
}

QPaintDeviceH QPainter_device(QPainterH handle)
{
	return (QPaintDeviceH) ((QPainter *)handle)->device();
}

bool QPainter_begin(QPainterH handle, QPaintDeviceH AnonParam1)
{
	return (bool) ((QPainter *)handle)->begin((QPaintDevice*)AnonParam1);
}

bool QPainter_end(QPainterH handle)
{
	return (bool) ((QPainter *)handle)->end();
}

bool QPainter_isActive(QPainterH handle)
{
	return (bool) ((QPainter *)handle)->isActive();
}

void QPainter_setCompositionMode(QPainterH handle, QPainter::CompositionMode mode)
{
	((QPainter *)handle)->setCompositionMode(mode);
}

QPainter::CompositionMode QPainter_compositionMode(QPainterH handle)
{
	return (QPainter::CompositionMode) ((QPainter *)handle)->compositionMode();
}

const QFontH QPainter_font(QPainterH handle)
{
	return (const QFontH) &((QPainter *)handle)->font();
}

void QPainter_setFont(QPainterH handle, const QFontH f)
{
	((QPainter *)handle)->setFont(*(const QFont*)f);
}

void QPainter_fontMetrics(QPainterH handle, QFontMetricsH retval)
{
	*(QFontMetrics *)retval = ((QPainter *)handle)->fontMetrics();
}

void QPainter_fontInfo(QPainterH handle, QFontInfoH retval)
{
	*(QFontInfo *)retval = ((QPainter *)handle)->fontInfo();
}

void QPainter_setPen(QPainterH handle, const QColorH color)
{
	((QPainter *)handle)->setPen(*(const QColor*)color);
}

void QPainter_setPen2(QPainterH handle, const QPenH pen)
{
	((QPainter *)handle)->setPen(*(const QPen*)pen);
}

void QPainter_setPen3(QPainterH handle, Qt::PenStyle style)
{
	((QPainter *)handle)->setPen(style);
}

const QPenH QPainter_pen(QPainterH handle)
{
	return (const QPenH) &((QPainter *)handle)->pen();
}

void QPainter_setBrush(QPainterH handle, const QBrushH brush)
{
	((QPainter *)handle)->setBrush(*(const QBrush*)brush);
}

void QPainter_setBrush2(QPainterH handle, Qt::BrushStyle style)
{
	((QPainter *)handle)->setBrush(style);
}

const QBrushH QPainter_brush(QPainterH handle)
{
	return (const QBrushH) &((QPainter *)handle)->brush();
}

void QPainter_setBackgroundMode(QPainterH handle, Qt::BGMode mode)
{
	((QPainter *)handle)->setBackgroundMode(mode);
}

Qt::BGMode QPainter_backgroundMode(QPainterH handle)
{
	return (Qt::BGMode) ((QPainter *)handle)->backgroundMode();
}

void QPainter_brushOrigin(QPainterH handle, PQtPoint retval)
{
	*(QPoint *)retval = ((QPainter *)handle)->brushOrigin();
}

void QPainter_setBrushOrigin(QPainterH handle, int x, int y)
{
	((QPainter *)handle)->setBrushOrigin(x, y);
}

void QPainter_setBrushOrigin2(QPainterH handle, const QPointH AnonParam1)
{
	((QPainter *)handle)->setBrushOrigin(*(const QPoint*)AnonParam1);
}

void QPainter_setBrushOrigin3(QPainterH handle, const QPointFH AnonParam1)
{
	((QPainter *)handle)->setBrushOrigin(*(const QPointF*)AnonParam1);
}

void QPainter_setBackground(QPainterH handle, const QBrushH bg)
{
	((QPainter *)handle)->setBackground(*(const QBrush*)bg);
}

const QBrushH QPainter_background(QPainterH handle)
{
	return (const QBrushH) &((QPainter *)handle)->background();
}

qreal QPainter_opacity(QPainterH handle)
{
	return (qreal) ((QPainter *)handle)->opacity();
}

void QPainter_setOpacity(QPainterH handle, qreal opacity)
{
	((QPainter *)handle)->setOpacity(opacity);
}

void QPainter_clipRegion(QPainterH handle, QRegionH retval)
{
	*(QRegion *)retval = ((QPainter *)handle)->clipRegion();
}

void QPainter_clipPath(QPainterH handle, QPainterPathH retval)
{
	*(QPainterPath *)retval = ((QPainter *)handle)->clipPath();
}

void QPainter_setClipRect(QPainterH handle, const QRectFH AnonParam1, Qt::ClipOperation op)
{
	((QPainter *)handle)->setClipRect(*(const QRectF*)AnonParam1, op);
}

void QPainter_setClipRect2(QPainterH handle, PRect AnonParam1, Qt::ClipOperation op)
{
	QRect t_AnonParam1;
	copyPRectToQRect(AnonParam1, t_AnonParam1);
	((QPainter *)handle)->setClipRect(t_AnonParam1, op);
}

void QPainter_setClipRect3(QPainterH handle, int x, int y, int w, int h, Qt::ClipOperation op)
{
	((QPainter *)handle)->setClipRect(x, y, w, h, op);
}

void QPainter_setClipRegion(QPainterH handle, const QRegionH AnonParam1, Qt::ClipOperation op)
{
	((QPainter *)handle)->setClipRegion(*(const QRegion*)AnonParam1, op);
}

void QPainter_setClipPath(QPainterH handle, const QPainterPathH path, Qt::ClipOperation op)
{
	((QPainter *)handle)->setClipPath(*(const QPainterPath*)path, op);
}

void QPainter_setClipping(QPainterH handle, bool enable)
{
	((QPainter *)handle)->setClipping(enable);
}

bool QPainter_hasClipping(QPainterH handle)
{
	return (bool) ((QPainter *)handle)->hasClipping();
}

void QPainter_clipBoundingRect(QPainterH handle, QRectFH retval)
{
	*(QRectF *)retval = ((QPainter *)handle)->clipBoundingRect();
}

void QPainter_save(QPainterH handle)
{
	((QPainter *)handle)->save();
}

void QPainter_restore(QPainterH handle)
{
	((QPainter *)handle)->restore();
}

void QPainter_setTransform(QPainterH handle, const QTransformH transform, bool combine)
{
	((QPainter *)handle)->setTransform(*(const QTransform*)transform, combine);
}

const QTransformH QPainter_transform(QPainterH handle)
{
	return (const QTransformH) &((QPainter *)handle)->transform();
}

const QTransformH QPainter_deviceTransform(QPainterH handle)
{
	return (const QTransformH) &((QPainter *)handle)->deviceTransform();
}

void QPainter_resetTransform(QPainterH handle)
{
	((QPainter *)handle)->resetTransform();
}

void QPainter_setWorldTransform(QPainterH handle, const QTransformH matrix, bool combine)
{
	((QPainter *)handle)->setWorldTransform(*(const QTransform*)matrix, combine);
}

const QTransformH QPainter_worldTransform(QPainterH handle)
{
	return (const QTransformH) &((QPainter *)handle)->worldTransform();
}

void QPainter_combinedTransform(QPainterH handle, QTransformH retval)
{
	*(QTransform *)retval = ((QPainter *)handle)->combinedTransform();
}

void QPainter_setWorldMatrixEnabled(QPainterH handle, bool enabled)
{
	((QPainter *)handle)->setWorldMatrixEnabled(enabled);
}

bool QPainter_worldMatrixEnabled(QPainterH handle)
{
	return (bool) ((QPainter *)handle)->worldMatrixEnabled();
}

void QPainter_scale(QPainterH handle, qreal sx, qreal sy)
{
	((QPainter *)handle)->scale(sx, sy);
}

void QPainter_shear(QPainterH handle, qreal sh, qreal sv)
{
	((QPainter *)handle)->shear(sh, sv);
}

void QPainter_rotate(QPainterH handle, qreal a)
{
	((QPainter *)handle)->rotate(a);
}

void QPainter_translate(QPainterH handle, const QPointFH offset)
{
	((QPainter *)handle)->translate(*(const QPointF*)offset);
}

void QPainter_translate2(QPainterH handle, const QPointH offset)
{
	((QPainter *)handle)->translate(*(const QPoint*)offset);
}

void QPainter_translate3(QPainterH handle, qreal dx, qreal dy)
{
	((QPainter *)handle)->translate(dx, dy);
}

void QPainter_window(QPainterH handle, PRect retval)
{
	QRect t_retval;
	t_retval = ((QPainter *)handle)->window();
	copyQRectToPRect(t_retval, retval);
}

void QPainter_setWindow(QPainterH handle, PRect window)
{
	QRect t_window;
	copyPRectToQRect(window, t_window);
	((QPainter *)handle)->setWindow(t_window);
}

void QPainter_setWindow2(QPainterH handle, int x, int y, int w, int h)
{
	((QPainter *)handle)->setWindow(x, y, w, h);
}

void QPainter_viewport(QPainterH handle, PRect retval)
{
	QRect t_retval;
	t_retval = ((QPainter *)handle)->viewport();
	copyQRectToPRect(t_retval, retval);
}

void QPainter_setViewport(QPainterH handle, PRect viewport)
{
	QRect t_viewport;
	copyPRectToQRect(viewport, t_viewport);
	((QPainter *)handle)->setViewport(t_viewport);
}

void QPainter_setViewport2(QPainterH handle, int x, int y, int w, int h)
{
	((QPainter *)handle)->setViewport(x, y, w, h);
}

void QPainter_setViewTransformEnabled(QPainterH handle, bool enable)
{
	((QPainter *)handle)->setViewTransformEnabled(enable);
}

bool QPainter_viewTransformEnabled(QPainterH handle)
{
	return (bool) ((QPainter *)handle)->viewTransformEnabled();
}

void QPainter_strokePath(QPainterH handle, const QPainterPathH path, const QPenH pen)
{
	((QPainter *)handle)->strokePath(*(const QPainterPath*)path, *(const QPen*)pen);
}

void QPainter_fillPath(QPainterH handle, const QPainterPathH path, const QBrushH brush)
{
	((QPainter *)handle)->fillPath(*(const QPainterPath*)path, *(const QBrush*)brush);
}

void QPainter_drawPath(QPainterH handle, const QPainterPathH path)
{
	((QPainter *)handle)->drawPath(*(const QPainterPath*)path);
}

void QPainter_drawPoint(QPainterH handle, const QPointFH pt)
{
	((QPainter *)handle)->drawPoint(*(const QPointF*)pt);
}

void QPainter_drawPoint2(QPainterH handle, const QPointH p)
{
	((QPainter *)handle)->drawPoint(*(const QPoint*)p);
}

void QPainter_drawPoint3(QPainterH handle, int x, int y)
{
	((QPainter *)handle)->drawPoint(x, y);
}

void QPainter_drawPoints(QPainterH handle, const QPointFH points, int pointCount)
{
	((QPainter *)handle)->drawPoints((const QPointF*)points, pointCount);
}

void QPainter_drawPoints2(QPainterH handle, const QPolygonFH points)
{
	((QPainter *)handle)->drawPoints(*(const QPolygonF*)points);
}

void QPainter_drawPoints3(QPainterH handle, const QPointH points, int pointCount)
{
	((QPainter *)handle)->drawPoints((const QPoint*)points, pointCount);
}

void QPainter_drawPoints4(QPainterH handle, const QPolygonH points)
{
	((QPainter *)handle)->drawPoints(*(const QPolygon*)points);
}

void QPainter_drawLine(QPainterH handle, const QLineFH line)
{
	((QPainter *)handle)->drawLine(*(const QLineF*)line);
}

void QPainter_drawLine2(QPainterH handle, const QLineH line)
{
	((QPainter *)handle)->drawLine(*(const QLine*)line);
}

void QPainter_drawLine3(QPainterH handle, int x1, int y1, int x2, int y2)
{
	((QPainter *)handle)->drawLine(x1, y1, x2, y2);
}

void QPainter_drawLine4(QPainterH handle, const QPointH p1, const QPointH p2)
{
	((QPainter *)handle)->drawLine(*(const QPoint*)p1, *(const QPoint*)p2);
}

void QPainter_drawLine5(QPainterH handle, const QPointFH p1, const QPointFH p2)
{
	((QPainter *)handle)->drawLine(*(const QPointF*)p1, *(const QPointF*)p2);
}

void QPainter_drawLines(QPainterH handle, const QLineFH lines, int lineCount)
{
	((QPainter *)handle)->drawLines((const QLineF*)lines, lineCount);
}

void QPainter_drawLines2(QPainterH handle, const QPointFH pointPairs, int lineCount)
{
	((QPainter *)handle)->drawLines((const QPointF*)pointPairs, lineCount);
}

void QPainter_drawLines3(QPainterH handle, const QLineH lines, int lineCount)
{
	((QPainter *)handle)->drawLines((const QLine*)lines, lineCount);
}

void QPainter_drawLines4(QPainterH handle, const QPointH pointPairs, int lineCount)
{
	((QPainter *)handle)->drawLines((const QPoint*)pointPairs, lineCount);
}

void QPainter_drawRect(QPainterH handle, const QRectFH rect)
{
	((QPainter *)handle)->drawRect(*(const QRectF*)rect);
}

void QPainter_drawRect2(QPainterH handle, int x1, int y1, int w, int h)
{
	((QPainter *)handle)->drawRect(x1, y1, w, h);
}

void QPainter_drawRect3(QPainterH handle, PRect rect)
{
	QRect t_rect;
	copyPRectToQRect(rect, t_rect);
	((QPainter *)handle)->drawRect(t_rect);
}

void QPainter_drawRects(QPainterH handle, const QRectFH rects, int rectCount)
{
	((QPainter *)handle)->drawRects((const QRectF*)rects, rectCount);
}

void QPainter_drawRects2(QPainterH handle, PRect rects, int rectCount)
{
	QRect t_rects;
	if ( rects )
		copyPRectToQRect(rects, t_rects);
	((QPainter *)handle)->drawRects(rects ? &t_rects : NULL, rectCount);
}

void QPainter_drawEllipse(QPainterH handle, const QRectFH r)
{
	((QPainter *)handle)->drawEllipse(*(const QRectF*)r);
}

void QPainter_drawEllipse2(QPainterH handle, PRect r)
{
	QRect t_r;
	copyPRectToQRect(r, t_r);
	((QPainter *)handle)->drawEllipse(t_r);
}

void QPainter_drawEllipse3(QPainterH handle, int x, int y, int w, int h)
{
	((QPainter *)handle)->drawEllipse(x, y, w, h);
}

void QPainter_drawEllipse4(QPainterH handle, const QPointFH center, qreal rx, qreal ry)
{
	((QPainter *)handle)->drawEllipse(*(const QPointF*)center, rx, ry);
}

void QPainter_drawEllipse5(QPainterH handle, const QPointH center, int rx, int ry)
{
	((QPainter *)handle)->drawEllipse(*(const QPoint*)center, rx, ry);
}

void QPainter_drawPolyline(QPainterH handle, const QPointFH points, int pointCount)
{
	((QPainter *)handle)->drawPolyline((const QPointF*)points, pointCount);
}

void QPainter_drawPolyline2(QPainterH handle, const QPolygonFH polyline)
{
	((QPainter *)handle)->drawPolyline(*(const QPolygonF*)polyline);
}

void QPainter_drawPolyline3(QPainterH handle, const QPointH points, int pointCount)
{
	((QPainter *)handle)->drawPolyline((const QPoint*)points, pointCount);
}

void QPainter_drawPolyline4(QPainterH handle, const QPolygonH polygon)
{
	((QPainter *)handle)->drawPolyline(*(const QPolygon*)polygon);
}

void QPainter_drawPolygon(QPainterH handle, const QPointFH points, int pointCount, Qt::FillRule fillRule)
{
	((QPainter *)handle)->drawPolygon((const QPointF*)points, pointCount, fillRule);
}

void QPainter_drawPolygon2(QPainterH handle, const QPolygonFH polygon, Qt::FillRule fillRule)
{
	((QPainter *)handle)->drawPolygon(*(const QPolygonF*)polygon, fillRule);
}

void QPainter_drawPolygon3(QPainterH handle, const QPointH points, int pointCount, Qt::FillRule fillRule)
{
	((QPainter *)handle)->drawPolygon((const QPoint*)points, pointCount, fillRule);
}

void QPainter_drawPolygon4(QPainterH handle, const QPolygonH polygon, Qt::FillRule fillRule)
{
	((QPainter *)handle)->drawPolygon(*(const QPolygon*)polygon, fillRule);
}

void QPainter_drawConvexPolygon(QPainterH handle, const QPointFH points, int pointCount)
{
	((QPainter *)handle)->drawConvexPolygon((const QPointF*)points, pointCount);
}

void QPainter_drawConvexPolygon2(QPainterH handle, const QPolygonFH polygon)
{
	((QPainter *)handle)->drawConvexPolygon(*(const QPolygonF*)polygon);
}

void QPainter_drawConvexPolygon3(QPainterH handle, const QPointH points, int pointCount)
{
	((QPainter *)handle)->drawConvexPolygon((const QPoint*)points, pointCount);
}

void QPainter_drawConvexPolygon4(QPainterH handle, const QPolygonH polygon)
{
	((QPainter *)handle)->drawConvexPolygon(*(const QPolygon*)polygon);
}

void QPainter_drawArc(QPainterH handle, const QRectFH rect, int a, int alen)
{
	((QPainter *)handle)->drawArc(*(const QRectF*)rect, a, alen);
}

void QPainter_drawArc2(QPainterH handle, PRect AnonParam1, int a, int alen)
{
	QRect t_AnonParam1;
	copyPRectToQRect(AnonParam1, t_AnonParam1);
	((QPainter *)handle)->drawArc(t_AnonParam1, a, alen);
}

void QPainter_drawArc3(QPainterH handle, int x, int y, int w, int h, int a, int alen)
{
	((QPainter *)handle)->drawArc(x, y, w, h, a, alen);
}

void QPainter_drawPie(QPainterH handle, const QRectFH rect, int a, int alen)
{
	((QPainter *)handle)->drawPie(*(const QRectF*)rect, a, alen);
}

void QPainter_drawPie2(QPainterH handle, int x, int y, int w, int h, int a, int alen)
{
	((QPainter *)handle)->drawPie(x, y, w, h, a, alen);
}

void QPainter_drawPie3(QPainterH handle, PRect AnonParam1, int a, int alen)
{
	QRect t_AnonParam1;
	copyPRectToQRect(AnonParam1, t_AnonParam1);
	((QPainter *)handle)->drawPie(t_AnonParam1, a, alen);
}

void QPainter_drawChord(QPainterH handle, const QRectFH rect, int a, int alen)
{
	((QPainter *)handle)->drawChord(*(const QRectF*)rect, a, alen);
}

void QPainter_drawChord2(QPainterH handle, int x, int y, int w, int h, int a, int alen)
{
	((QPainter *)handle)->drawChord(x, y, w, h, a, alen);
}

void QPainter_drawChord3(QPainterH handle, PRect AnonParam1, int a, int alen)
{
	QRect t_AnonParam1;
	copyPRectToQRect(AnonParam1, t_AnonParam1);
	((QPainter *)handle)->drawChord(t_AnonParam1, a, alen);
}

void QPainter_drawRoundedRect(QPainterH handle, const QRectFH rect, qreal xRadius, qreal yRadius, Qt::SizeMode mode)
{
	((QPainter *)handle)->drawRoundedRect(*(const QRectF*)rect, xRadius, yRadius, mode);
}

void QPainter_drawRoundedRect2(QPainterH handle, int x, int y, int w, int h, qreal xRadius, qreal yRadius, Qt::SizeMode mode)
{
	((QPainter *)handle)->drawRoundedRect(x, y, w, h, xRadius, yRadius, mode);
}

void QPainter_drawRoundedRect3(QPainterH handle, PRect rect, qreal xRadius, qreal yRadius, Qt::SizeMode mode)
{
	QRect t_rect;
	copyPRectToQRect(rect, t_rect);
	((QPainter *)handle)->drawRoundedRect(t_rect, xRadius, yRadius, mode);
}

void QPainter_drawRoundRect(QPainterH handle, const QRectFH r, int xround, int yround)
{
	((QPainter *)handle)->drawRoundedRect(*(const QRectF*)r, xround, yround);
}

void QPainter_drawRoundRect2(QPainterH handle, int x, int y, int w, int h, int AnonParam5, int AnonParam6)
{
	((QPainter *)handle)->drawRoundedRect(x, y, w, h, AnonParam5, AnonParam6);
}

void QPainter_drawRoundRect3(QPainterH handle, PRect r, int xround, int yround)
{
	QRect t_r;
	copyPRectToQRect(r, t_r);
	((QPainter *)handle)->drawRoundedRect(t_r, xround, yround);
}

void QPainter_drawTiledPixmap(QPainterH handle, const QRectFH rect, const QPixmapH pm, const QPointFH offset)
{
	((QPainter *)handle)->drawTiledPixmap(*(const QRectF*)rect, *(const QPixmap*)pm, *(const QPointF*)offset);
}

void QPainter_drawTiledPixmap2(QPainterH handle, int x, int y, int w, int h, const QPixmapH AnonParam5, int sx, int sy)
{
	((QPainter *)handle)->drawTiledPixmap(x, y, w, h, *(const QPixmap*)AnonParam5, sx, sy);
}

void QPainter_drawTiledPixmap3(QPainterH handle, PRect AnonParam1, const QPixmapH AnonParam2, const QPointH AnonParam3)
{
	QRect t_AnonParam1;
	copyPRectToQRect(AnonParam1, t_AnonParam1);
	((QPainter *)handle)->drawTiledPixmap(t_AnonParam1, *(const QPixmap*)AnonParam2, *(const QPoint*)AnonParam3);
}

void QPainter_drawPicture(QPainterH handle, const QPointFH p, const QPictureH picture)
{
	((QPainter *)handle)->drawPicture(*(const QPointF*)p, *(const QPicture*)picture);
}

void QPainter_drawPicture2(QPainterH handle, int x, int y, const QPictureH picture)
{
	((QPainter *)handle)->drawPicture(x, y, *(const QPicture*)picture);
}

void QPainter_drawPicture3(QPainterH handle, const QPointH p, const QPictureH picture)
{
	((QPainter *)handle)->drawPicture(*(const QPoint*)p, *(const QPicture*)picture);
}

void QPainter_drawPixmap(QPainterH handle, const QRectFH targetRect, const QPixmapH pixmap, const QRectFH sourceRect)
{
	((QPainter *)handle)->drawPixmap(*(const QRectF*)targetRect, *(const QPixmap*)pixmap, *(const QRectF*)sourceRect);
}

void QPainter_drawPixmap2(QPainterH handle, PRect targetRect, const QPixmapH pixmap, PRect sourceRect)
{
	QRect t_targetRect;
	QRect t_sourceRect;
	copyPRectToQRect(targetRect, t_targetRect);
	copyPRectToQRect(sourceRect, t_sourceRect);
	((QPainter *)handle)->drawPixmap(t_targetRect, *(const QPixmap*)pixmap, t_sourceRect);
}

void QPainter_drawPixmap3(QPainterH handle, int x, int y, int w, int h, const QPixmapH pm, int sx, int sy, int sw, int sh)
{
	((QPainter *)handle)->drawPixmap(x, y, w, h, *(const QPixmap*)pm, sx, sy, sw, sh);
}

void QPainter_drawPixmap4(QPainterH handle, int x, int y, const QPixmapH pm, int sx, int sy, int sw, int sh)
{
	((QPainter *)handle)->drawPixmap(x, y, *(const QPixmap*)pm, sx, sy, sw, sh);
}

void QPainter_drawPixmap5(QPainterH handle, const QPointFH p, const QPixmapH pm, const QRectFH sr)
{
	((QPainter *)handle)->drawPixmap(*(const QPointF*)p, *(const QPixmap*)pm, *(const QRectF*)sr);
}

void QPainter_drawPixmap6(QPainterH handle, const QPointH p, const QPixmapH pm, PRect sr)
{
	QRect t_sr;
	copyPRectToQRect(sr, t_sr);
	((QPainter *)handle)->drawPixmap(*(const QPoint*)p, *(const QPixmap*)pm, t_sr);
}

void QPainter_drawPixmap7(QPainterH handle, const QPointFH p, const QPixmapH pm)
{
	((QPainter *)handle)->drawPixmap(*(const QPointF*)p, *(const QPixmap*)pm);
}

void QPainter_drawPixmap8(QPainterH handle, const QPointH p, const QPixmapH pm)
{
	((QPainter *)handle)->drawPixmap(*(const QPoint*)p, *(const QPixmap*)pm);
}

void QPainter_drawPixmap9(QPainterH handle, int x, int y, const QPixmapH pm)
{
	((QPainter *)handle)->drawPixmap(x, y, *(const QPixmap*)pm);
}

void QPainter_drawPixmap10(QPainterH handle, PRect r, const QPixmapH pm)
{
	QRect t_r;
	copyPRectToQRect(r, t_r);
	((QPainter *)handle)->drawPixmap(t_r, *(const QPixmap*)pm);
}

void QPainter_drawPixmap11(QPainterH handle, int x, int y, int w, int h, const QPixmapH pm)
{
	((QPainter *)handle)->drawPixmap(x, y, w, h, *(const QPixmap*)pm);
}

void QPainter_drawImage(QPainterH handle, const QRectFH targetRect, const QImageH image, const QRectFH sourceRect, unsigned int flags)
{
	((QPainter *)handle)->drawImage(*(const QRectF*)targetRect, *(const QImage*)image, *(const QRectF*)sourceRect, (Qt::ImageConversionFlags)flags);
}

void QPainter_drawImage2(QPainterH handle, PRect targetRect, const QImageH image, PRect sourceRect, unsigned int flags)
{
	QRect t_targetRect;
	QRect t_sourceRect;
	copyPRectToQRect(targetRect, t_targetRect);
	copyPRectToQRect(sourceRect, t_sourceRect);
	((QPainter *)handle)->drawImage(t_targetRect, *(const QImage*)image, t_sourceRect, (Qt::ImageConversionFlags)flags);
}

void QPainter_drawImage3(QPainterH handle, const QPointFH p, const QImageH image, const QRectFH sr, unsigned int flags)
{
	((QPainter *)handle)->drawImage(*(const QPointF*)p, *(const QImage*)image, *(const QRectF*)sr, (Qt::ImageConversionFlags)flags);
}

void QPainter_drawImage4(QPainterH handle, const QPointH p, const QImageH image, PRect sr, unsigned int flags)
{
	QRect t_sr;
	copyPRectToQRect(sr, t_sr);
	((QPainter *)handle)->drawImage(*(const QPoint*)p, *(const QImage*)image, t_sr, (Qt::ImageConversionFlags)flags);
}

void QPainter_drawImage5(QPainterH handle, const QRectFH r, const QImageH image)
{
	((QPainter *)handle)->drawImage(*(const QRectF*)r, *(const QImage*)image);
}

void QPainter_drawImage6(QPainterH handle, PRect r, const QImageH image)
{
	QRect t_r;
	copyPRectToQRect(r, t_r);
	((QPainter *)handle)->drawImage(t_r, *(const QImage*)image);
}

void QPainter_drawImage7(QPainterH handle, const QPointFH p, const QImageH image)
{
	((QPainter *)handle)->drawImage(*(const QPointF*)p, *(const QImage*)image);
}

void QPainter_drawImage8(QPainterH handle, const QPointH p, const QImageH image)
{
	((QPainter *)handle)->drawImage(*(const QPoint*)p, *(const QImage*)image);
}

void QPainter_drawImage9(QPainterH handle, int x, int y, const QImageH image, int sx, int sy, int sw, int sh, unsigned int flags)
{
	((QPainter *)handle)->drawImage(x, y, *(const QImage*)image, sx, sy, sw, sh, (Qt::ImageConversionFlags)flags);
}

void QPainter_setLayoutDirection(QPainterH handle, Qt::LayoutDirection direction)
{
	((QPainter *)handle)->setLayoutDirection(direction);
}

Qt::LayoutDirection QPainter_layoutDirection(QPainterH handle)
{
	return (Qt::LayoutDirection) ((QPainter *)handle)->layoutDirection();
}

void QPainter_drawGlyphRun(QPainterH handle, const QPointFH position, const QGlyphRunH glyphRun)
{
	((QPainter *)handle)->drawGlyphRun(*(const QPointF*)position, *(const QGlyphRun*)glyphRun);
}

void QPainter_drawStaticText(QPainterH handle, const QPointFH topLeftPosition, const QStaticTextH staticText)
{
	((QPainter *)handle)->drawStaticText(*(const QPointF*)topLeftPosition, *(const QStaticText*)staticText);
}

void QPainter_drawStaticText2(QPainterH handle, const QPointH topLeftPosition, const QStaticTextH staticText)
{
	((QPainter *)handle)->drawStaticText(*(const QPoint*)topLeftPosition, *(const QStaticText*)staticText);
}

void QPainter_drawStaticText3(QPainterH handle, int left, int top, const QStaticTextH staticText)
{
	((QPainter *)handle)->drawStaticText(left, top, *(const QStaticText*)staticText);
}

void QPainter_drawText(QPainterH handle, const QPointFH p, PWideString s)
{
	QString t_s;
	copyPWideStringToQString(s, t_s);
	((QPainter *)handle)->drawText(*(const QPointF*)p, t_s);
}

void QPainter_drawText2(QPainterH handle, const QPointH p, PWideString s)
{
	QString t_s;
	copyPWideStringToQString(s, t_s);
	((QPainter *)handle)->drawText(*(const QPoint*)p, t_s);
}

void QPainter_drawText3(QPainterH handle, int x, int y, PWideString s)
{
	QString t_s;
	copyPWideStringToQString(s, t_s);
	((QPainter *)handle)->drawText(x, y, t_s);
}

void QPainter_drawText4(QPainterH handle, const QPointFH p, PWideString str, int tf, int justificationPadding)
{
	QString t_str;
	copyPWideStringToQString(str, t_str);
	((QPainter *)handle)->drawText(*(const QPointF*)p, t_str, tf, justificationPadding);
}

void QPainter_drawText5(QPainterH handle, const QRectFH r, int flags, PWideString text, QRectFH br)
{
	QString t_text;
	copyPWideStringToQString(text, t_text);
	((QPainter *)handle)->drawText(*(const QRectF*)r, flags, t_text, (QRectF*)br);
}

void QPainter_drawText6(QPainterH handle, PRect r, int flags, PWideString text, PRect br)
{
	QRect t_r;
	QString t_text;
	QRect t_br;
	copyPRectToQRect(r, t_r);
	copyPWideStringToQString(text, t_text);
	if ( br )
		copyPRectToQRect(br, t_br);
	((QPainter *)handle)->drawText(t_r, flags, t_text, br ? &t_br : NULL);
	if ( br )
		copyQRectToPRect(t_br, br);
}

void QPainter_drawText7(QPainterH handle, int x, int y, int w, int h, int flags, PWideString text, PRect br)
{
	QString t_text;
	QRect t_br;
	copyPWideStringToQString(text, t_text);
	if ( br )
		copyPRectToQRect(br, t_br);
	((QPainter *)handle)->drawText(x, y, w, h, flags, t_text, br ? &t_br : NULL);
	if ( br )
		copyQRectToPRect(t_br, br);
}

void QPainter_drawText8(QPainterH handle, const QRectFH r, PWideString text, const QTextOptionH o)
{
	QString t_text;
	copyPWideStringToQString(text, t_text);
	((QPainter *)handle)->drawText(*(const QRectF*)r, t_text, *(const QTextOption*)o);
}

void QPainter_boundingRect(QPainterH handle, QRectFH retval, const QRectFH rect, int flags, PWideString text)
{
	QString t_text;
	copyPWideStringToQString(text, t_text);
	*(QRectF *)retval = ((QPainter *)handle)->boundingRect(*(const QRectF*)rect, flags, t_text);
}

void QPainter_boundingRect2(QPainterH handle, PRect retval, PRect rect, int flags, PWideString text)
{
	QRect t_retval;
	QRect t_rect;
	QString t_text;
	copyPRectToQRect(rect, t_rect);
	copyPWideStringToQString(text, t_text);
	t_retval = ((QPainter *)handle)->boundingRect(t_rect, flags, t_text);
	copyQRectToPRect(t_retval, retval);
}

void QPainter_boundingRect3(QPainterH handle, PRect retval, int x, int y, int w, int h, int flags, PWideString text)
{
	QRect t_retval;
	QString t_text;
	copyPWideStringToQString(text, t_text);
	t_retval = ((QPainter *)handle)->boundingRect(x, y, w, h, flags, t_text);
	copyQRectToPRect(t_retval, retval);
}

void QPainter_boundingRect4(QPainterH handle, QRectFH retval, const QRectFH rect, PWideString text, const QTextOptionH o)
{
	QString t_text;
	copyPWideStringToQString(text, t_text);
	*(QRectF *)retval = ((QPainter *)handle)->boundingRect(*(const QRectF*)rect, t_text, *(const QTextOption*)o);
}

void QPainter_drawText9(QPainterH handle, const QPointFH p, const QStringH s)
{
	((QPainter *)handle)->drawText(*(const QPointF*)p, *(const QString *)s);
}

void QPainter_drawText10(QPainterH handle, const QPointH p, const QStringH s)
{
	((QPainter *)handle)->drawText(*(const QPoint*)p, *(const QString *)s);
}

void QPainter_drawText11(QPainterH handle, int x, int y, const QStringH s)
{
	((QPainter *)handle)->drawText(x, y, *(const QString *)s);
}

void QPainter_drawText12(QPainterH handle, const QPointFH p, const QStringH str, int tf, int justificationPadding)
{
	((QPainter *)handle)->drawText(*(const QPointF*)p, *(const QString*)str, tf, justificationPadding);
}

void QPainter_drawText13(QPainterH handle, const QRectFH r, int flags, const QStringH text, QRectFH br)
{
  ((QPainter *)handle)->drawText(*(const QRectF*)r, flags, *(const QString *)text, (QRectF*)br);
}

void QPainter_drawText14(QPainterH handle, PRect r, int flags, const QStringH text, PRect br)
{
	QRect t_r;
	QRect t_br;
	copyPRectToQRect(r, t_r);
	if ( br )
		copyPRectToQRect(br, t_br);
	((QPainter *)handle)->drawText(t_r, flags, *(const QString*)text, br ? &t_br : NULL);
	if ( br )
		copyQRectToPRect(t_br, br);
}

void QPainter_drawText15(QPainterH handle, int x, int y, int w, int h, int flags, const QStringH text, PRect br)
{
	QRect t_br;
	if ( br )
		copyPRectToQRect(br, t_br);
	((QPainter *)handle)->drawText(x, y, w, h, flags, *(const QString*)text, br ? &t_br : NULL);
	if ( br )
		copyQRectToPRect(t_br, br);
}

void QPainter_drawText16(QPainterH handle, const QRectFH r, PWideString text, const QTextOptionH o)
{
	((QPainter *)handle)->drawText(*(const QRectF*)r, *(const QString*)text, *(const QTextOption*)o);
}

void QPainter_boundingRect5(QPainterH handle, QRectFH retval, const QRectFH rect, int flags, const QStringH text)
{
	*(QRectF *)retval = ((QPainter *)handle)->boundingRect(*(const QRectF*)rect, flags, *(const QString*)text);
}

void QPainter_boundingRect6(QPainterH handle, PRect retval, PRect rect, int flags, const QStringH text)
{
	QRect t_retval;
	QRect t_rect;
	copyPRectToQRect(rect, t_rect);
	t_retval = ((QPainter *)handle)->boundingRect(t_rect, flags, *(const QString*)text);
	copyQRectToPRect(t_retval, retval);
}

void QPainter_boundingRect7(QPainterH handle, PRect retval, int x, int y, int w, int h, int flags, const QStringH text)
{
	QRect t_retval;
	t_retval = ((QPainter *)handle)->boundingRect(x, y, w, h, flags, *(const QString*)text);
	copyQRectToPRect(t_retval, retval);
}

void QPainter_boundingRect8(QPainterH handle, QRectFH retval, const QRectFH rect, const QStringH text, const QTextOptionH o)
{
	*(QRectF *)retval = ((QPainter *)handle)->boundingRect(*(const QRectF*)rect, *(const QString*)text, *(const QTextOption*)o);
}

void QPainter_fillRect(QPainterH handle, const QRectFH AnonParam1, const QBrushH AnonParam2)
{
	((QPainter *)handle)->fillRect(*(const QRectF*)AnonParam1, *(const QBrush*)AnonParam2);
}

void QPainter_fillRect2(QPainterH handle, int x, int y, int w, int h, const QBrushH AnonParam5)
{
	((QPainter *)handle)->fillRect(x, y, w, h, *(const QBrush*)AnonParam5);
}

void QPainter_fillRect3(QPainterH handle, PRect AnonParam1, const QBrushH AnonParam2)
{
	QRect t_AnonParam1;
	copyPRectToQRect(AnonParam1, t_AnonParam1);
	((QPainter *)handle)->fillRect(t_AnonParam1, *(const QBrush*)AnonParam2);
}

void QPainter_fillRect4(QPainterH handle, const QRectFH AnonParam1, const QColorH color)
{
	((QPainter *)handle)->fillRect(*(const QRectF*)AnonParam1, *(const QColor*)color);
}

void QPainter_fillRect5(QPainterH handle, int x, int y, int w, int h, const QColorH color)
{
	((QPainter *)handle)->fillRect(x, y, w, h, *(const QColor*)color);
}

void QPainter_fillRect6(QPainterH handle, PRect AnonParam1, const QColorH color)
{
	QRect t_AnonParam1;
	copyPRectToQRect(AnonParam1, t_AnonParam1);
	((QPainter *)handle)->fillRect(t_AnonParam1, *(const QColor*)color);
}

void QPainter_fillRect7(QPainterH handle, int x, int y, int w, int h, Qt::GlobalColor c)
{
	((QPainter *)handle)->fillRect(x, y, w, h, c);
}

void QPainter_fillRect8(QPainterH handle, PRect r, Qt::GlobalColor c)
{
	QRect t_r;
	copyPRectToQRect(r, t_r);
	((QPainter *)handle)->fillRect(t_r, c);
}

void QPainter_fillRect9(QPainterH handle, const QRectFH r, Qt::GlobalColor c)
{
	((QPainter *)handle)->fillRect(*(const QRectF*)r, c);
}

void QPainter_fillRect10(QPainterH handle, int x, int y, int w, int h, Qt::BrushStyle style)
{
	((QPainter *)handle)->fillRect(x, y, w, h, style);
}

void QPainter_fillRect11(QPainterH handle, PRect r, Qt::BrushStyle style)
{
	QRect t_r;
	copyPRectToQRect(r, t_r);
	((QPainter *)handle)->fillRect(t_r, style);
}

void QPainter_fillRect12(QPainterH handle, const QRectFH r, Qt::BrushStyle style)
{
	((QPainter *)handle)->fillRect(*(const QRectF*)r, style);
}

void QPainter_fillRect13(QPainterH handle, int x, int y, int w, int h, unsigned int gradient)
{
	((QPainter *)handle)->fillRect(x, y, w, h, (QGradient::Preset) gradient);
}

void QPainter_fillRect14(QPainterH handle, PRect r, unsigned int gradient)
{
	QRect t_r;
	copyPRectToQRect(r, t_r);
	((QPainter *)handle)->fillRect(t_r, (QGradient::Preset) gradient);
}

void QPainter_fillRect15(QPainterH handle, const QRectFH r, unsigned int gradient)
{
	((QPainter *)handle)->fillRect(*(const QRectF*)r, (QGradient::Preset) gradient);
}

void QPainter_eraseRect(QPainterH handle, const QRectFH AnonParam1)
{
	((QPainter *)handle)->eraseRect(*(const QRectF*)AnonParam1);
}

void QPainter_eraseRect2(QPainterH handle, int x, int y, int w, int h)
{
	((QPainter *)handle)->eraseRect(x, y, w, h);
}

void QPainter_eraseRect3(QPainterH handle, PRect AnonParam1)
{
	QRect t_AnonParam1;
	copyPRectToQRect(AnonParam1, t_AnonParam1);
	((QPainter *)handle)->eraseRect(t_AnonParam1);
}

void QPainter_setRenderHint(QPainterH handle, QPainter::RenderHint hint, bool on)
{
	((QPainter *)handle)->setRenderHint(hint, on);
}

void QPainter_setRenderHints(QPainterH handle, unsigned int hints, bool on)
{
	((QPainter *)handle)->setRenderHints((QPainter::RenderHints)hints, on);
}

unsigned int QPainter_renderHints(QPainterH handle)
{
	return (unsigned int) ((QPainter *)handle)->renderHints();
}

bool QPainter_testRenderHint(QPainterH handle, QPainter::RenderHint hint)
{
	return (bool) ((QPainter *)handle)->testRenderHint(hint);
}

QPaintEngineH QPainter_paintEngine(QPainterH handle)
{
	return (QPaintEngineH) ((QPainter *)handle)->paintEngine();
}

void QPainter_beginNativePainting(QPainterH handle)
{
	((QPainter *)handle)->beginNativePainting();
}

void QPainter_endNativePainting(QPainterH handle)
{
	((QPainter *)handle)->endNativePainting();
}

