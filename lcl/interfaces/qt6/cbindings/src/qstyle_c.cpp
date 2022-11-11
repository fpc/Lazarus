//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qstyle_c.h"

void QStyle_polish(QStyleH handle, QWidgetH AnonParam1)
{
	((QStyle *)handle)->polish((QWidget*)AnonParam1);
}

void QStyle_unpolish(QStyleH handle, QWidgetH AnonParam1)
{
	((QStyle *)handle)->unpolish((QWidget*)AnonParam1);
}

void QStyle_polish2(QStyleH handle, QApplicationH AnonParam1)
{
	((QStyle *)handle)->polish((QApplication*)AnonParam1);
}

void QStyle_unpolish2(QStyleH handle, QApplicationH AnonParam1)
{
	((QStyle *)handle)->unpolish((QApplication*)AnonParam1);
}

void QStyle_polish3(QStyleH handle, QPaletteH AnonParam1)
{
	((QStyle *)handle)->polish(*(QPalette*)AnonParam1);
}

void QStyle_itemTextRect(QStyleH handle, PRect retval, const QFontMetricsH fm, PRect r, int flags, bool enabled, PWideString text)
{
	QRect t_retval;
	QRect t_r;
	QString t_text;
	copyPRectToQRect(r, t_r);
	copyPWideStringToQString(text, t_text);
	t_retval = ((QStyle *)handle)->itemTextRect(*(const QFontMetrics*)fm, t_r, flags, enabled, t_text);
	copyQRectToPRect(t_retval, retval);
}

void QStyle_itemPixmapRect(QStyleH handle, PRect retval, PRect r, int flags, const QPixmapH pixmap)
{
	QRect t_retval;
	QRect t_r;
	copyPRectToQRect(r, t_r);
	t_retval = ((QStyle *)handle)->itemPixmapRect(t_r, flags, *(const QPixmap*)pixmap);
	copyQRectToPRect(t_retval, retval);
}

void QStyle_drawItemText(QStyleH handle, QPainterH painter, PRect rect, int flags, const QPaletteH pal, bool enabled, PWideString text, QPalette::ColorRole textRole)
{
	QRect t_rect;
	QString t_text;
	copyPRectToQRect(rect, t_rect);
	copyPWideStringToQString(text, t_text);
	((QStyle *)handle)->drawItemText((QPainter*)painter, t_rect, flags, *(const QPalette*)pal, enabled, t_text, textRole);
}

void QStyle_drawItemPixmap(QStyleH handle, QPainterH painter, PRect rect, int alignment, const QPixmapH pixmap)
{
	QRect t_rect;
	copyPRectToQRect(rect, t_rect);
	((QStyle *)handle)->drawItemPixmap((QPainter*)painter, t_rect, alignment, *(const QPixmap*)pixmap);
}

void QStyle_standardPalette(QStyleH handle, QPaletteH retval)
{
	*(QPalette *)retval = ((QStyle *)handle)->standardPalette();
}

void QStyle_drawPrimitive(QStyleH handle, QStyle::PrimitiveElement pe, const QStyleOptionH opt, QPainterH p, const QWidgetH w)
{
	((QStyle *)handle)->drawPrimitive(pe, (const QStyleOption*)opt, (QPainter*)p, (const QWidget*)w);
}

void QStyle_drawControl(QStyleH handle, QStyle::ControlElement element, const QStyleOptionH opt, QPainterH p, const QWidgetH w)
{
	((QStyle *)handle)->drawControl(element, (const QStyleOption*)opt, (QPainter*)p, (const QWidget*)w);
}

void QStyle_subElementRect(QStyleH handle, PRect retval, QStyle::SubElement subElement, const QStyleOptionH option, const QWidgetH widget)
{
	QRect t_retval;
	t_retval = ((QStyle *)handle)->subElementRect(subElement, (const QStyleOption*)option, (const QWidget*)widget);
	copyQRectToPRect(t_retval, retval);
}

void QStyle_drawComplexControl(QStyleH handle, QStyle::ComplexControl cc, const QStyleOptionComplexH opt, QPainterH p, const QWidgetH widget)
{
	((QStyle *)handle)->drawComplexControl(cc, (const QStyleOptionComplex*)opt, (QPainter*)p, (const QWidget*)widget);
}

QStyle::SubControl QStyle_hitTestComplexControl(QStyleH handle, QStyle::ComplexControl cc, const QStyleOptionComplexH opt, const QPointH pt, const QWidgetH widget)
{
	return (QStyle::SubControl) ((QStyle *)handle)->hitTestComplexControl(cc, (const QStyleOptionComplex*)opt, *(const QPoint*)pt, (const QWidget*)widget);
}

void QStyle_subControlRect(QStyleH handle, PRect retval, QStyle::ComplexControl cc, const QStyleOptionComplexH opt, QStyle::SubControl sc, const QWidgetH widget)
{
	QRect t_retval;
	t_retval = ((QStyle *)handle)->subControlRect(cc, (const QStyleOptionComplex*)opt, sc, (const QWidget*)widget);
	copyQRectToPRect(t_retval, retval);
}

int QStyle_pixelMetric(QStyleH handle, QStyle::PixelMetric metric, const QStyleOptionH option, const QWidgetH widget)
{
	return (int) ((QStyle *)handle)->pixelMetric(metric, (const QStyleOption*)option, (const QWidget*)widget);
}

void QStyle_sizeFromContents(QStyleH handle, PSize retval, QStyle::ContentsType ct, const QStyleOptionH opt, const QSizeH contentsSize, const QWidgetH w)
{
	*(QSize *)retval = ((QStyle *)handle)->sizeFromContents(ct, (const QStyleOption*)opt, *(const QSize*)contentsSize, (const QWidget*)w);
}

int QStyle_styleHint(QStyleH handle, QStyle::StyleHint stylehint, const QStyleOptionH opt, const QWidgetH widget, QStyleHintReturnH returnData)
{
	return (int) ((QStyle *)handle)->styleHint(stylehint, (const QStyleOption*)opt, (const QWidget*)widget, (QStyleHintReturn*)returnData);
}

void QStyle_standardPixmap(QStyleH handle, QPixmapH retval, QStyle::StandardPixmap standardPixmap, const QStyleOptionH opt, const QWidgetH widget)
{
	*(QPixmap *)retval = ((QStyle *)handle)->standardPixmap(standardPixmap, (const QStyleOption*)opt, (const QWidget*)widget);
}

void QStyle_standardIcon(QStyleH handle, QIconH retval, QStyle::StandardPixmap standardIcon, const QStyleOptionH option, const QWidgetH widget)
{
	*(QIcon *)retval = ((QStyle *)handle)->standardIcon(standardIcon, (const QStyleOption*)option, (const QWidget*)widget);
}

void QStyle_generatedIconPixmap(QStyleH handle, QPixmapH retval, QIcon::Mode iconMode, const QPixmapH pixmap, const QStyleOptionH opt)
{
	*(QPixmap *)retval = ((QStyle *)handle)->generatedIconPixmap(iconMode, *(const QPixmap*)pixmap, (const QStyleOption*)opt);
}

void QStyle_visualRect(PRect retval, Qt::LayoutDirection direction, PRect boundingRect, PRect logicalRect)
{
	QRect t_retval;
	QRect t_boundingRect;
	QRect t_logicalRect;
	copyPRectToQRect(boundingRect, t_boundingRect);
	copyPRectToQRect(logicalRect, t_logicalRect);
	t_retval = QStyle::visualRect(direction, t_boundingRect, t_logicalRect);
	copyQRectToPRect(t_retval, retval);
}

void QStyle_visualPos(PQtPoint retval, Qt::LayoutDirection direction, PRect boundingRect, const QPointH logicalPos)
{
	QRect t_boundingRect;
	copyPRectToQRect(boundingRect, t_boundingRect);
	*(QPoint *)retval = QStyle::visualPos(direction, t_boundingRect, *(const QPoint*)logicalPos);
}

int QStyle_sliderPositionFromValue(int min, int max, int val, int space, bool upsideDown)
{
	return (int) QStyle::sliderPositionFromValue(min, max, val, space, upsideDown);
}

int QStyle_sliderValueFromPosition(int min, int max, int pos, int space, bool upsideDown)
{
	return (int) QStyle::sliderValueFromPosition(min, max, pos, space, upsideDown);
}

unsigned int QStyle_visualAlignment(Qt::LayoutDirection direction, unsigned int alignment)
{
	return (unsigned int) QStyle::visualAlignment(direction, (Qt::Alignment)alignment);
}

void QStyle_alignedRect(PRect retval, Qt::LayoutDirection direction, unsigned int alignment, const QSizeH size, PRect rectangle)
{
	QRect t_retval;
	QRect t_rectangle;
	copyPRectToQRect(rectangle, t_rectangle);
	t_retval = QStyle::alignedRect(direction, (Qt::Alignment)alignment, *(const QSize*)size, t_rectangle);
	copyQRectToPRect(t_retval, retval);
}

int QStyle_layoutSpacing(QStyleH handle, QSizePolicy::ControlType control1, QSizePolicy::ControlType control2, Qt::Orientation orientation, const QStyleOptionH option, const QWidgetH widget)
{
	return (int) ((QStyle *)handle)->layoutSpacing(control1, control2, orientation, (const QStyleOption*)option, (const QWidget*)widget);
}

int QStyle_combinedLayoutSpacing(QStyleH handle, unsigned int controls1, unsigned int controls2, Qt::Orientation orientation, QStyleOptionH option, QWidgetH widget)
{
	return (int) ((QStyle *)handle)->combinedLayoutSpacing((QSizePolicy::ControlTypes)controls1, (QSizePolicy::ControlTypes)controls2, orientation, (QStyleOption*)option, (QWidget*)widget);
}

const QStyleH QStyle_proxy(QStyleH handle)
{
	return (const QStyleH) ((QStyle *)handle)->proxy();
}

