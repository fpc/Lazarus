//******************************************************************************
//  Copyright (c) 2005-2024 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QSTYLE_C_H
#define QSTYLE_C_H

#include <QtWidgets>
#include "pascalbind.h"

C_EXPORT void QStyle_polish(QStyleH handle, QWidgetH AnonParam1);
C_EXPORT void QStyle_unpolish(QStyleH handle, QWidgetH AnonParam1);
C_EXPORT void QStyle_polish2(QStyleH handle, QApplicationH AnonParam1);
C_EXPORT void QStyle_unpolish2(QStyleH handle, QApplicationH AnonParam1);
C_EXPORT void QStyle_polish3(QStyleH handle, QPaletteH AnonParam1);
C_EXPORT void QStyle_itemTextRect(QStyleH handle, PRect retval, const QFontMetricsH fm, PRect r, int flags, bool enabled, PWideString text);
C_EXPORT void QStyle_itemPixmapRect(QStyleH handle, PRect retval, PRect r, int flags, const QPixmapH pixmap);
C_EXPORT void QStyle_drawItemText(QStyleH handle, QPainterH painter, PRect rect, int flags, const QPaletteH pal, bool enabled, PWideString text, QPalette::ColorRole textRole);
C_EXPORT void QStyle_drawItemPixmap(QStyleH handle, QPainterH painter, PRect rect, int alignment, const QPixmapH pixmap);
C_EXPORT void QStyle_standardPalette(QStyleH handle, QPaletteH retval);
C_EXPORT void QStyle_drawPrimitive(QStyleH handle, QStyle::PrimitiveElement pe, const QStyleOptionH opt, QPainterH p, const QWidgetH w);
C_EXPORT void QStyle_drawControl(QStyleH handle, QStyle::ControlElement element, const QStyleOptionH opt, QPainterH p, const QWidgetH w);
C_EXPORT void QStyle_subElementRect(QStyleH handle, PRect retval, QStyle::SubElement subElement, const QStyleOptionH option, const QWidgetH widget);
C_EXPORT void QStyle_drawComplexControl(QStyleH handle, QStyle::ComplexControl cc, const QStyleOptionComplexH opt, QPainterH p, const QWidgetH widget);
C_EXPORT QStyle::SubControl QStyle_hitTestComplexControl(QStyleH handle, QStyle::ComplexControl cc, const QStyleOptionComplexH opt, const QPointH pt, const QWidgetH widget);
C_EXPORT void QStyle_subControlRect(QStyleH handle, PRect retval, QStyle::ComplexControl cc, const QStyleOptionComplexH opt, QStyle::SubControl sc, const QWidgetH widget);
C_EXPORT int QStyle_pixelMetric(QStyleH handle, QStyle::PixelMetric metric, const QStyleOptionH option, const QWidgetH widget);
C_EXPORT void QStyle_sizeFromContents(QStyleH handle, PSize retval, QStyle::ContentsType ct, const QStyleOptionH opt, const QSizeH contentsSize, const QWidgetH w);
C_EXPORT int QStyle_styleHint(QStyleH handle, QStyle::StyleHint stylehint, const QStyleOptionH opt, const QWidgetH widget, QStyleHintReturnH returnData);
C_EXPORT void QStyle_standardPixmap(QStyleH handle, QPixmapH retval, QStyle::StandardPixmap standardPixmap, const QStyleOptionH opt, const QWidgetH widget);
C_EXPORT void QStyle_standardIcon(QStyleH handle, QIconH retval, QStyle::StandardPixmap standardIcon, const QStyleOptionH option, const QWidgetH widget);
C_EXPORT void QStyle_generatedIconPixmap(QStyleH handle, QPixmapH retval, QIcon::Mode iconMode, const QPixmapH pixmap, const QStyleOptionH opt);
C_EXPORT void QStyle_visualRect(PRect retval, Qt::LayoutDirection direction, PRect boundingRect, PRect logicalRect);
C_EXPORT void QStyle_visualPos(PQtPoint retval, Qt::LayoutDirection direction, PRect boundingRect, const QPointH logicalPos);
C_EXPORT int QStyle_sliderPositionFromValue(int min, int max, int val, int space, bool upsideDown);
C_EXPORT int QStyle_sliderValueFromPosition(int min, int max, int pos, int space, bool upsideDown);
C_EXPORT unsigned int QStyle_visualAlignment(Qt::LayoutDirection direction, unsigned int alignment);
C_EXPORT void QStyle_alignedRect(PRect retval, Qt::LayoutDirection direction, unsigned int alignment, const QSizeH size, PRect rectangle);
C_EXPORT int QStyle_layoutSpacing(QStyleH handle, QSizePolicy::ControlType control1, QSizePolicy::ControlType control2, Qt::Orientation orientation, const QStyleOptionH option, const QWidgetH widget);
C_EXPORT int QStyle_combinedLayoutSpacing(QStyleH handle, unsigned int controls1, unsigned int controls2, Qt::Orientation orientation, QStyleOptionH option, QWidgetH widget);
C_EXPORT const QStyleH QStyle_proxy(QStyleH handle);
C_EXPORT void QStyle_name(QStyleH handle, PWideString retval);

#endif
