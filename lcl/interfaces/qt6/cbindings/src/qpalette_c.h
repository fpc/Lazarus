//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QPALETTE_C_H
#define QPALETTE_C_H

#include <QtGui>
#include "pascalbind.h"

C_EXPORT QPaletteH QPalette_Create();
C_EXPORT void QPalette_Destroy(QPaletteH handle);
C_EXPORT QPaletteH QPalette_Create2(const QColorH button);
C_EXPORT QPaletteH QPalette_Create3(Qt::GlobalColor button);
C_EXPORT QPaletteH QPalette_Create4(const QColorH button, const QColorH window);
C_EXPORT QPaletteH QPalette_Create5(const QBrushH windowText, const QBrushH button, const QBrushH light, const QBrushH dark, const QBrushH mid, const QBrushH text, const QBrushH bright_text, const QBrushH base, const QBrushH window);
C_EXPORT QPaletteH QPalette_Create6(const QColorH windowText, const QColorH window, const QColorH light, const QColorH dark, const QColorH mid, const QColorH text, const QColorH base);
C_EXPORT QPaletteH QPalette_Create7(const QPaletteH palette);
C_EXPORT void QPalette_swap(QPaletteH handle, QPaletteH other);
C_EXPORT QPalette::ColorGroup QPalette_currentColorGroup(QPaletteH handle);
C_EXPORT void QPalette_setCurrentColorGroup(QPaletteH handle, QPalette::ColorGroup cg);
C_EXPORT const QColorH QPalette_color(QPaletteH handle, QPalette::ColorGroup cg, QPalette::ColorRole cr);
C_EXPORT const QBrushH QPalette_brush(QPaletteH handle, QPalette::ColorGroup cg, QPalette::ColorRole cr);
C_EXPORT void QPalette_setColor(QPaletteH handle, QPalette::ColorGroup cg, QPalette::ColorRole cr, const QColorH color);
C_EXPORT void QPalette_setColor2(QPaletteH handle, QPalette::ColorRole cr, const QColorH color);
C_EXPORT void QPalette_setBrush(QPaletteH handle, QPalette::ColorRole cr, const QBrushH brush);
C_EXPORT bool QPalette_isBrushSet(QPaletteH handle, QPalette::ColorGroup cg, QPalette::ColorRole cr);
C_EXPORT void QPalette_setBrush2(QPaletteH handle, QPalette::ColorGroup cg, QPalette::ColorRole cr, const QBrushH brush);
C_EXPORT void QPalette_setColorGroup(QPaletteH handle, QPalette::ColorGroup cr, const QBrushH windowText, const QBrushH button, const QBrushH light, const QBrushH dark, const QBrushH mid, const QBrushH text, const QBrushH bright_text, const QBrushH base, const QBrushH window);
C_EXPORT bool QPalette_isEqual(QPaletteH handle, QPalette::ColorGroup cr1, QPalette::ColorGroup cr2);
C_EXPORT const QColorH QPalette_color2(QPaletteH handle, QPalette::ColorRole cr);
C_EXPORT const QBrushH QPalette_brush2(QPaletteH handle, QPalette::ColorRole cr);
C_EXPORT const QBrushH QPalette_windowText(QPaletteH handle);
C_EXPORT const QBrushH QPalette_button(QPaletteH handle);
C_EXPORT const QBrushH QPalette_light(QPaletteH handle);
C_EXPORT const QBrushH QPalette_dark(QPaletteH handle);
C_EXPORT const QBrushH QPalette_mid(QPaletteH handle);
C_EXPORT const QBrushH QPalette_text(QPaletteH handle);
C_EXPORT const QBrushH QPalette_base(QPaletteH handle);
C_EXPORT const QBrushH QPalette_alternateBase(QPaletteH handle);
C_EXPORT const QBrushH QPalette_toolTipBase(QPaletteH handle);
C_EXPORT const QBrushH QPalette_toolTipText(QPaletteH handle);
C_EXPORT const QBrushH QPalette_window(QPaletteH handle);
C_EXPORT const QBrushH QPalette_midlight(QPaletteH handle);
C_EXPORT const QBrushH QPalette_brightText(QPaletteH handle);
C_EXPORT const QBrushH QPalette_buttonText(QPaletteH handle);
C_EXPORT const QBrushH QPalette_shadow(QPaletteH handle);
C_EXPORT const QBrushH QPalette_highlight(QPaletteH handle);
C_EXPORT const QBrushH QPalette_highlightedText(QPaletteH handle);
C_EXPORT const QBrushH QPalette_link(QPaletteH handle);
C_EXPORT const QBrushH QPalette_linkVisited(QPaletteH handle);
C_EXPORT bool QPalette_isCopyOf(QPaletteH handle, const QPaletteH p);
C_EXPORT qint64 QPalette_cacheKey(QPaletteH handle);
C_EXPORT void QPalette_resolve(QPaletteH handle, QPaletteH retval, const QPaletteH AnonParam1);

#endif
