//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************




#include <QtGui>

#include "pascalbind.h"


//=======================================================
// Flat function from qglobal.h
//=======================================================
C_EXPORT const char *QtVersion();


//=======================================================
//  Drawing Utility Functions
//=======================================================

#include <qdrawutil.h>

C_EXPORT void q_DrawShadeRect(QPainterH p, int x, int y, int w, int h, const QPaletteH pal, bool sunken, int lineWidth, int midLineWidth, const QBrushH fill);
C_EXPORT void q_DrawShadeRect2(QPainterH p, PRect r, const QPaletteH pal, bool sunken, int lineWidth, int midLineWidth, const QBrushH fill);
C_EXPORT void q_DrawPlainRect(QPainterH p, int x, int y, int w, int h, const QColorH p6, int lineWidth, const QBrushH fill);
C_EXPORT void q_DrawPlainRect2(QPainterH p, PRect r, const QColorH p3, int lineWidth, const QBrushH fill);
C_EXPORT void q_DrawWinPanel(QPainterH p, int x, int y, int w, int h, const QPaletteH pal, bool sunken, const QBrushH fill);
C_EXPORT void q_DrawWinPanel2(QPainterH p, PRect r, const QPaletteH pal, bool sunken, const QBrushH fill);
C_EXPORT void q_DrawShadeLine(QPainterH p, int x1, int y1, int x2, int y2, const QPaletteH palette,bool sunken, int lineWidth, int midLineWidth);
C_EXPORT void q_DrawShadePanel(QPainterH p, int x, int y, int w, int h, const QPaletteH palette,bool sunken, int lineWidth, const QBrushH fill);
C_EXPORT void q_DrawShadePanel2(QPainterH p, PRect r, const QPaletteH palette,bool sunken, int lineWidth, const QBrushH fill);

