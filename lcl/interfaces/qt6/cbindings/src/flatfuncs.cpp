//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************




#include "flatfuncs.h"
#include <QtGlobal>

//=======================================================
// Flat function from qglobal.h
//=======================================================
const char *QtVersion() {
  return qVersion();
}


//=======================================================
//  Drawing Utility Functions
//=======================================================

void q_DrawShadeRect(QPainterH p, int x, int y, int w, int h, const QPaletteH pal, bool sunken, int lineWidth, int midLineWidth, const QBrushH fill) {
  qDrawShadeRect((QPainter*)p, x, y, w, h, *(const QPalette*)pal, sunken, lineWidth, midLineWidth, (const QBrush*)fill);
  }

void q_DrawShadeRect2(QPainterH p, PRect r, const QPaletteH pal, bool sunken, int lineWidth, int midLineWidth, const QBrushH fill) {
  QRect t_r;
  copyPRectToQRect(r, t_r);
  qDrawShadeRect((QPainter*)p, t_r, *(const QPalette*)pal, sunken, lineWidth, midLineWidth, (const QBrush*)fill);
  }

void q_DrawPlainRect(QPainterH p, int x, int y, int w, int h, const QColorH p6, int lineWidth, const QBrushH fill) {
  qDrawPlainRect((QPainter*)p, x, y, w, h, *(const QColor*)p6, lineWidth, (const QBrush*)fill);
  }
  
  
void q_DrawPlainRect2(QPainterH p, PRect r, const QColorH p3, int lineWidth, const QBrushH fill) {
  QRect t_r;
  copyPRectToQRect(r, t_r);
  qDrawPlainRect((QPainter*)p, t_r, *(const QColor*)p3, lineWidth, (const QBrush*)fill);
  }
  

void q_DrawWinPanel(QPainterH p, int x, int y, int w, int h, const QPaletteH pal, bool sunken, const QBrushH fill) {
  qDrawWinPanel((QPainter*)p, x, y, w, h, *(const QPalette*)pal, sunken, (const QBrush*)fill);
  }

void q_DrawWinPanel2(QPainterH p, PRect r, const QPaletteH pal, bool sunken, const QBrushH fill) {
  QRect t_r;
  copyPRectToQRect(r, t_r);
  qDrawWinPanel((QPainter*)p, t_r, *(const QPalette*)pal, sunken, (const QBrush*)fill);
  }

void q_DrawShadeLine(QPainterH p, int x1, int y1, int x2, int y2, const QPaletteH palette,bool sunken, int lineWidth, int midLineWidth) {
  qDrawShadeLine((QPainter*)p, x1, y1, x2, y2, *(const QPalette*) palette, sunken, lineWidth, midLineWidth);
  }

void q_DrawShadePanel(QPainterH p, int x, int y, int w, int h, const QPaletteH palette,bool sunken, int lineWidth, const QBrushH fill) {
  qDrawShadePanel((QPainter*)p, x, y, w, h, *(const QPalette*) palette, sunken, lineWidth, (const QBrush*) fill);
  }

void q_DrawShadePanel2(QPainterH p, PRect r, const QPaletteH palette,bool sunken, int lineWidth, const QBrushH fill) {
  QRect t_r;
  copyPRectToQRect(r, t_r);
  qDrawShadePanel((QPainter*)p, t_r, *(const QPalette*) palette, sunken, lineWidth, (const QBrush*) fill);
  }

