//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QTOOLTIP_C_H
#define QTOOLTIP_C_H

#include <QtWidgets>
#include "pascalbind.h"

C_EXPORT void QToolTip_showText(const QPointH pos, PWideString text, QWidgetH w);
C_EXPORT void QToolTip_showText2(const QPointH pos, PWideString text, QWidgetH w, PRect rect);
C_EXPORT void QToolTip_hideText();
C_EXPORT bool QToolTip_isVisible();
C_EXPORT void QToolTip_text(PWideString retval);
C_EXPORT void QToolTip_palette(QPaletteH retval);
C_EXPORT void QToolTip_setPalette(const QPaletteH AnonParam1);
C_EXPORT void QToolTip_font(QFontH retval);
C_EXPORT void QToolTip_setFont(const QFontH AnonParam1);

#endif
