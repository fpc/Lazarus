//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qtooltip_c.h"

void QToolTip_showText(const QPointH pos, PWideString text, QWidgetH w)
{
	QString t_text;
	copyPWideStringToQString(text, t_text);
	QToolTip::showText(*(const QPoint*)pos, t_text, (QWidget*)w);
}

void QToolTip_showText2(const QPointH pos, PWideString text, QWidgetH w, PRect rect)
{
	QString t_text;
	QRect t_rect;
	copyPWideStringToQString(text, t_text);
	copyPRectToQRect(rect, t_rect);
	QToolTip::showText(*(const QPoint*)pos, t_text, (QWidget*)w, t_rect);
}

void QToolTip_hideText()
{
	QToolTip::hideText();
}

bool QToolTip_isVisible()
{
	return (bool) QToolTip::isVisible();
}

void QToolTip_text(PWideString retval)
{
	QString t_retval;
	t_retval = QToolTip::text();
	copyQStringToPWideString(t_retval, retval);
}

void QToolTip_palette(QPaletteH retval)
{
	*(QPalette *)retval = QToolTip::palette();
}

void QToolTip_setPalette(const QPaletteH AnonParam1)
{
	QToolTip::setPalette(*(const QPalette*)AnonParam1);
}

void QToolTip_font(QFontH retval)
{
	*(QFont *)retval = QToolTip::font();
}

void QToolTip_setFont(const QFontH AnonParam1)
{
	QToolTip::setFont(*(const QFont*)AnonParam1);
}

