//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qpalette_c.h"

QPaletteH QPalette_Create()
{
	return (QPaletteH) new QPalette();
}

void QPalette_Destroy(QPaletteH handle)
{
	delete (QPalette *)handle;
}

QPaletteH QPalette_Create2(const QColorH button)
{
	return (QPaletteH) new QPalette(*(const QColor*)button);
}

QPaletteH QPalette_Create3(Qt::GlobalColor button)
{
	return (QPaletteH) new QPalette(button);
}

QPaletteH QPalette_Create4(const QColorH button, const QColorH window)
{
	return (QPaletteH) new QPalette(*(const QColor*)button, *(const QColor*)window);
}

QPaletteH QPalette_Create5(const QBrushH windowText, const QBrushH button, const QBrushH light, const QBrushH dark, const QBrushH mid, const QBrushH text, const QBrushH bright_text, const QBrushH base, const QBrushH window)
{
	return (QPaletteH) new QPalette(*(const QBrush*)windowText, *(const QBrush*)button, *(const QBrush*)light, *(const QBrush*)dark, *(const QBrush*)mid, *(const QBrush*)text, *(const QBrush*)bright_text, *(const QBrush*)base, *(const QBrush*)window);
}

QPaletteH QPalette_Create6(const QColorH windowText, const QColorH window, const QColorH light, const QColorH dark, const QColorH mid, const QColorH text, const QColorH base)
{
	return (QPaletteH) new QPalette(*(const QColor*)windowText, *(const QColor*)window, *(const QColor*)light, *(const QColor*)dark, *(const QColor*)mid, *(const QColor*)text, *(const QColor*)base);
}

QPaletteH QPalette_Create7(const QPaletteH palette)
{
	return (QPaletteH) new QPalette(*(const QPalette*)palette);
}

void QPalette_swap(QPaletteH handle, QPaletteH other)
{
	((QPalette *)handle)->swap(*(QPalette*)other);
}

QPalette::ColorGroup QPalette_currentColorGroup(QPaletteH handle)
{
	return (QPalette::ColorGroup) ((QPalette *)handle)->currentColorGroup();
}

void QPalette_setCurrentColorGroup(QPaletteH handle, QPalette::ColorGroup cg)
{
	((QPalette *)handle)->setCurrentColorGroup(cg);
}

const QColorH QPalette_color(QPaletteH handle, QPalette::ColorGroup cg, QPalette::ColorRole cr)
{
	return (const QColorH) &((QPalette *)handle)->color(cg, cr);
}

const QBrushH QPalette_brush(QPaletteH handle, QPalette::ColorGroup cg, QPalette::ColorRole cr)
{
	return (const QBrushH) &((QPalette *)handle)->brush(cg, cr);
}

void QPalette_setColor(QPaletteH handle, QPalette::ColorGroup cg, QPalette::ColorRole cr, const QColorH color)
{
	((QPalette *)handle)->setColor(cg, cr, *(const QColor*)color);
}

void QPalette_setColor2(QPaletteH handle, QPalette::ColorRole cr, const QColorH color)
{
	((QPalette *)handle)->setColor(cr, *(const QColor*)color);
}

void QPalette_setBrush(QPaletteH handle, QPalette::ColorRole cr, const QBrushH brush)
{
	((QPalette *)handle)->setBrush(cr, *(const QBrush*)brush);
}

bool QPalette_isBrushSet(QPaletteH handle, QPalette::ColorGroup cg, QPalette::ColorRole cr)
{
	return (bool) ((QPalette *)handle)->isBrushSet(cg, cr);
}

void QPalette_setBrush2(QPaletteH handle, QPalette::ColorGroup cg, QPalette::ColorRole cr, const QBrushH brush)
{
	((QPalette *)handle)->setBrush(cg, cr, *(const QBrush*)brush);
}

void QPalette_setColorGroup(QPaletteH handle, QPalette::ColorGroup cr, const QBrushH windowText, const QBrushH button, const QBrushH light, const QBrushH dark, const QBrushH mid, const QBrushH text, const QBrushH bright_text, const QBrushH base, const QBrushH window)
{
	((QPalette *)handle)->setColorGroup(cr, *(const QBrush*)windowText, *(const QBrush*)button, *(const QBrush*)light, *(const QBrush*)dark, *(const QBrush*)mid, *(const QBrush*)text, *(const QBrush*)bright_text, *(const QBrush*)base, *(const QBrush*)window);
}

bool QPalette_isEqual(QPaletteH handle, QPalette::ColorGroup cr1, QPalette::ColorGroup cr2)
{
	return (bool) ((QPalette *)handle)->isEqual(cr1, cr2);
}

const QColorH QPalette_color2(QPaletteH handle, QPalette::ColorRole cr)
{
	return (const QColorH) &((QPalette *)handle)->color(cr);
}

const QBrushH QPalette_brush2(QPaletteH handle, QPalette::ColorRole cr)
{
	return (const QBrushH) &((QPalette *)handle)->brush(cr);
}

const QBrushH QPalette_windowText(QPaletteH handle)
{
	return (const QBrushH) &((QPalette *)handle)->windowText();
}

const QBrushH QPalette_button(QPaletteH handle)
{
	return (const QBrushH) &((QPalette *)handle)->button();
}

const QBrushH QPalette_light(QPaletteH handle)
{
	return (const QBrushH) &((QPalette *)handle)->light();
}

const QBrushH QPalette_dark(QPaletteH handle)
{
	return (const QBrushH) &((QPalette *)handle)->dark();
}

const QBrushH QPalette_mid(QPaletteH handle)
{
	return (const QBrushH) &((QPalette *)handle)->mid();
}

const QBrushH QPalette_text(QPaletteH handle)
{
	return (const QBrushH) &((QPalette *)handle)->text();
}

const QBrushH QPalette_base(QPaletteH handle)
{
	return (const QBrushH) &((QPalette *)handle)->base();
}

const QBrushH QPalette_alternateBase(QPaletteH handle)
{
	return (const QBrushH) &((QPalette *)handle)->alternateBase();
}

const QBrushH QPalette_toolTipBase(QPaletteH handle)
{
	return (const QBrushH) &((QPalette *)handle)->toolTipBase();
}

const QBrushH QPalette_toolTipText(QPaletteH handle)
{
	return (const QBrushH) &((QPalette *)handle)->toolTipText();
}

const QBrushH QPalette_window(QPaletteH handle)
{
	return (const QBrushH) &((QPalette *)handle)->window();
}

const QBrushH QPalette_midlight(QPaletteH handle)
{
	return (const QBrushH) &((QPalette *)handle)->midlight();
}

const QBrushH QPalette_brightText(QPaletteH handle)
{
	return (const QBrushH) &((QPalette *)handle)->brightText();
}

const QBrushH QPalette_buttonText(QPaletteH handle)
{
	return (const QBrushH) &((QPalette *)handle)->buttonText();
}

const QBrushH QPalette_shadow(QPaletteH handle)
{
	return (const QBrushH) &((QPalette *)handle)->shadow();
}

const QBrushH QPalette_highlight(QPaletteH handle)
{
	return (const QBrushH) &((QPalette *)handle)->highlight();
}

const QBrushH QPalette_highlightedText(QPaletteH handle)
{
	return (const QBrushH) &((QPalette *)handle)->highlightedText();
}

const QBrushH QPalette_link(QPaletteH handle)
{
	return (const QBrushH) &((QPalette *)handle)->link();
}

const QBrushH QPalette_linkVisited(QPaletteH handle)
{
	return (const QBrushH) &((QPalette *)handle)->linkVisited();
}

bool QPalette_isCopyOf(QPaletteH handle, const QPaletteH p)
{
	return (bool) ((QPalette *)handle)->isCopyOf(*(const QPalette*)p);
}

qint64 QPalette_cacheKey(QPaletteH handle)
{
	return (qint64) ((QPalette *)handle)->cacheKey();
}

void QPalette_resolve(QPaletteH handle, QPaletteH retval, const QPaletteH AnonParam1)
{
	*(QPalette *)retval = ((QPalette *)handle)->resolve(*(const QPalette*)AnonParam1);
}

