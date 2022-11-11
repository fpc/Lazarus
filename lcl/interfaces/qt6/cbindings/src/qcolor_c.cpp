//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qcolor_c.h"

QColorH QColor_Create()
{
	return (QColorH) new QColor();
}

void QColor_Destroy(QColorH handle)
{
	delete (QColor *)handle;
}

QColorH QColor_Create2(Qt::GlobalColor color)
{
	return (QColorH) new QColor(color);
}

QColorH QColor_Create3(int r, int g, int b, int a)
{
	return (QColorH) new QColor(r, g, b, a);
}

QColorH QColor_Create4(QRgb rgb)
{
	return (QColorH) new QColor(rgb);
}

QColorH QColor_Create5(PWideString name)
{
	QString t_name;
	copyPWideStringToQString(name, t_name);
	return (QColorH) new QColor(t_name);
}

QColorH QColor_Create6(const char* name)
{
	return (QColorH) new QColor(name);
}

QColorH QColor_Create7(const QColorH color)
{
	return (QColorH) new QColor(*(const QColor*)color);
}

QColorH QColor_Create8(QColor::Spec spec)
{
	return (QColorH) new QColor(spec);
}

QColorH QColor_Create9(QRgba64 rgba64)
{
	return (QColorH) new QColor(rgba64);
}

bool QColor_isValid(QColorH handle)
{
	return (bool) ((QColor *)handle)->isValid();
}

void QColor_name(QColorH handle, PWideString retval, QColor::NameFormat format)
{
	QString t_retval;
	t_retval = ((QColor *)handle)->name(format);
	copyQStringToPWideString(t_retval, retval);
}

void QColor_setNamedColor(QColorH handle, PWideString name)
{
	QString t_name;
	copyPWideStringToQString(name, t_name);
	((QColor *)handle)->setNamedColor(t_name);
}

void QColor_colorNames(QStringListH retval)
{
	*(QStringList *)retval = QColor::colorNames();
}

QColor::Spec QColor_spec(QColorH handle)
{
	return (QColor::Spec) ((QColor *)handle)->spec();
}

int QColor_alpha(QColorH handle)
{
	return (int) ((QColor *)handle)->alpha();
}

void QColor_setAlpha(QColorH handle, int alpha)
{
	((QColor *)handle)->setAlpha(alpha);
}

qreal QColor_alphaF(QColorH handle)
{
	return (qreal) ((QColor *)handle)->alphaF();
}

void QColor_setAlphaF(QColorH handle, qreal alpha)
{
	((QColor *)handle)->setAlphaF(alpha);
}

int QColor_red(QColorH handle)
{
	return (int) ((QColor *)handle)->red();
}

int QColor_green(QColorH handle)
{
	return (int) ((QColor *)handle)->green();
}

int QColor_blue(QColorH handle)
{
	return (int) ((QColor *)handle)->blue();
}

void QColor_setRed(QColorH handle, int red)
{
	((QColor *)handle)->setRed(red);
}

void QColor_setGreen(QColorH handle, int green)
{
	((QColor *)handle)->setGreen(green);
}

void QColor_setBlue(QColorH handle, int blue)
{
	((QColor *)handle)->setBlue(blue);
}

qreal QColor_redF(QColorH handle)
{
	return (qreal) ((QColor *)handle)->redF();
}

qreal QColor_greenF(QColorH handle)
{
	return (qreal) ((QColor *)handle)->greenF();
}

qreal QColor_blueF(QColorH handle)
{
	return (qreal) ((QColor *)handle)->blueF();
}

void QColor_setRedF(QColorH handle, qreal red)
{
	((QColor *)handle)->setRedF(red);
}

void QColor_setGreenF(QColorH handle, qreal green)
{
	((QColor *)handle)->setGreenF(green);
}

void QColor_setBlueF(QColorH handle, qreal blue)
{
	((QColor *)handle)->setBlueF(blue);
}

void QColor_getRgb(QColorH handle, int* r, int* g, int* b, int* a)
{
	((QColor *)handle)->getRgb(r, g, b, a);
}

void QColor_setRgb(QColorH handle, int r, int g, int b, int a)
{
	((QColor *)handle)->setRgb(r, g, b, a);
}

void QColor_getRgbF(QColorH handle, qreal* r, qreal* g, qreal* b, qreal* a)
{
  ((QColor *)handle)->getRgbF((float*)r, (float*)g, (float*)b , (float*)a);
}

void QColor_setRgbF(QColorH handle, qreal r, qreal g, qreal b, qreal a)
{
	((QColor *)handle)->setRgbF(r, g, b, a);
}

QRgb QColor_rgba(QColorH handle)
{
	return (QRgb) ((QColor *)handle)->rgba();
}

void QColor_setRgba(QColorH handle, QRgb rgba)
{
	((QColor *)handle)->setRgba(rgba);
}

QRgb QColor_rgb(QColorH handle)
{
	return (QRgb) ((QColor *)handle)->rgb();
}

void QColor_setRgb2(QColorH handle, QRgb rgb)
{
	((QColor *)handle)->setRgb(rgb);
}

int QColor_hue(QColorH handle)
{
	return (int) ((QColor *)handle)->hue();
}

int QColor_saturation(QColorH handle)
{
	return (int) ((QColor *)handle)->saturation();
}

int QColor_hsvHue(QColorH handle)
{
	return (int) ((QColor *)handle)->hsvHue();
}

int QColor_hsvSaturation(QColorH handle)
{
	return (int) ((QColor *)handle)->hsvSaturation();
}

int QColor_value(QColorH handle)
{
	return (int) ((QColor *)handle)->value();
}

qreal QColor_hueF(QColorH handle)
{
	return (qreal) ((QColor *)handle)->hueF();
}

qreal QColor_saturationF(QColorH handle)
{
	return (qreal) ((QColor *)handle)->saturationF();
}

qreal QColor_hsvHueF(QColorH handle)
{
	return (qreal) ((QColor *)handle)->hsvHueF();
}

qreal QColor_hsvSaturationF(QColorH handle)
{
	return (qreal) ((QColor *)handle)->hsvSaturationF();
}

qreal QColor_valueF(QColorH handle)
{
	return (qreal) ((QColor *)handle)->valueF();
}

void QColor_getHsv(QColorH handle, int* h, int* s, int* v, int* a)
{
	((QColor *)handle)->getHsv(h, s, v, a);
}

void QColor_setHsv(QColorH handle, int h, int s, int v, int a)
{
	((QColor *)handle)->setHsv(h, s, v, a);
}

void QColor_getHsvF(QColorH handle, qreal* h, qreal* s, qreal* v, qreal* a)
{
	((QColor *)handle)->getHsvF((float*)h, (float*)s, (float*)v, (float*)a);
}

void QColor_setHsvF(QColorH handle, qreal h, qreal s, qreal v, qreal a)
{
	((QColor *)handle)->setHsvF(h, s, v, a);
}

int QColor_cyan(QColorH handle)
{
	return (int) ((QColor *)handle)->cyan();
}

int QColor_magenta(QColorH handle)
{
	return (int) ((QColor *)handle)->magenta();
}

int QColor_yellow(QColorH handle)
{
	return (int) ((QColor *)handle)->yellow();
}

int QColor_black(QColorH handle)
{
	return (int) ((QColor *)handle)->black();
}

qreal QColor_cyanF(QColorH handle)
{
	return (qreal) ((QColor *)handle)->cyanF();
}

qreal QColor_magentaF(QColorH handle)
{
	return (qreal) ((QColor *)handle)->magentaF();
}

qreal QColor_yellowF(QColorH handle)
{
	return (qreal) ((QColor *)handle)->yellowF();
}

qreal QColor_blackF(QColorH handle)
{
	return (qreal) ((QColor *)handle)->blackF();
}

void QColor_getCmyk(QColorH handle, int* c, int* m, int* y, int* k, int* a)
{
	((QColor *)handle)->getCmyk(c, m, y, k, a);
}

void QColor_setCmyk(QColorH handle, int c, int m, int y, int k, int a)
{
	((QColor *)handle)->setCmyk(c, m, y, k, a);
}

void QColor_getCmykF(QColorH handle, qreal* c, qreal* m, qreal* y, qreal* k, qreal* a)
{
	((QColor *)handle)->getCmykF((float*)c, (float*)m, (float*)y, (float*)k, (float*)a);
}

void QColor_setCmykF(QColorH handle, qreal c, qreal m, qreal y, qreal k, qreal a)
{
	((QColor *)handle)->setCmykF(c, m, y, k, a);
}

int QColor_hslHue(QColorH handle)
{
	return (int) ((QColor *)handle)->hslHue();
}

int QColor_hslSaturation(QColorH handle)
{
	return (int) ((QColor *)handle)->hslSaturation();
}

int QColor_lightness(QColorH handle)
{
	return (int) ((QColor *)handle)->lightness();
}

qreal QColor_hslHueF(QColorH handle)
{
	return (qreal) ((QColor *)handle)->hslHueF();
}

qreal QColor_hslSaturationF(QColorH handle)
{
	return (qreal) ((QColor *)handle)->hslSaturationF();
}

qreal QColor_lightnessF(QColorH handle)
{
	return (qreal) ((QColor *)handle)->lightnessF();
}

void QColor_getHsl(QColorH handle, int* h, int* s, int* l, int* a)
{
	((QColor *)handle)->getHsl(h, s, l, a);
}

void QColor_setHsl(QColorH handle, int h, int s, int l, int a)
{
	((QColor *)handle)->setHsl(h, s, l, a);
}

void QColor_getHslF(QColorH handle, qreal* h, qreal* s, qreal* l, qreal* a)
{
	((QColor *)handle)->getHslF((float*)h, (float*)s, (float*)l, (float*)a);
}

void QColor_setHslF(QColorH handle, qreal h, qreal s, qreal l, qreal a)
{
	((QColor *)handle)->setHslF(h, s, l, a);
}

void QColor_toRgb(QColorH handle, PQColor retval)
{
	*(QColor *)retval = ((QColor *)handle)->toRgb();
}

void QColor_toHsv(QColorH handle, PQColor retval)
{
	*(QColor *)retval = ((QColor *)handle)->toHsv();
}

void QColor_toCmyk(QColorH handle, PQColor retval)
{
	*(QColor *)retval = ((QColor *)handle)->toCmyk();
}

void QColor_toHsl(QColorH handle, PQColor retval)
{
	*(QColor *)retval = ((QColor *)handle)->toHsl();
}

void QColor_convertTo(QColorH handle, PQColor retval, QColor::Spec colorSpec)
{
	*(QColor *)retval = ((QColor *)handle)->convertTo(colorSpec);
}

void QColor_fromRgb(PQColor retval, QRgb rgb)
{
	*(QColor *)retval = QColor::fromRgb(rgb);
}

void QColor_fromRgba(PQColor retval, QRgb rgba)
{
	*(QColor *)retval = QColor::fromRgba(rgba);
}

void QColor_fromRgba64(PQColor retval, QRgba64 rgba64)
{
	*(QColor *)retval = QColor::fromRgba64(rgba64);
}

void QColor_fromRgba642(PQColor retval, ushort r, ushort g, ushort b, ushort a)
{
  *(QColor *)retval = QColor::fromRgba64(r, g, b, a);
}

void QColor_fromRgb2(PQColor retval, int r, int g, int b, int a)
{
	*(QColor *)retval = QColor::fromRgb(r, g, b, a);
}

void QColor_fromRgbF(PQColor retval, qreal r, qreal g, qreal b, qreal a)
{
	*(QColor *)retval = QColor::fromRgbF(r, g, b, a);
}

void QColor_fromHsv(PQColor retval, int h, int s, int v, int a)
{
	*(QColor *)retval = QColor::fromHsv(h, s, v, a);
}

void QColor_fromHsvF(PQColor retval, qreal h, qreal s, qreal v, qreal a)
{
	*(QColor *)retval = QColor::fromHsvF(h, s, v, a);
}

void QColor_fromCmyk(PQColor retval, int c, int m, int y, int k, int a)
{
	*(QColor *)retval = QColor::fromCmyk(c, m, y, k, a);
}

void QColor_fromCmykF(PQColor retval, qreal c, qreal m, qreal y, qreal k, qreal a)
{
	*(QColor *)retval = QColor::fromCmykF(c, m, y, k, a);
}

void QColor_fromHsl(PQColor retval, int h, int s, int l, int a)
{
	*(QColor *)retval = QColor::fromHsl(h, s, l, a);
}

void QColor_fromHslF(PQColor retval, qreal h, qreal s, qreal l, qreal a)
{
	*(QColor *)retval = QColor::fromHslF(h, s, l, a);
}

void QColor_lighter(QColorH handle, PQColor retval, int f)
{
	*(QColor *)retval = ((QColor *)handle)->lighter(f);
}

void QColor_darker(QColorH handle, PQColor retval, int f)
{
	*(QColor *)retval = ((QColor *)handle)->darker(f);
}

bool QColor_isValidColor(PWideString name)
{
	QString t_name;
	copyPWideStringToQString(name, t_name);
	return (bool) QColor::isValidColor(t_name);
}

QRgb QColor_Rgba2(int r, int g, int b, int a)
{
  return (QRgb) qRgba(r, g, b, a);
}

int QColor_alpha2(QRgb rgba)
{
  return (int) qAlpha(rgba);
}

int QColor_red2(QRgb rgba)
{
  return (int) qRed(rgba);
}

int QColor_green2(QRgb rgba)
{
  return (int) qGreen(rgba);
}

int QColor_blue2(QRgb rgba)
{
  return (int) qBlue(rgba);
}

QRgb QColor_premultiply(QRgb rgba)
{
  return (QRgb) qPremultiply(rgba);
}

QRgb QColor_unpremultiply(QRgb rgba)
{
  return (QRgb) qUnpremultiply(rgba);
}

