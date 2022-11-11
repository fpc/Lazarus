//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QCOLOR_C_H
#define QCOLOR_C_H

#include <QtGui>
#include "pascalbind.h"

C_EXPORT QColorH QColor_Create();
C_EXPORT void QColor_Destroy(QColorH handle);
C_EXPORT QColorH QColor_Create2(Qt::GlobalColor color);
C_EXPORT QColorH QColor_Create3(int r, int g, int b, int a);
C_EXPORT QColorH QColor_Create4(QRgb rgb);
C_EXPORT QColorH QColor_Create5(PWideString name);
C_EXPORT QColorH QColor_Create6(const char* name);
C_EXPORT QColorH QColor_Create7(const QColorH color);
C_EXPORT QColorH QColor_Create8(QColor::Spec spec);
C_EXPORT QColorH QColor_Create9(QRgba64 rgba64);
C_EXPORT bool QColor_isValid(QColorH handle);
C_EXPORT void QColor_name(QColorH handle, PWideString retval, QColor::NameFormat format);
C_EXPORT void QColor_setNamedColor(QColorH handle, PWideString name);
C_EXPORT void QColor_colorNames(QStringListH retval);
C_EXPORT QColor::Spec QColor_spec(QColorH handle);
C_EXPORT int QColor_alpha(QColorH handle);
C_EXPORT void QColor_setAlpha(QColorH handle, int alpha);
C_EXPORT qreal QColor_alphaF(QColorH handle);
C_EXPORT void QColor_setAlphaF(QColorH handle, qreal alpha);
C_EXPORT int QColor_red(QColorH handle);
C_EXPORT int QColor_green(QColorH handle);
C_EXPORT int QColor_blue(QColorH handle);
C_EXPORT void QColor_setRed(QColorH handle, int red);
C_EXPORT void QColor_setGreen(QColorH handle, int green);
C_EXPORT void QColor_setBlue(QColorH handle, int blue);
C_EXPORT qreal QColor_redF(QColorH handle);
C_EXPORT qreal QColor_greenF(QColorH handle);
C_EXPORT qreal QColor_blueF(QColorH handle);
C_EXPORT void QColor_setRedF(QColorH handle, qreal red);
C_EXPORT void QColor_setGreenF(QColorH handle, qreal green);
C_EXPORT void QColor_setBlueF(QColorH handle, qreal blue);
C_EXPORT void QColor_getRgb(QColorH handle, int* r, int* g, int* b, int* a);
C_EXPORT void QColor_setRgb(QColorH handle, int r, int g, int b, int a);
C_EXPORT void QColor_getRgbF(QColorH handle, qreal* r, qreal* g, qreal* b, qreal* a);
C_EXPORT void QColor_setRgbF(QColorH handle, qreal r, qreal g, qreal b, qreal a);
C_EXPORT QRgb QColor_rgba(QColorH handle);
C_EXPORT void QColor_setRgba(QColorH handle, QRgb rgba);
C_EXPORT QRgb QColor_rgb(QColorH handle);
C_EXPORT void QColor_setRgb2(QColorH handle, QRgb rgb);
C_EXPORT int QColor_hue(QColorH handle);
C_EXPORT int QColor_saturation(QColorH handle);
C_EXPORT int QColor_hsvHue(QColorH handle);
C_EXPORT int QColor_hsvSaturation(QColorH handle);
C_EXPORT int QColor_value(QColorH handle);
C_EXPORT qreal QColor_hueF(QColorH handle);
C_EXPORT qreal QColor_saturationF(QColorH handle);
C_EXPORT qreal QColor_hsvHueF(QColorH handle);
C_EXPORT qreal QColor_hsvSaturationF(QColorH handle);
C_EXPORT qreal QColor_valueF(QColorH handle);
C_EXPORT void QColor_getHsv(QColorH handle, int* h, int* s, int* v, int* a);
C_EXPORT void QColor_setHsv(QColorH handle, int h, int s, int v, int a);
C_EXPORT void QColor_getHsvF(QColorH handle, qreal* h, qreal* s, qreal* v, qreal* a);
C_EXPORT void QColor_setHsvF(QColorH handle, qreal h, qreal s, qreal v, qreal a);
C_EXPORT int QColor_cyan(QColorH handle);
C_EXPORT int QColor_magenta(QColorH handle);
C_EXPORT int QColor_yellow(QColorH handle);
C_EXPORT int QColor_black(QColorH handle);
C_EXPORT qreal QColor_cyanF(QColorH handle);
C_EXPORT qreal QColor_magentaF(QColorH handle);
C_EXPORT qreal QColor_yellowF(QColorH handle);
C_EXPORT qreal QColor_blackF(QColorH handle);
C_EXPORT void QColor_getCmyk(QColorH handle, int* c, int* m, int* y, int* k, int* a);
C_EXPORT void QColor_setCmyk(QColorH handle, int c, int m, int y, int k, int a);
C_EXPORT void QColor_getCmykF(QColorH handle, qreal* c, qreal* m, qreal* y, qreal* k, qreal* a);
C_EXPORT void QColor_setCmykF(QColorH handle, qreal c, qreal m, qreal y, qreal k, qreal a);
C_EXPORT int QColor_hslHue(QColorH handle);
C_EXPORT int QColor_hslSaturation(QColorH handle);
C_EXPORT int QColor_lightness(QColorH handle);
C_EXPORT qreal QColor_hslHueF(QColorH handle);
C_EXPORT qreal QColor_hslSaturationF(QColorH handle);
C_EXPORT qreal QColor_lightnessF(QColorH handle);
C_EXPORT void QColor_getHsl(QColorH handle, int* h, int* s, int* l, int* a);
C_EXPORT void QColor_setHsl(QColorH handle, int h, int s, int l, int a);
C_EXPORT void QColor_getHslF(QColorH handle, qreal* h, qreal* s, qreal* l, qreal* a);
C_EXPORT void QColor_setHslF(QColorH handle, qreal h, qreal s, qreal l, qreal a);
C_EXPORT void QColor_toRgb(QColorH handle, PQColor retval);
C_EXPORT void QColor_toHsv(QColorH handle, PQColor retval);
C_EXPORT void QColor_toCmyk(QColorH handle, PQColor retval);
C_EXPORT void QColor_toHsl(QColorH handle, PQColor retval);
C_EXPORT void QColor_convertTo(QColorH handle, PQColor retval, QColor::Spec colorSpec);
C_EXPORT void QColor_fromRgb(PQColor retval, QRgb rgb);
C_EXPORT void QColor_fromRgba(PQColor retval, QRgb rgba);
C_EXPORT void QColor_fromRgba64(PQColor retval, QRgba64 rgba64);
C_EXPORT void QColor_fromRgba642(PQColor retval, ushort r, ushort g, ushort b, ushort a);
C_EXPORT void QColor_fromRgb2(PQColor retval, int r, int g, int b, int a);
C_EXPORT void QColor_fromRgbF(PQColor retval, qreal r, qreal g, qreal b, qreal a);
C_EXPORT void QColor_fromHsv(PQColor retval, int h, int s, int v, int a);
C_EXPORT void QColor_fromHsvF(PQColor retval, qreal h, qreal s, qreal v, qreal a);
C_EXPORT void QColor_fromCmyk(PQColor retval, int c, int m, int y, int k, int a);
C_EXPORT void QColor_fromCmykF(PQColor retval, qreal c, qreal m, qreal y, qreal k, qreal a);
C_EXPORT void QColor_fromHsl(PQColor retval, int h, int s, int l, int a);
C_EXPORT void QColor_fromHslF(PQColor retval, qreal h, qreal s, qreal l, qreal a);
C_EXPORT void QColor_lighter(QColorH handle, PQColor retval, int f);
C_EXPORT void QColor_darker(QColorH handle, PQColor retval, int f);
C_EXPORT bool QColor_isValidColor(PWideString name);

C_EXPORT QRgb QColor_Rgba2(int r, int g, int b, int a);
C_EXPORT int QColor_alpha2(QRgb rgba);
C_EXPORT int QColor_red2(QRgb rgba);
C_EXPORT int QColor_green2(QRgb rgba);
C_EXPORT int QColor_blue2(QRgb rgba);
C_EXPORT QRgb QColor_premultiply(QRgb rgba);
C_EXPORT QRgb QColor_unpremultiply(QRgb rgba);


#endif
