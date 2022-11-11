//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QVARIANT_C_H
#define QVARIANT_C_H

#include <QtCore>
#include "pascalbind.h"

C_EXPORT QVariantH QVariant_Create();
C_EXPORT void QVariant_Destroy(QVariantH handle);
C_EXPORT QVariantH QVariant_Create3(QMetaType typeId, const void* copy);
C_EXPORT QVariantH QVariant_Create5(const QVariantH other);
/* C_EXPORT QVariantH QVariant_Create6(QDataStreamH s); */
C_EXPORT QVariantH QVariant_Create8(uint ui);
C_EXPORT QVariantH QVariant_Create9(qlonglong ll);
C_EXPORT QVariantH QVariant_Create10(qulonglong ull);
C_EXPORT QVariantH QVariant_Create11(bool b);
C_EXPORT QVariantH QVariant_Create12(double d);
C_EXPORT QVariantH QVariant_Create13(float f);
C_EXPORT QVariantH QVariant_Create14(const char* str);
C_EXPORT QVariantH QVariant_Create15(const QByteArrayH bytearray);
C_EXPORT QVariantH QVariant_Create16(const QBitArrayH bitarray);
C_EXPORT QVariantH QVariant_Create17(PWideString string);
C_EXPORT QVariantH QVariant_Create18(const QStringListH stringlist);
C_EXPORT QVariantH QVariant_Create19(PWideChar qchar);
C_EXPORT QVariantH QVariant_Create20(const QDateH date);
C_EXPORT QVariantH QVariant_Create21(const QTimeH time);
C_EXPORT QVariantH QVariant_Create25(const QPointH pt);
C_EXPORT QVariantH QVariant_Create26(const QPointFH pt);
C_EXPORT QVariantH QVariant_Create27(const QLineH line);
C_EXPORT QVariantH QVariant_Create28(const QLineFH line);
C_EXPORT QVariantH QVariant_Create29(PRect rect);
C_EXPORT QVariantH QVariant_Create30(const QRectFH rect);
C_EXPORT QVariantH QVariant_Create31(const QLocaleH locale);
C_EXPORT QVariantH QVariant_Create33(const QRegularExpressionH re);
C_EXPORT QVariantH QVariant_Create34(const QUrlH url);
C_EXPORT QVariantH QVariant_Create35(const QEasingCurveH easing);
C_EXPORT QVariantH QVariant_Create36(const QUuidH uuid);
C_EXPORT QVariantH QVariant_Create37(const QModelIndexH modelIndex);
C_EXPORT QVariantH QVariant_Create38(const QJsonValueH jsonValue);
C_EXPORT QVariantH QVariant_Create39(const QJsonObjectH jsonObject);
C_EXPORT QVariantH QVariant_Create40(const QJsonArrayH jsonArray);
C_EXPORT QVariantH QVariant_Create41(const QJsonDocumentH jsonDocument);
C_EXPORT void QVariant_swap(QVariantH handle, QVariantH other);
C_EXPORT QVariant::Type QVariant_type(QVariantH handle);
C_EXPORT int QVariant_userType(QVariantH handle);
C_EXPORT const char* QVariant_typeName(QVariantH handle);
C_EXPORT bool QVariant_canConvert(QVariantH handle, int targetTypeId);
C_EXPORT bool QVariant_convert(QVariantH handle, int targetTypeId);
C_EXPORT bool QVariant_isValid(QVariantH handle);
C_EXPORT bool QVariant_isNull(QVariantH handle);
C_EXPORT void QVariant_clear(QVariantH handle);
C_EXPORT void QVariant_detach(QVariantH handle);
C_EXPORT bool QVariant_isDetached(QVariantH handle);
C_EXPORT int QVariant_toInt(QVariantH handle, bool* ok);
C_EXPORT uint QVariant_toUInt(QVariantH handle, bool* ok);
C_EXPORT qlonglong QVariant_toLongLong(QVariantH handle, bool* ok);
C_EXPORT qulonglong QVariant_toULongLong(QVariantH handle, bool* ok);
C_EXPORT bool QVariant_toBool(QVariantH handle);
C_EXPORT double QVariant_toDouble(QVariantH handle, bool* ok);
C_EXPORT float QVariant_toFloat(QVariantH handle, bool* ok);
C_EXPORT qreal QVariant_toReal(QVariantH handle, bool* ok);
C_EXPORT void QVariant_toByteArray(QVariantH handle, QByteArrayH retval);
C_EXPORT void QVariant_toBitArray(QVariantH handle, QBitArrayH retval);
C_EXPORT void QVariant_toString(QVariantH handle, PWideString retval);
C_EXPORT void QVariant_toStringList(QVariantH handle, QStringListH retval);
C_EXPORT void QVariant_toChar(QVariantH handle, PWideChar retval);
C_EXPORT void QVariant_toDate(QVariantH handle, QDateH retval);
C_EXPORT void QVariant_toTime(QVariantH handle, QTimeH retval);
C_EXPORT void QVariant_toDateTime(QVariantH handle, QDateTimeH retval);
C_EXPORT void QVariant_toPoint(QVariantH handle, PQtPoint retval);
C_EXPORT void QVariant_toPointF(QVariantH handle, PQtPointF retval);
C_EXPORT void QVariant_toRect(QVariantH handle, PRect retval);
C_EXPORT void QVariant_toSize(QVariantH handle, PSize retval);
C_EXPORT void QVariant_toSizeF(QVariantH handle, QSizeFH retval);
C_EXPORT void QVariant_toLine(QVariantH handle, QLineH retval);
C_EXPORT void QVariant_toLineF(QVariantH handle, QLineFH retval);
C_EXPORT void QVariant_toRectF(QVariantH handle, QRectFH retval);
C_EXPORT void QVariant_toLocale(QVariantH handle, QLocaleH retval);
C_EXPORT void QVariant_toRegularExpression(QVariantH handle, QRegularExpressionH retval);
C_EXPORT void QVariant_toUrl(QVariantH handle, QUrlH retval);
C_EXPORT void QVariant_toEasingCurve(QVariantH handle, QEasingCurveH retval);
C_EXPORT void QVariant_toUuid(QVariantH handle, QUuidH retval);
C_EXPORT void QVariant_toModelIndex(QVariantH handle, QModelIndexH retval);
C_EXPORT void QVariant_toJsonValue(QVariantH handle, QJsonValueH retval);
C_EXPORT void QVariant_toJsonObject(QVariantH handle, QJsonObjectH retval);
C_EXPORT void QVariant_toJsonArray(QVariantH handle, QJsonArrayH retval);
C_EXPORT void QVariant_toJsonDocument(QVariantH handle, QJsonDocumentH retval);
C_EXPORT void QVariant_load(QVariantH handle, QDataStreamH ds);
C_EXPORT void QVariant_save(QVariantH handle, QDataStreamH ds);
C_EXPORT const char* QVariant_typeToName(int typeId);
C_EXPORT QVariant::Type QVariant_nameToType(const char* name);
C_EXPORT const void* QVariant_constData(QVariantH handle);

#endif
