//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qvariant_c.h"

QVariantH QVariant_Create()
{
	return (QVariantH) new QVariant();
}

void QVariant_Destroy(QVariantH handle)
{
	delete (QVariant *)handle;
}

QVariantH QVariant_Create3(QMetaType typeId, const void* copy)
{
	return (QVariantH) new QVariant(typeId, copy);
}

QVariantH QVariant_Create5(const QVariantH other)
{
	return (QVariantH) new QVariant(*(const QVariant*)other);
}

/*QVariantH QVariant_Create6(QDataStreamH s)
{
	return (QVariantH) new QVariant(*(QDataStream*)s);
}*/

QVariantH QVariant_Create8(uint ui)
{
	return (QVariantH) new QVariant(ui);
}

QVariantH QVariant_Create9(qlonglong ll)
{
	return (QVariantH) new QVariant(ll);
}

QVariantH QVariant_Create10(qulonglong ull)
{
	return (QVariantH) new QVariant(ull);
}

QVariantH QVariant_Create11(bool b)
{
	return (QVariantH) new QVariant(b);
}

QVariantH QVariant_Create12(double d)
{
	return (QVariantH) new QVariant(d);
}

QVariantH QVariant_Create13(float f)
{
	return (QVariantH) new QVariant(f);
}

QVariantH QVariant_Create14(const char* str)
{
	return (QVariantH) new QVariant(str);
}

QVariantH QVariant_Create15(const QByteArrayH bytearray)
{
	return (QVariantH) new QVariant(*(const QByteArray*)bytearray);
}

QVariantH QVariant_Create16(const QBitArrayH bitarray)
{
	return (QVariantH) new QVariant(*(const QBitArray*)bitarray);
}

QVariantH QVariant_Create17(PWideString string)
{
	QString t_string;
	copyPWideStringToQString(string, t_string);
	return (QVariantH) new QVariant(t_string);
}

QVariantH QVariant_Create18(const QStringListH stringlist)
{
	return (QVariantH) new QVariant(*(const QStringList*)stringlist);
}

QVariantH QVariant_Create19(PWideChar qchar)
{
	return (QVariantH) new QVariant(*(QChar *)qchar);
}

QVariantH QVariant_Create20(const QDateH date)
{
	return (QVariantH) new QVariant(*(const QDate*)date);
}

QVariantH QVariant_Create21(const QTimeH time)
{
	return (QVariantH) new QVariant(*(const QTime*)time);
}

QVariantH QVariant_Create25(const QPointH pt)
{
	return (QVariantH) new QVariant(*(const QPoint*)pt);
}

QVariantH QVariant_Create26(const QPointFH pt)
{
	return (QVariantH) new QVariant(*(const QPointF*)pt);
}

QVariantH QVariant_Create27(const QLineH line)
{
	return (QVariantH) new QVariant(*(const QLine*)line);
}

QVariantH QVariant_Create28(const QLineFH line)
{
	return (QVariantH) new QVariant(*(const QLineF*)line);
}

QVariantH QVariant_Create29(PRect rect)
{
	QRect t_rect;
	copyPRectToQRect(rect, t_rect);
	return (QVariantH) new QVariant(t_rect);
}

QVariantH QVariant_Create30(const QRectFH rect)
{
	return (QVariantH) new QVariant(*(const QRectF*)rect);
}

QVariantH QVariant_Create31(const QLocaleH locale)
{
	return (QVariantH) new QVariant(*(const QLocale*)locale);
}

QVariantH QVariant_Create33(const QRegularExpressionH re)
{
	return (QVariantH) new QVariant(*(const QRegularExpression*)re);
}

QVariantH QVariant_Create34(const QUrlH url)
{
	return (QVariantH) new QVariant(*(const QUrl*)url);
}

QVariantH QVariant_Create35(const QEasingCurveH easing)
{
	return (QVariantH) new QVariant(*(const QEasingCurve*)easing);
}

QVariantH QVariant_Create36(const QUuidH uuid)
{
	return (QVariantH) new QVariant(*(const QUuid*)uuid);
}

QVariantH QVariant_Create37(const QModelIndexH modelIndex)
{
	return (QVariantH) new QVariant(*(const QModelIndex*)modelIndex);
}

QVariantH QVariant_Create38(const QJsonValueH jsonValue)
{
	return (QVariantH) new QVariant(*(const QJsonValue*)jsonValue);
}

QVariantH QVariant_Create39(const QJsonObjectH jsonObject)
{
	return (QVariantH) new QVariant(*(const QJsonObject*)jsonObject);
}

QVariantH QVariant_Create40(const QJsonArrayH jsonArray)
{
	return (QVariantH) new QVariant(*(const QJsonArray*)jsonArray);
}

QVariantH QVariant_Create41(const QJsonDocumentH jsonDocument)
{
	return (QVariantH) new QVariant(*(const QJsonDocument*)jsonDocument);
}

void QVariant_swap(QVariantH handle, QVariantH other)
{
	((QVariant *)handle)->swap(*(QVariant*)other);
}

QVariant::Type QVariant_type(QVariantH handle)
{
	return (QVariant::Type) ((QVariant *)handle)->type();
}

int QVariant_userType(QVariantH handle)
{
	return (int) ((QVariant *)handle)->userType();
}

const char* QVariant_typeName(QVariantH handle)
{
	return (const char*) ((QVariant *)handle)->typeName();
}

bool QVariant_canConvert(QVariantH handle, int targetTypeId)
{
	return (bool) ((QVariant *)handle)->canConvert(targetTypeId);
}

bool QVariant_convert(QVariantH handle, int targetTypeId)
{
	return (bool) ((QVariant *)handle)->convert(targetTypeId);
}

bool QVariant_isValid(QVariantH handle)
{
	return (bool) ((QVariant *)handle)->isValid();
}

bool QVariant_isNull(QVariantH handle)
{
	return (bool) ((QVariant *)handle)->isNull();
}

void QVariant_clear(QVariantH handle)
{
	((QVariant *)handle)->clear();
}

void QVariant_detach(QVariantH handle)
{
	((QVariant *)handle)->detach();
}

bool QVariant_isDetached(QVariantH handle)
{
	return (bool) ((QVariant *)handle)->isDetached();
}

int QVariant_toInt(QVariantH handle, bool* ok)
{
	return (int) ((QVariant *)handle)->toInt(ok);
}

uint QVariant_toUInt(QVariantH handle, bool* ok)
{
	return (uint) ((QVariant *)handle)->toUInt(ok);
}

qlonglong QVariant_toLongLong(QVariantH handle, bool* ok)
{
	return (qlonglong) ((QVariant *)handle)->toLongLong(ok);
}

qulonglong QVariant_toULongLong(QVariantH handle, bool* ok)
{
	return (qulonglong) ((QVariant *)handle)->toULongLong(ok);
}

bool QVariant_toBool(QVariantH handle)
{
	return (bool) ((QVariant *)handle)->toBool();
}

double QVariant_toDouble(QVariantH handle, bool* ok)
{
	return (double) ((QVariant *)handle)->toDouble(ok);
}

float QVariant_toFloat(QVariantH handle, bool* ok)
{
	return (float) ((QVariant *)handle)->toFloat(ok);
}

qreal QVariant_toReal(QVariantH handle, bool* ok)
{
	return (qreal) ((QVariant *)handle)->toReal(ok);
}

void QVariant_toByteArray(QVariantH handle, QByteArrayH retval)
{
	*(QByteArray *)retval = ((QVariant *)handle)->toByteArray();
}

void QVariant_toBitArray(QVariantH handle, QBitArrayH retval)
{
	*(QBitArray *)retval = ((QVariant *)handle)->toBitArray();
}

void QVariant_toString(QVariantH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QVariant *)handle)->toString();
	copyQStringToPWideString(t_retval, retval);
}

void QVariant_toStringList(QVariantH handle, QStringListH retval)
{
	*(QStringList *)retval = ((QVariant *)handle)->toStringList();
}

void QVariant_toChar(QVariantH handle, PWideChar retval)
{
	*(QChar *)retval = ((QVariant *)handle)->toChar();
}

void QVariant_toDate(QVariantH handle, QDateH retval)
{
	*(QDate *)retval = ((QVariant *)handle)->toDate();
}

void QVariant_toTime(QVariantH handle, QTimeH retval)
{
	*(QTime *)retval = ((QVariant *)handle)->toTime();
}

void QVariant_toDateTime(QVariantH handle, QDateTimeH retval)
{
	*(QDateTime *)retval = ((QVariant *)handle)->toDateTime();
}

void QVariant_toPoint(QVariantH handle, PQtPoint retval)
{
	*(QPoint *)retval = ((QVariant *)handle)->toPoint();
}

void QVariant_toPointF(QVariantH handle, PQtPointF retval)
{
	*(QPointF *)retval = ((QVariant *)handle)->toPointF();
}

void QVariant_toRect(QVariantH handle, PRect retval)
{
	QRect t_retval;
	t_retval = ((QVariant *)handle)->toRect();
	copyQRectToPRect(t_retval, retval);
}

void QVariant_toSize(QVariantH handle, PSize retval)
{
	*(QSize *)retval = ((QVariant *)handle)->toSize();
}

void QVariant_toSizeF(QVariantH handle, QSizeFH retval)
{
	*(QSizeF *)retval = ((QVariant *)handle)->toSizeF();
}

void QVariant_toLine(QVariantH handle, QLineH retval)
{
	*(QLine *)retval = ((QVariant *)handle)->toLine();
}

void QVariant_toLineF(QVariantH handle, QLineFH retval)
{
	*(QLineF *)retval = ((QVariant *)handle)->toLineF();
}

void QVariant_toRectF(QVariantH handle, QRectFH retval)
{
	*(QRectF *)retval = ((QVariant *)handle)->toRectF();
}

void QVariant_toLocale(QVariantH handle, QLocaleH retval)
{
	*(QLocale *)retval = ((QVariant *)handle)->toLocale();
}

void QVariant_toRegularExpression(QVariantH handle, QRegularExpressionH retval)
{
	*(QRegularExpression *)retval = ((QVariant *)handle)->toRegularExpression();
}

void QVariant_toUrl(QVariantH handle, QUrlH retval)
{
	*(QUrl *)retval = ((QVariant *)handle)->toUrl();
}

void QVariant_toEasingCurve(QVariantH handle, QEasingCurveH retval)
{
	*(QEasingCurve *)retval = ((QVariant *)handle)->toEasingCurve();
}

void QVariant_toUuid(QVariantH handle, QUuidH retval)
{
	*(QUuid *)retval = ((QVariant *)handle)->toUuid();
}

void QVariant_toModelIndex(QVariantH handle, QModelIndexH retval)
{
	*(QModelIndex *)retval = ((QVariant *)handle)->toModelIndex();
}

void QVariant_toJsonValue(QVariantH handle, QJsonValueH retval)
{
	*(QJsonValue *)retval = ((QVariant *)handle)->toJsonValue();
}

void QVariant_toJsonObject(QVariantH handle, QJsonObjectH retval)
{
	*(QJsonObject *)retval = ((QVariant *)handle)->toJsonObject();
}

void QVariant_toJsonArray(QVariantH handle, QJsonArrayH retval)
{
	*(QJsonArray *)retval = ((QVariant *)handle)->toJsonArray();
}

void QVariant_toJsonDocument(QVariantH handle, QJsonDocumentH retval)
{
	*(QJsonDocument *)retval = ((QVariant *)handle)->toJsonDocument();
}

void QVariant_load(QVariantH handle, QDataStreamH ds)
{
	((QVariant *)handle)->load(*(QDataStream*)ds);
}

void QVariant_save(QVariantH handle, QDataStreamH ds)
{
	((QVariant *)handle)->save(*(QDataStream*)ds);
}

const char* QVariant_typeToName(int typeId)
{
	return (const char*) QVariant::typeToName(typeId);
}

QVariant::Type QVariant_nameToType(const char* name)
{
	return (QVariant::Type) QVariant::nameToType(name);
}

const void* QVariant_constData(QVariantH handle)
{
	return (const void*) ((QVariant *)handle)->constData();
}

