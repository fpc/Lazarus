//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QLOCALE_C_H
#define QLOCALE_C_H

#include <QtCore>
#include "pascalbind.h"

C_EXPORT QLocaleH QLocale_Create();
C_EXPORT void QLocale_Destroy(QLocaleH handle);
C_EXPORT QLocaleH QLocale_Create2(PWideString name);
C_EXPORT QLocaleH QLocale_Create3(QLocale::Language language, QLocale::Country country);
C_EXPORT QLocaleH QLocale_Create4(QLocale::Language language, QLocale::Script script, QLocale::Country country);
C_EXPORT QLocaleH QLocale_Create5(const QLocaleH other);
C_EXPORT QLocale::Language QLocale_language(QLocaleH handle);
C_EXPORT QLocale::Script QLocale_script(QLocaleH handle);
C_EXPORT QLocale::Country QLocale_country(QLocaleH handle);
C_EXPORT void QLocale_name(QLocaleH handle, PWideString retval);
C_EXPORT void QLocale_bcp47Name(QLocaleH handle, PWideString retval);
C_EXPORT void QLocale_nativeLanguageName(QLocaleH handle, PWideString retval);
C_EXPORT void QLocale_nativeCountryName(QLocaleH handle, PWideString retval);
C_EXPORT short QLocale_toShort(QLocaleH handle, PWideString s, bool* ok);
C_EXPORT ushort QLocale_toUShort(QLocaleH handle, PWideString s, bool* ok);
C_EXPORT int QLocale_toInt(QLocaleH handle, PWideString s, bool* ok);
C_EXPORT uint QLocale_toUInt(QLocaleH handle, PWideString s, bool* ok);
C_EXPORT qlonglong QLocale_toLongLong(QLocaleH handle, PWideString s, bool* ok);
C_EXPORT qulonglong QLocale_toULongLong(QLocaleH handle, PWideString s, bool* ok);
C_EXPORT float QLocale_toFloat(QLocaleH handle, PWideString s, bool* ok);
C_EXPORT double QLocale_toDouble(QLocaleH handle, PWideString s, bool* ok);
C_EXPORT void QLocale_toString(QLocaleH handle, PWideString retval, qlonglong i);
C_EXPORT void QLocale_toString2(QLocaleH handle, PWideString retval, qulonglong i);
C_EXPORT void QLocale_toString3(QLocaleH handle, PWideString retval, short i);
C_EXPORT void QLocale_toString4(QLocaleH handle, PWideString retval, ushort i);
C_EXPORT void QLocale_toString5(QLocaleH handle, PWideString retval, int i);
C_EXPORT void QLocale_toString6(QLocaleH handle, PWideString retval, uint i);
C_EXPORT void QLocale_toString7(QLocaleH handle, PWideString retval, double i, char f, int prec);
C_EXPORT void QLocale_toString8(QLocaleH handle, PWideString retval, float i, char f, int prec);
C_EXPORT void QLocale_toString9(QLocaleH handle, PWideString retval, const QDateH date, PWideString formatStr);
C_EXPORT void QLocale_toString10(QLocaleH handle, PWideString retval, const QDateH date, QLocale::FormatType format);
C_EXPORT void QLocale_toString11(QLocaleH handle, PWideString retval, const QTimeH time, PWideString formatStr);
C_EXPORT void QLocale_toString12(QLocaleH handle, PWideString retval, const QTimeH time, QLocale::FormatType format);
C_EXPORT void QLocale_toString13(QLocaleH handle, PWideString retval, const QDateTimeH dateTime, QLocale::FormatType format);
C_EXPORT void QLocale_toString14(QLocaleH handle, PWideString retval, const QDateTimeH dateTime, PWideString format);
C_EXPORT void QLocale_dateFormat(QLocaleH handle, PWideString retval, QLocale::FormatType format);
C_EXPORT void QLocale_timeFormat(QLocaleH handle, PWideString retval, QLocale::FormatType format);
C_EXPORT void QLocale_dateTimeFormat(QLocaleH handle, PWideString retval, QLocale::FormatType format);
C_EXPORT void QLocale_toDate(QLocaleH handle, QDateH retval, PWideString string, QLocale::FormatType AnonParam2);
C_EXPORT void QLocale_toTime(QLocaleH handle, QTimeH retval, PWideString string, QLocale::FormatType AnonParam2);
C_EXPORT void QLocale_toDateTime(QLocaleH handle, QDateTimeH retval, PWideString string, QLocale::FormatType format);
C_EXPORT void QLocale_toDate2(QLocaleH handle, QDateH retval, PWideString string, PWideString format);
C_EXPORT void QLocale_toTime2(QLocaleH handle, QTimeH retval, PWideString string, PWideString format);
C_EXPORT void QLocale_toDateTime2(QLocaleH handle, QDateTimeH retval, PWideString string, PWideString format);
C_EXPORT void QLocale_decimalPoint(QLocaleH handle, PWideString retval);
C_EXPORT void QLocale_groupSeparator(QLocaleH handle, PWideString retval);
C_EXPORT void QLocale_percent(QLocaleH handle, PWideString retval);
C_EXPORT void QLocale_zeroDigit(QLocaleH handle, PWideString retval);
C_EXPORT void QLocale_negativeSign(QLocaleH handle, PWideString retval);
C_EXPORT void QLocale_positiveSign(QLocaleH handle, PWideString retval);
C_EXPORT void QLocale_exponential(QLocaleH handle, PWideString retval);
C_EXPORT void QLocale_monthName(QLocaleH handle, PWideString retval, int AnonParam1, QLocale::FormatType format);
C_EXPORT void QLocale_standaloneMonthName(QLocaleH handle, PWideString retval, int AnonParam1, QLocale::FormatType format);
C_EXPORT void QLocale_dayName(QLocaleH handle, PWideString retval, int AnonParam1, QLocale::FormatType format);
C_EXPORT void QLocale_standaloneDayName(QLocaleH handle, PWideString retval, int AnonParam1, QLocale::FormatType format);
C_EXPORT Qt::DayOfWeek QLocale_firstDayOfWeek(QLocaleH handle);
C_EXPORT void QLocale_weekdays(QLocaleH handle, PPtrIntArray retval);
C_EXPORT void QLocale_amText(QLocaleH handle, PWideString retval);
C_EXPORT void QLocale_pmText(QLocaleH handle, PWideString retval);
C_EXPORT QLocale::MeasurementSystem QLocale_measurementSystem(QLocaleH handle);
C_EXPORT Qt::LayoutDirection QLocale_textDirection(QLocaleH handle);
C_EXPORT void QLocale_toUpper(QLocaleH handle, PWideString retval, PWideString str);
C_EXPORT void QLocale_toLower(QLocaleH handle, PWideString retval, PWideString str);
C_EXPORT void QLocale_currencySymbol(QLocaleH handle, PWideString retval, QLocale::CurrencySymbolFormat AnonParam1);
C_EXPORT void QLocale_toCurrencyString(QLocaleH handle, PWideString retval, qlonglong AnonParam1, PWideString symbol);
C_EXPORT void QLocale_toCurrencyString2(QLocaleH handle, PWideString retval, qulonglong AnonParam1, PWideString symbol);
C_EXPORT void QLocale_toCurrencyString3(QLocaleH handle, PWideString retval, short AnonParam1, PWideString symbol);
C_EXPORT void QLocale_toCurrencyString4(QLocaleH handle, PWideString retval, ushort AnonParam1, PWideString symbol);
C_EXPORT void QLocale_toCurrencyString5(QLocaleH handle, PWideString retval, int AnonParam1, PWideString symbol);
C_EXPORT void QLocale_toCurrencyString6(QLocaleH handle, PWideString retval, uint AnonParam1, PWideString symbol);
C_EXPORT void QLocale_toCurrencyString7(QLocaleH handle, PWideString retval, double AnonParam1, PWideString symbol);
C_EXPORT void QLocale_toCurrencyString8(QLocaleH handle, PWideString retval, float AnonParam1, PWideString symbol);
C_EXPORT void QLocale_uiLanguages(QLocaleH handle, QStringListH retval);
C_EXPORT void QLocale_languageToString(PWideString retval, QLocale::Language language);
C_EXPORT void QLocale_countryToString(PWideString retval, QLocale::Country country);
C_EXPORT void QLocale_scriptToString(PWideString retval, QLocale::Script script);
C_EXPORT void QLocale_setDefault(const QLocaleH locale);
C_EXPORT void QLocale_c(QLocaleH retval);
C_EXPORT void QLocale_system(QLocaleH retval);
C_EXPORT void QLocale_matchingLocales(PPtrIntArray retval, QLocale::Language language, QLocale::Script script, QLocale::Country country);
C_EXPORT void QLocale_setNumberOptions(QLocaleH handle, unsigned int options);
C_EXPORT unsigned int QLocale_numberOptions(QLocaleH handle);
C_EXPORT void QLocale_quoteString(QLocaleH handle, PWideString retval, PWideString str, QLocale::QuotationStyle style);
C_EXPORT void QLocale_createSeparatedList(QLocaleH handle, PWideString retval, const QStringListH strl);

#endif
