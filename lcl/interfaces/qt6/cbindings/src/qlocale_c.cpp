//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qlocale_c.h"

QLocaleH QLocale_Create()
{
	return (QLocaleH) new QLocale();
}

void QLocale_Destroy(QLocaleH handle)
{
	delete (QLocale *)handle;
}

QLocaleH QLocale_Create2(PWideString name)
{
	QString t_name;
	copyPWideStringToQString(name, t_name);
	return (QLocaleH) new QLocale(t_name);
}

QLocaleH QLocale_Create3(QLocale::Language language, QLocale::Country country)
{
	return (QLocaleH) new QLocale(language, country);
}

QLocaleH QLocale_Create4(QLocale::Language language, QLocale::Script script, QLocale::Country country)
{
	return (QLocaleH) new QLocale(language, script, country);
}

QLocaleH QLocale_Create5(const QLocaleH other)
{
	return (QLocaleH) new QLocale(*(const QLocale*)other);
}

QLocale::Language QLocale_language(QLocaleH handle)
{
	return (QLocale::Language) ((QLocale *)handle)->language();
}

QLocale::Script QLocale_script(QLocaleH handle)
{
	return (QLocale::Script) ((QLocale *)handle)->script();
}

QLocale::Country QLocale_country(QLocaleH handle)
{
	return (QLocale::Country) ((QLocale *)handle)->country();
}

void QLocale_name(QLocaleH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QLocale *)handle)->name();
	copyQStringToPWideString(t_retval, retval);
}

void QLocale_bcp47Name(QLocaleH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QLocale *)handle)->bcp47Name();
	copyQStringToPWideString(t_retval, retval);
}

void QLocale_nativeLanguageName(QLocaleH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QLocale *)handle)->nativeLanguageName();
	copyQStringToPWideString(t_retval, retval);
}

void QLocale_nativeCountryName(QLocaleH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QLocale *)handle)->nativeCountryName();
	copyQStringToPWideString(t_retval, retval);
}

short QLocale_toShort(QLocaleH handle, PWideString s, bool* ok)
{
	QString t_s;
	copyPWideStringToQString(s, t_s);
	return (short) ((QLocale *)handle)->toShort(t_s, ok);
}

ushort QLocale_toUShort(QLocaleH handle, PWideString s, bool* ok)
{
	QString t_s;
	copyPWideStringToQString(s, t_s);
	return (ushort) ((QLocale *)handle)->toUShort(t_s, ok);
}

int QLocale_toInt(QLocaleH handle, PWideString s, bool* ok)
{
	QString t_s;
	copyPWideStringToQString(s, t_s);
	return (int) ((QLocale *)handle)->toInt(t_s, ok);
}

uint QLocale_toUInt(QLocaleH handle, PWideString s, bool* ok)
{
	QString t_s;
	copyPWideStringToQString(s, t_s);
	return (uint) ((QLocale *)handle)->toUInt(t_s, ok);
}

qlonglong QLocale_toLongLong(QLocaleH handle, PWideString s, bool* ok)
{
	QString t_s;
	copyPWideStringToQString(s, t_s);
	return (qlonglong) ((QLocale *)handle)->toLongLong(t_s, ok);
}

qulonglong QLocale_toULongLong(QLocaleH handle, PWideString s, bool* ok)
{
	QString t_s;
	copyPWideStringToQString(s, t_s);
	return (qulonglong) ((QLocale *)handle)->toULongLong(t_s, ok);
}

float QLocale_toFloat(QLocaleH handle, PWideString s, bool* ok)
{
	QString t_s;
	copyPWideStringToQString(s, t_s);
	return (float) ((QLocale *)handle)->toFloat(t_s, ok);
}

double QLocale_toDouble(QLocaleH handle, PWideString s, bool* ok)
{
	QString t_s;
	copyPWideStringToQString(s, t_s);
	return (double) ((QLocale *)handle)->toDouble(t_s, ok);
}

void QLocale_toString(QLocaleH handle, PWideString retval, qlonglong i)
{
	QString t_retval;
	t_retval = ((QLocale *)handle)->toString(i);
	copyQStringToPWideString(t_retval, retval);
}

void QLocale_toString2(QLocaleH handle, PWideString retval, qulonglong i)
{
	QString t_retval;
	t_retval = ((QLocale *)handle)->toString(i);
	copyQStringToPWideString(t_retval, retval);
}

void QLocale_toString3(QLocaleH handle, PWideString retval, short i)
{
	QString t_retval;
	t_retval = ((QLocale *)handle)->toString(i);
	copyQStringToPWideString(t_retval, retval);
}

void QLocale_toString4(QLocaleH handle, PWideString retval, ushort i)
{
	QString t_retval;
	t_retval = ((QLocale *)handle)->toString(i);
	copyQStringToPWideString(t_retval, retval);
}

void QLocale_toString5(QLocaleH handle, PWideString retval, int i)
{
	QString t_retval;
	t_retval = ((QLocale *)handle)->toString(i);
	copyQStringToPWideString(t_retval, retval);
}

void QLocale_toString6(QLocaleH handle, PWideString retval, uint i)
{
	QString t_retval;
	t_retval = ((QLocale *)handle)->toString(i);
	copyQStringToPWideString(t_retval, retval);
}

void QLocale_toString7(QLocaleH handle, PWideString retval, double i, char f, int prec)
{
	QString t_retval;
	t_retval = ((QLocale *)handle)->toString(i, f, prec);
	copyQStringToPWideString(t_retval, retval);
}

void QLocale_toString8(QLocaleH handle, PWideString retval, float i, char f, int prec)
{
	QString t_retval;
	t_retval = ((QLocale *)handle)->toString(i, f, prec);
	copyQStringToPWideString(t_retval, retval);
}

void QLocale_toString9(QLocaleH handle, PWideString retval, const QDateH date, PWideString formatStr)
{
	QString t_retval;
	QString t_formatStr;
	copyPWideStringToQString(formatStr, t_formatStr);
	t_retval = ((QLocale *)handle)->toString(*(const QDate*)date, t_formatStr);
	copyQStringToPWideString(t_retval, retval);
}

void QLocale_toString10(QLocaleH handle, PWideString retval, const QDateH date, QLocale::FormatType format)
{
	QString t_retval;
	t_retval = ((QLocale *)handle)->toString(*(const QDate*)date, format);
	copyQStringToPWideString(t_retval, retval);
}

void QLocale_toString11(QLocaleH handle, PWideString retval, const QTimeH time, PWideString formatStr)
{
	QString t_retval;
	QString t_formatStr;
	copyPWideStringToQString(formatStr, t_formatStr);
	t_retval = ((QLocale *)handle)->toString(*(const QTime*)time, t_formatStr);
	copyQStringToPWideString(t_retval, retval);
}

void QLocale_toString12(QLocaleH handle, PWideString retval, const QTimeH time, QLocale::FormatType format)
{
	QString t_retval;
	t_retval = ((QLocale *)handle)->toString(*(const QTime*)time, format);
	copyQStringToPWideString(t_retval, retval);
}

void QLocale_toString13(QLocaleH handle, PWideString retval, const QDateTimeH dateTime, QLocale::FormatType format)
{
	QString t_retval;
	t_retval = ((QLocale *)handle)->toString(*(const QDateTime*)dateTime, format);
	copyQStringToPWideString(t_retval, retval);
}

void QLocale_toString14(QLocaleH handle, PWideString retval, const QDateTimeH dateTime, PWideString format)
{
	QString t_retval;
	QString t_format;
	copyPWideStringToQString(format, t_format);
	t_retval = ((QLocale *)handle)->toString(*(const QDateTime*)dateTime, t_format);
	copyQStringToPWideString(t_retval, retval);
}

void QLocale_dateFormat(QLocaleH handle, PWideString retval, QLocale::FormatType format)
{
	QString t_retval;
	t_retval = ((QLocale *)handle)->dateFormat(format);
	copyQStringToPWideString(t_retval, retval);
}

void QLocale_timeFormat(QLocaleH handle, PWideString retval, QLocale::FormatType format)
{
	QString t_retval;
	t_retval = ((QLocale *)handle)->timeFormat(format);
	copyQStringToPWideString(t_retval, retval);
}

void QLocale_dateTimeFormat(QLocaleH handle, PWideString retval, QLocale::FormatType format)
{
	QString t_retval;
	t_retval = ((QLocale *)handle)->dateTimeFormat(format);
	copyQStringToPWideString(t_retval, retval);
}

void QLocale_toDate(QLocaleH handle, QDateH retval, PWideString string, QLocale::FormatType AnonParam2)
{
	QString t_string;
	copyPWideStringToQString(string, t_string);
	*(QDate *)retval = ((QLocale *)handle)->toDate(t_string, AnonParam2);
}

void QLocale_toTime(QLocaleH handle, QTimeH retval, PWideString string, QLocale::FormatType AnonParam2)
{
	QString t_string;
	copyPWideStringToQString(string, t_string);
	*(QTime *)retval = ((QLocale *)handle)->toTime(t_string, AnonParam2);
}

void QLocale_toDateTime(QLocaleH handle, QDateTimeH retval, PWideString string, QLocale::FormatType format)
{
	QString t_string;
	copyPWideStringToQString(string, t_string);
	*(QDateTime *)retval = ((QLocale *)handle)->toDateTime(t_string, format);
}

void QLocale_toDate2(QLocaleH handle, QDateH retval, PWideString string, PWideString format)
{
	QString t_string;
	QString t_format;
	copyPWideStringToQString(string, t_string);
	copyPWideStringToQString(format, t_format);
	*(QDate *)retval = ((QLocale *)handle)->toDate(t_string, t_format);
}

void QLocale_toTime2(QLocaleH handle, QTimeH retval, PWideString string, PWideString format)
{
	QString t_string;
	QString t_format;
	copyPWideStringToQString(string, t_string);
	copyPWideStringToQString(format, t_format);
	*(QTime *)retval = ((QLocale *)handle)->toTime(t_string, t_format);
}

void QLocale_toDateTime2(QLocaleH handle, QDateTimeH retval, PWideString string, PWideString format)
{
	QString t_string;
	QString t_format;
	copyPWideStringToQString(string, t_string);
	copyPWideStringToQString(format, t_format);
	*(QDateTime *)retval = ((QLocale *)handle)->toDateTime(t_string, t_format);
}

void QLocale_decimalPoint(QLocaleH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QLocale *)handle)->decimalPoint();
	copyQStringToPWideString(t_retval, retval);
}

void QLocale_groupSeparator(QLocaleH handle, PWideString retval)
{
  QString t_retval;
	t_retval = ((QLocale *)handle)->groupSeparator();
	copyQStringToPWideString(t_retval, retval);
}

void QLocale_percent(QLocaleH handle, PWideString retval)
{
  QString t_retval;
	t_retval = ((QLocale *)handle)->percent();
	copyQStringToPWideString(t_retval, retval);
}

void QLocale_zeroDigit(QLocaleH handle, PWideString retval)
{
  QString t_retval;
	t_retval = ((QLocale *)handle)->zeroDigit();
	copyQStringToPWideString(t_retval, retval);
}

void QLocale_negativeSign(QLocaleH handle, PWideString retval)
{
  QString t_retval;
	t_retval = ((QLocale *)handle)->negativeSign();
	copyQStringToPWideString(t_retval, retval);
}

void QLocale_positiveSign(QLocaleH handle, PWideString retval)
{
  QString t_retval;
	t_retval = ((QLocale *)handle)->positiveSign();
	copyQStringToPWideString(t_retval, retval);
}

void QLocale_exponential(QLocaleH handle, PWideString retval)
{
  QString t_retval;
	t_retval = ((QLocale *)handle)->exponential();
	copyQStringToPWideString(t_retval, retval);
}

void QLocale_monthName(QLocaleH handle, PWideString retval, int AnonParam1, QLocale::FormatType format)
{
	QString t_retval;
	t_retval = ((QLocale *)handle)->monthName(AnonParam1, format);
	copyQStringToPWideString(t_retval, retval);
}

void QLocale_standaloneMonthName(QLocaleH handle, PWideString retval, int AnonParam1, QLocale::FormatType format)
{
	QString t_retval;
	t_retval = ((QLocale *)handle)->standaloneMonthName(AnonParam1, format);
	copyQStringToPWideString(t_retval, retval);
}

void QLocale_dayName(QLocaleH handle, PWideString retval, int AnonParam1, QLocale::FormatType format)
{
	QString t_retval;
	t_retval = ((QLocale *)handle)->dayName(AnonParam1, format);
	copyQStringToPWideString(t_retval, retval);
}

void QLocale_standaloneDayName(QLocaleH handle, PWideString retval, int AnonParam1, QLocale::FormatType format)
{
	QString t_retval;
	t_retval = ((QLocale *)handle)->standaloneDayName(AnonParam1, format);
	copyQStringToPWideString(t_retval, retval);
}

Qt::DayOfWeek QLocale_firstDayOfWeek(QLocaleH handle)
{
	return (Qt::DayOfWeek) ((QLocale *)handle)->firstDayOfWeek();
}

void QLocale_weekdays(QLocaleH handle, PPtrIntArray retval)
{
	QList<Qt::DayOfWeek> t_retval;
	t_retval = ((QLocale *)handle)->weekdays();
	copyQListTemplateToPtrIntArray(t_retval, retval);
}

void QLocale_amText(QLocaleH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QLocale *)handle)->amText();
	copyQStringToPWideString(t_retval, retval);
}

void QLocale_pmText(QLocaleH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QLocale *)handle)->pmText();
	copyQStringToPWideString(t_retval, retval);
}

QLocale::MeasurementSystem QLocale_measurementSystem(QLocaleH handle)
{
	return (QLocale::MeasurementSystem) ((QLocale *)handle)->measurementSystem();
}

Qt::LayoutDirection QLocale_textDirection(QLocaleH handle)
{
	return (Qt::LayoutDirection) ((QLocale *)handle)->textDirection();
}

void QLocale_toUpper(QLocaleH handle, PWideString retval, PWideString str)
{
	QString t_retval;
	QString t_str;
	copyPWideStringToQString(str, t_str);
	t_retval = ((QLocale *)handle)->toUpper(t_str);
	copyQStringToPWideString(t_retval, retval);
}

void QLocale_toLower(QLocaleH handle, PWideString retval, PWideString str)
{
	QString t_retval;
	QString t_str;
	copyPWideStringToQString(str, t_str);
	t_retval = ((QLocale *)handle)->toLower(t_str);
	copyQStringToPWideString(t_retval, retval);
}

void QLocale_currencySymbol(QLocaleH handle, PWideString retval, QLocale::CurrencySymbolFormat AnonParam1)
{
	QString t_retval;
	t_retval = ((QLocale *)handle)->currencySymbol(AnonParam1);
	copyQStringToPWideString(t_retval, retval);
}

void QLocale_toCurrencyString(QLocaleH handle, PWideString retval, qlonglong AnonParam1, PWideString symbol)
{
	QString t_retval;
	QString t_symbol;
	copyPWideStringToQString(symbol, t_symbol);
	t_retval = ((QLocale *)handle)->toCurrencyString(AnonParam1, t_symbol);
	copyQStringToPWideString(t_retval, retval);
}

void QLocale_toCurrencyString2(QLocaleH handle, PWideString retval, qulonglong AnonParam1, PWideString symbol)
{
	QString t_retval;
	QString t_symbol;
	copyPWideStringToQString(symbol, t_symbol);
	t_retval = ((QLocale *)handle)->toCurrencyString(AnonParam1, t_symbol);
	copyQStringToPWideString(t_retval, retval);
}

void QLocale_toCurrencyString3(QLocaleH handle, PWideString retval, short AnonParam1, PWideString symbol)
{
	QString t_retval;
	QString t_symbol;
	copyPWideStringToQString(symbol, t_symbol);
	t_retval = ((QLocale *)handle)->toCurrencyString(AnonParam1, t_symbol);
	copyQStringToPWideString(t_retval, retval);
}

void QLocale_toCurrencyString4(QLocaleH handle, PWideString retval, ushort AnonParam1, PWideString symbol)
{
	QString t_retval;
	QString t_symbol;
	copyPWideStringToQString(symbol, t_symbol);
	t_retval = ((QLocale *)handle)->toCurrencyString(AnonParam1, t_symbol);
	copyQStringToPWideString(t_retval, retval);
}

void QLocale_toCurrencyString5(QLocaleH handle, PWideString retval, int AnonParam1, PWideString symbol)
{
	QString t_retval;
	QString t_symbol;
	copyPWideStringToQString(symbol, t_symbol);
	t_retval = ((QLocale *)handle)->toCurrencyString(AnonParam1, t_symbol);
	copyQStringToPWideString(t_retval, retval);
}

void QLocale_toCurrencyString6(QLocaleH handle, PWideString retval, uint AnonParam1, PWideString symbol)
{
	QString t_retval;
	QString t_symbol;
	copyPWideStringToQString(symbol, t_symbol);
	t_retval = ((QLocale *)handle)->toCurrencyString(AnonParam1, t_symbol);
	copyQStringToPWideString(t_retval, retval);
}

void QLocale_toCurrencyString7(QLocaleH handle, PWideString retval, double AnonParam1, PWideString symbol)
{
	QString t_retval;
	QString t_symbol;
	copyPWideStringToQString(symbol, t_symbol);
	t_retval = ((QLocale *)handle)->toCurrencyString(AnonParam1, t_symbol);
	copyQStringToPWideString(t_retval, retval);
}

void QLocale_toCurrencyString8(QLocaleH handle, PWideString retval, float AnonParam1, PWideString symbol)
{
	QString t_retval;
	QString t_symbol;
	copyPWideStringToQString(symbol, t_symbol);
	t_retval = ((QLocale *)handle)->toCurrencyString(AnonParam1, t_symbol);
	copyQStringToPWideString(t_retval, retval);
}

void QLocale_uiLanguages(QLocaleH handle, QStringListH retval)
{
	*(QStringList *)retval = ((QLocale *)handle)->uiLanguages();
}

void QLocale_languageToString(PWideString retval, QLocale::Language language)
{
	QString t_retval;
	t_retval = QLocale::languageToString(language);
	copyQStringToPWideString(t_retval, retval);
}

void QLocale_countryToString(PWideString retval, QLocale::Country country)
{
	QString t_retval;
	t_retval = QLocale::countryToString(country);
	copyQStringToPWideString(t_retval, retval);
}

void QLocale_scriptToString(PWideString retval, QLocale::Script script)
{
	QString t_retval;
	t_retval = QLocale::scriptToString(script);
	copyQStringToPWideString(t_retval, retval);
}

void QLocale_setDefault(const QLocaleH locale)
{
	QLocale::setDefault(*(const QLocale*)locale);
}

void QLocale_c(QLocaleH retval)
{
	*(QLocale *)retval = QLocale::c();
}

void QLocale_system(QLocaleH retval)
{
	*(QLocale *)retval = QLocale::system();
}

void QLocale_matchingLocales(PPtrIntArray retval, QLocale::Language language, QLocale::Script script, QLocale::Country country)
{
	QList<QLocale> t_retval;
	t_retval = QLocale::matchingLocales(language, script, country);
	copyQListTemplateToPtrIntArrayWithNew(t_retval, retval);
}

void QLocale_setNumberOptions(QLocaleH handle, unsigned int options)
{
	((QLocale *)handle)->setNumberOptions((QLocale::NumberOptions)options);
}

unsigned int QLocale_numberOptions(QLocaleH handle)
{
	return (unsigned int) ((QLocale *)handle)->numberOptions();
}

void QLocale_quoteString(QLocaleH handle, PWideString retval, PWideString str, QLocale::QuotationStyle style)
{
	QString t_retval;
	QString t_str;
	copyPWideStringToQString(str, t_str);
	t_retval = ((QLocale *)handle)->quoteString(t_str, style);
	copyQStringToPWideString(t_retval, retval);
}

void QLocale_createSeparatedList(QLocaleH handle, PWideString retval, const QStringListH strl)
{
	QString t_retval;
	t_retval = ((QLocale *)handle)->createSeparatedList(*(const QStringList*)strl);
	copyQStringToPWideString(t_retval, retval);
}

