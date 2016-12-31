//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qtranslator_c.h"

QTranslatorH QTranslator_Create(QObjectH parent)
{
	return (QTranslatorH) new QTranslator((QObject*)parent);
}

void QTranslator_Destroy(QTranslatorH handle)
{
	delete (QTranslator *)handle;
}

void QTranslator_translate(QTranslatorH handle, PWideString retval, const char* context, const char* sourceText, const char* disambiguation, int n)
{
	QString t_retval;
	t_retval = ((QTranslator *)handle)->translate(context, sourceText, disambiguation, n);
	copyQStringToPWideString(t_retval, retval);
}

bool QTranslator_isEmpty(QTranslatorH handle)
{
	return (bool) ((QTranslator *)handle)->isEmpty();
}

bool QTranslator_load(QTranslatorH handle, PWideString filename, PWideString directory, PWideString search_delimiters, PWideString suffix)
{
	QString t_filename;
	QString t_directory;
	QString t_search_delimiters;
	QString t_suffix;
	copyPWideStringToQString(filename, t_filename);
	copyPWideStringToQString(directory, t_directory);
	copyPWideStringToQString(search_delimiters, t_search_delimiters);
	copyPWideStringToQString(suffix, t_suffix);
	return (bool) ((QTranslator *)handle)->load(t_filename, t_directory, t_search_delimiters, t_suffix);
}

bool QTranslator_load2(QTranslatorH handle, const QLocaleH locale, PWideString filename, PWideString prefix, PWideString directory, PWideString suffix)
{
	QString t_filename;
	QString t_prefix;
	QString t_directory;
	QString t_suffix;
	copyPWideStringToQString(filename, t_filename);
	copyPWideStringToQString(prefix, t_prefix);
	copyPWideStringToQString(directory, t_directory);
	copyPWideStringToQString(suffix, t_suffix);
	return (bool) ((QTranslator *)handle)->load(*(const QLocale*)locale, t_filename, t_prefix, t_directory, t_suffix);
}

bool QTranslator_load3(QTranslatorH handle, const uchar* data, int len, PWideString directory)
{
	QString t_directory;
	copyPWideStringToQString(directory, t_directory);
	return (bool) ((QTranslator *)handle)->load(data, len, t_directory);
}

