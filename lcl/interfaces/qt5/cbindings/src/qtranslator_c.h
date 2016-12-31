//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QTRANSLATOR_C_H
#define QTRANSLATOR_C_H

#include <QtCore>
#include "pascalbind.h"

C_EXPORT QTranslatorH QTranslator_Create(QObjectH parent);
C_EXPORT void QTranslator_Destroy(QTranslatorH handle);
C_EXPORT void QTranslator_translate(QTranslatorH handle, PWideString retval, const char* context, const char* sourceText, const char* disambiguation, int n);
C_EXPORT bool QTranslator_isEmpty(QTranslatorH handle);
C_EXPORT bool QTranslator_load(QTranslatorH handle, PWideString filename, PWideString directory, PWideString search_delimiters, PWideString suffix);
C_EXPORT bool QTranslator_load2(QTranslatorH handle, const QLocaleH locale, PWideString filename, PWideString prefix, PWideString directory, PWideString suffix);
C_EXPORT bool QTranslator_load3(QTranslatorH handle, const uchar* data, int len, PWideString directory);

#endif
