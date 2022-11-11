//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QSTRINGLIST_C_H
#define QSTRINGLIST_C_H

#include <QtCore>
#include "pascalbind.h"

C_EXPORT QStringListH QStringList_Create();
C_EXPORT void QStringList_Destroy(QStringListH handle);
C_EXPORT QStringListH QStringList_Create2(PWideString i);
C_EXPORT QStringListH QStringList_Create3(const QStringListH l);
C_EXPORT void QStringList_sort(QStringListH handle, Qt::CaseSensitivity cs);
C_EXPORT int QStringList_removeDuplicates(QStringListH handle);
C_EXPORT void QStringList_join(QStringListH handle, PWideString retval, PWideString sep);
C_EXPORT void QStringList_join2(QStringListH handle, PWideString retval, PWideChar sep);
C_EXPORT void QStringList_filter(QStringListH handle, QStringListH retval, PWideString str, Qt::CaseSensitivity cs);
C_EXPORT bool QStringList_contains(QStringListH handle, PWideString str, Qt::CaseSensitivity cs);
C_EXPORT QStringListH QStringList_replaceInStrings(QStringListH handle, PWideString before, PWideString after, Qt::CaseSensitivity cs);
C_EXPORT void QStringList_filter3(QStringListH handle, QStringListH retval, const QRegularExpressionH re);
C_EXPORT QStringListH QStringList_replaceInStrings3(QStringListH handle, const QRegularExpressionH re, PWideString after);
C_EXPORT int QStringList_indexOf3(QStringListH handle, const QRegularExpressionH re, int from);
C_EXPORT int QStringList_lastIndexOf3(QStringListH handle, const QRegularExpressionH re, int from);
C_EXPORT int QStringList_size(QStringListH handle);
C_EXPORT bool QStringList_isEmpty(QStringListH handle);
C_EXPORT void QStringList_clear(QStringListH handle);
C_EXPORT void QStringList_at(QStringListH handle, PWideString retval, int i);
C_EXPORT void QStringList_append(QStringListH handle, PWideString s);
C_EXPORT void QStringList_prepend(QStringListH handle, PWideString s);
C_EXPORT void QStringList_insert(QStringListH handle, int i, PWideString s);
C_EXPORT void QStringList_replace(QStringListH handle, int i, PWideString s);
C_EXPORT void QStringList_removeAt(QStringListH handle, int i);
C_EXPORT int QStringList_removeAll(QStringListH handle, PWideString s);
C_EXPORT void QStringList_takeAt(QStringListH handle, PWideString retval, int i);
C_EXPORT void QStringList_takeFirst(QStringListH handle, PWideString retval);
C_EXPORT void QStringList_takeLast(QStringListH handle, PWideString retval);
C_EXPORT void QStringList_move(QStringListH handle, int from, int to);
C_EXPORT void QStringList_swapItemsAt(QStringListH handle, int i, int j);

#endif
