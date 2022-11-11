//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qstringlist_c.h"

QStringListH QStringList_Create()
{
	return (QStringListH) new QStringList();
}

void QStringList_Destroy(QStringListH handle)
{
	delete (QStringList *)handle;
}

QStringListH QStringList_Create2(PWideString i)
{
	QString t_i;
	copyPWideStringToQString(i, t_i);
	return (QStringListH) new QStringList(t_i);
}

QStringListH QStringList_Create3(const QStringListH l)
{
	return (QStringListH) new QStringList(*(const QStringList*)l);
}

void QStringList_sort(QStringListH handle, Qt::CaseSensitivity cs)
{
	((QStringList *)handle)->sort(cs);
}

int QStringList_removeDuplicates(QStringListH handle)
{
	return (int) ((QStringList *)handle)->removeDuplicates();
}

void QStringList_join(QStringListH handle, PWideString retval, PWideString sep)
{
	QString t_retval;
	QString t_sep;
	copyPWideStringToQString(sep, t_sep);
	t_retval = ((QStringList *)handle)->join(t_sep);
	copyQStringToPWideString(t_retval, retval);
}

void QStringList_join2(QStringListH handle, PWideString retval, PWideChar sep)
{
	QString t_retval;
	t_retval = ((QStringList *)handle)->join(*(QChar *)sep);
	copyQStringToPWideString(t_retval, retval);
}

void QStringList_filter(QStringListH handle, QStringListH retval, PWideString str, Qt::CaseSensitivity cs)
{
	QString t_str;
	copyPWideStringToQString(str, t_str);
	*(QStringList *)retval = ((QStringList *)handle)->filter(t_str, cs);
}

bool QStringList_contains(QStringListH handle, PWideString str, Qt::CaseSensitivity cs)
{
	QString t_str;
	copyPWideStringToQString(str, t_str);
	return (bool) ((QStringList *)handle)->contains(t_str, cs);
}

QStringListH QStringList_replaceInStrings(QStringListH handle, PWideString before, PWideString after, Qt::CaseSensitivity cs)
{
	QString t_before;
	QString t_after;
	copyPWideStringToQString(before, t_before);
	copyPWideStringToQString(after, t_after);
	return (QStringListH) &((QStringList *)handle)->replaceInStrings(t_before, t_after, cs);
}

void QStringList_filter3(QStringListH handle, QStringListH retval, const QRegularExpressionH re)
{
	*(QStringList *)retval = ((QStringList *)handle)->filter(*(const QRegularExpression*)re);
}

QStringListH QStringList_replaceInStrings3(QStringListH handle, const QRegularExpressionH re, PWideString after)
{
	QString t_after;
	copyPWideStringToQString(after, t_after);
	return (QStringListH) &((QStringList *)handle)->replaceInStrings(*(const QRegularExpression*)re, t_after);
}

int QStringList_indexOf3(QStringListH handle, const QRegularExpressionH re, int from)
{
	return (int) ((QStringList *)handle)->indexOf(*(const QRegularExpression*)re, from);
}

int QStringList_lastIndexOf3(QStringListH handle, const QRegularExpressionH re, int from)
{
	return (int) ((QStringList *)handle)->lastIndexOf(*(const QRegularExpression*)re, from);
}

int QStringList_size(QStringListH handle)
{
	return (int) ((QStringList *)handle)->size();
}

bool QStringList_isEmpty(QStringListH handle)
{
	return (bool) ((QStringList *)handle)->isEmpty();
}

void QStringList_clear(QStringListH handle)
{
	((QStringList *)handle)->clear();
}

void QStringList_at(QStringListH handle, PWideString retval, int i)
{
	QString t_retval;
	t_retval = ((QStringList *)handle)->at(i);
	copyQStringToPWideString(t_retval, retval);
}

void QStringList_append(QStringListH handle, PWideString s)
{
	QString t_s;
	copyPWideStringToQString(s, t_s);
	((QStringList *)handle)->append(t_s);
}

void QStringList_prepend(QStringListH handle, PWideString s)
{
	QString t_s;
	copyPWideStringToQString(s, t_s);
	((QStringList *)handle)->prepend(t_s);
}

void QStringList_insert(QStringListH handle, int i, PWideString s)
{
	QString t_s;
	copyPWideStringToQString(s, t_s);
	((QStringList *)handle)->insert(i, t_s);
}

void QStringList_replace(QStringListH handle, int i, PWideString s)
{
	QString t_s;
	copyPWideStringToQString(s, t_s);
	((QStringList *)handle)->replace(i, t_s);
}

void QStringList_removeAt(QStringListH handle, int i)
{
	((QStringList *)handle)->removeAt(i);
}

int QStringList_removeAll(QStringListH handle, PWideString s)
{
	QString t_s;
	copyPWideStringToQString(s, t_s);
	return (int) ((QStringList *)handle)->removeAll(t_s);
}

void QStringList_takeAt(QStringListH handle, PWideString retval, int i)
{
	QString t_retval;
	t_retval = ((QStringList *)handle)->takeAt(i);
	copyQStringToPWideString(t_retval, retval);
}

void QStringList_takeFirst(QStringListH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QStringList *)handle)->takeFirst();
	copyQStringToPWideString(t_retval, retval);
}

void QStringList_takeLast(QStringListH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QStringList *)handle)->takeLast();
	copyQStringToPWideString(t_retval, retval);
}

void QStringList_move(QStringListH handle, int from, int to)
{
	((QStringList *)handle)->move(from, to);
}

void QStringList_swapItemsAt(QStringListH handle, int i, int j)
{
	((QStringList *)handle)->swapItemsAt(i, j);
}

