//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qwebhistory_c.h"

QWebHistoryItemH QWebHistoryItem_Create(const QWebHistoryItemH other)
{
	return (QWebHistoryItemH) new QWebHistoryItem(*(const QWebHistoryItem*)other);
}

void QWebHistoryItem_Destroy(QWebHistoryItemH handle)
{
	delete (QWebHistoryItem *)handle;
}

void QWebHistoryItem_originalUrl(QWebHistoryItemH handle, QUrlH retval)
{
	*(QUrl *)retval = ((QWebHistoryItem *)handle)->originalUrl();
}

void QWebHistoryItem_url(QWebHistoryItemH handle, QUrlH retval)
{
	*(QUrl *)retval = ((QWebHistoryItem *)handle)->url();
}

void QWebHistoryItem_title(QWebHistoryItemH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QWebHistoryItem *)handle)->title();
	copyQStringToPWideString(t_retval, retval);
}

void QWebHistoryItem_lastVisited(QWebHistoryItemH handle, QDateTimeH retval)
{
	*(QDateTime *)retval = ((QWebHistoryItem *)handle)->lastVisited();
}

void QWebHistoryItem_icon(QWebHistoryItemH handle, QIconH retval)
{
	*(QIcon *)retval = ((QWebHistoryItem *)handle)->icon();
}

void QWebHistoryItem_userData(QWebHistoryItemH handle, QVariantH retval)
{
	*(QVariant *)retval = ((QWebHistoryItem *)handle)->userData();
}

void QWebHistoryItem_setUserData(QWebHistoryItemH handle, const QVariantH userData)
{
	((QWebHistoryItem *)handle)->setUserData(*(const QVariant*)userData);
}

bool QWebHistoryItem_isValid(QWebHistoryItemH handle)
{
	return (bool) ((QWebHistoryItem *)handle)->isValid();
}

void QWebHistory_clear(QWebHistoryH handle)
{
	((QWebHistory *)handle)->clear();
}

void QWebHistory_items(QWebHistoryH handle, PPtrIntArray retval)
{
	QList<QWebHistoryItem> t_retval;
	t_retval = ((QWebHistory *)handle)->items();
	copyQListTemplateToPtrIntArrayWithNew(t_retval, retval);
}

void QWebHistory_backItems(QWebHistoryH handle, PPtrIntArray retval, int maxItems)
{
	QList<QWebHistoryItem> t_retval;
	t_retval = ((QWebHistory *)handle)->backItems(maxItems);
	copyQListTemplateToPtrIntArrayWithNew(t_retval, retval);
}

void QWebHistory_forwardItems(QWebHistoryH handle, PPtrIntArray retval, int maxItems)
{
	QList<QWebHistoryItem> t_retval;
	t_retval = ((QWebHistory *)handle)->forwardItems(maxItems);
	copyQListTemplateToPtrIntArrayWithNew(t_retval, retval);
}

bool QWebHistory_canGoBack(QWebHistoryH handle)
{
	return (bool) ((QWebHistory *)handle)->canGoBack();
}

bool QWebHistory_canGoForward(QWebHistoryH handle)
{
	return (bool) ((QWebHistory *)handle)->canGoForward();
}

void QWebHistory_back(QWebHistoryH handle)
{
	((QWebHistory *)handle)->back();
}

void QWebHistory_forward(QWebHistoryH handle)
{
	((QWebHistory *)handle)->forward();
}

void QWebHistory_goToItem(QWebHistoryH handle, const QWebHistoryItemH item)
{
	((QWebHistory *)handle)->goToItem(*(const QWebHistoryItem*)item);
}

void QWebHistory_backItem(QWebHistoryH handle, QWebHistoryItemH retval)
{
	*(QWebHistoryItem *)retval = ((QWebHistory *)handle)->backItem();
}

void QWebHistory_currentItem(QWebHistoryH handle, QWebHistoryItemH retval)
{
	*(QWebHistoryItem *)retval = ((QWebHistory *)handle)->currentItem();
}

void QWebHistory_forwardItem(QWebHistoryH handle, QWebHistoryItemH retval)
{
	*(QWebHistoryItem *)retval = ((QWebHistory *)handle)->forwardItem();
}

void QWebHistory_itemAt(QWebHistoryH handle, QWebHistoryItemH retval, int i)
{
	*(QWebHistoryItem *)retval = ((QWebHistory *)handle)->itemAt(i);
}

int QWebHistory_currentItemIndex(QWebHistoryH handle)
{
	return (int) ((QWebHistory *)handle)->currentItemIndex();
}

int QWebHistory_count(QWebHistoryH handle)
{
	return (int) ((QWebHistory *)handle)->count();
}

int QWebHistory_maximumItemCount(QWebHistoryH handle)
{
	return (int) ((QWebHistory *)handle)->maximumItemCount();
}

void QWebHistory_setMaximumItemCount(QWebHistoryH handle, int count)
{
	((QWebHistory *)handle)->setMaximumItemCount(count);
}

