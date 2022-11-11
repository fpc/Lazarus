//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QWEBHISTORY_C_H
#define QWEBHISTORY_C_H

#include <QtWebKitWidgets>
#include "pascalbind.h"

C_EXPORT QWebHistoryItemH QWebHistoryItem_Create(const QWebHistoryItemH other);
C_EXPORT void QWebHistoryItem_Destroy(QWebHistoryItemH handle);
C_EXPORT void QWebHistoryItem_originalUrl(QWebHistoryItemH handle, QUrlH retval);
C_EXPORT void QWebHistoryItem_url(QWebHistoryItemH handle, QUrlH retval);
C_EXPORT void QWebHistoryItem_title(QWebHistoryItemH handle, PWideString retval);
C_EXPORT void QWebHistoryItem_lastVisited(QWebHistoryItemH handle, QDateTimeH retval);
C_EXPORT void QWebHistoryItem_icon(QWebHistoryItemH handle, QIconH retval);
C_EXPORT void QWebHistoryItem_userData(QWebHistoryItemH handle, QVariantH retval);
C_EXPORT void QWebHistoryItem_setUserData(QWebHistoryItemH handle, const QVariantH userData);
C_EXPORT bool QWebHistoryItem_isValid(QWebHistoryItemH handle);
C_EXPORT void QWebHistory_clear(QWebHistoryH handle);
C_EXPORT void QWebHistory_items(QWebHistoryH handle, PPtrIntArray retval);
C_EXPORT void QWebHistory_backItems(QWebHistoryH handle, PPtrIntArray retval, int maxItems);
C_EXPORT void QWebHistory_forwardItems(QWebHistoryH handle, PPtrIntArray retval, int maxItems);
C_EXPORT bool QWebHistory_canGoBack(QWebHistoryH handle);
C_EXPORT bool QWebHistory_canGoForward(QWebHistoryH handle);
C_EXPORT void QWebHistory_back(QWebHistoryH handle);
C_EXPORT void QWebHistory_forward(QWebHistoryH handle);
C_EXPORT void QWebHistory_goToItem(QWebHistoryH handle, const QWebHistoryItemH item);
C_EXPORT void QWebHistory_backItem(QWebHistoryH handle, QWebHistoryItemH retval);
C_EXPORT void QWebHistory_currentItem(QWebHistoryH handle, QWebHistoryItemH retval);
C_EXPORT void QWebHistory_forwardItem(QWebHistoryH handle, QWebHistoryItemH retval);
C_EXPORT void QWebHistory_itemAt(QWebHistoryH handle, QWebHistoryItemH retval, int i);
C_EXPORT int QWebHistory_currentItemIndex(QWebHistoryH handle);
C_EXPORT int QWebHistory_count(QWebHistoryH handle);
C_EXPORT int QWebHistory_maximumItemCount(QWebHistoryH handle);
C_EXPORT void QWebHistory_setMaximumItemCount(QWebHistoryH handle, int count);

#endif
