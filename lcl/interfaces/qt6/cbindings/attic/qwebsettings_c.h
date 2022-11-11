//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QWEBSETTINGS_C_H
#define QWEBSETTINGS_C_H

#include <QtWebKitWidgets>
#include "pascalbind.h"

C_EXPORT QWebSettingsH QWebSettings_globalSettings();
C_EXPORT void QWebSettings_setFontFamily(QWebSettingsH handle, QWebSettings::FontFamily which, PWideString family);
C_EXPORT void QWebSettings_fontFamily(QWebSettingsH handle, PWideString retval, QWebSettings::FontFamily which);
C_EXPORT void QWebSettings_resetFontFamily(QWebSettingsH handle, QWebSettings::FontFamily which);
C_EXPORT void QWebSettings_setFontSize(QWebSettingsH handle, QWebSettings::FontSize type, int size);
C_EXPORT int QWebSettings_fontSize(QWebSettingsH handle, QWebSettings::FontSize type);
C_EXPORT void QWebSettings_resetFontSize(QWebSettingsH handle, QWebSettings::FontSize type);
C_EXPORT void QWebSettings_setAttribute(QWebSettingsH handle, QWebSettings::WebAttribute attr, bool on);
C_EXPORT bool QWebSettings_testAttribute(QWebSettingsH handle, QWebSettings::WebAttribute attr);
C_EXPORT void QWebSettings_resetAttribute(QWebSettingsH handle, QWebSettings::WebAttribute attr);
C_EXPORT void QWebSettings_setUserStyleSheetUrl(QWebSettingsH handle, const QUrlH location);
C_EXPORT void QWebSettings_userStyleSheetUrl(QWebSettingsH handle, QUrlH retval);
C_EXPORT void QWebSettings_setDefaultTextEncoding(QWebSettingsH handle, PWideString encoding);
C_EXPORT void QWebSettings_defaultTextEncoding(QWebSettingsH handle, PWideString retval);
C_EXPORT void QWebSettings_setIconDatabasePath(PWideString location);
C_EXPORT void QWebSettings_iconDatabasePath(PWideString retval);
C_EXPORT void QWebSettings_clearIconDatabase();
C_EXPORT void QWebSettings_iconForUrl(QIconH retval, const QUrlH url);
C_EXPORT void QWebSettings_setWebGraphic(QWebSettings::WebGraphic type, const QPixmapH graphic);
C_EXPORT void QWebSettings_webGraphic(QPixmapH retval, QWebSettings::WebGraphic type);
C_EXPORT void QWebSettings_setMaximumPagesInCache(int pages);
C_EXPORT int QWebSettings_maximumPagesInCache();
C_EXPORT void QWebSettings_setObjectCacheCapacities(int cacheMinDeadCapacity, int cacheMaxDead, int totalCapacity);
C_EXPORT void QWebSettings_setOfflineStoragePath(PWideString path);
C_EXPORT void QWebSettings_offlineStoragePath(PWideString retval);
C_EXPORT void QWebSettings_setOfflineStorageDefaultQuota(qint64 maximumSize);
C_EXPORT qint64 QWebSettings_offlineStorageDefaultQuota();
C_EXPORT void QWebSettings_setOfflineWebApplicationCachePath(PWideString path);
C_EXPORT void QWebSettings_offlineWebApplicationCachePath(PWideString retval);
C_EXPORT void QWebSettings_setOfflineWebApplicationCacheQuota(qint64 maximumSize);
C_EXPORT qint64 QWebSettings_offlineWebApplicationCacheQuota();
C_EXPORT void QWebSettings_setLocalStoragePath(QWebSettingsH handle, PWideString path);
C_EXPORT void QWebSettings_localStoragePath(QWebSettingsH handle, PWideString retval);
C_EXPORT void QWebSettings_clearMemoryCaches();
C_EXPORT void QWebSettings_enablePersistentStorage(PWideString path);
C_EXPORT void QWebSettings_setThirdPartyCookiePolicy(QWebSettingsH handle, QWebSettings::ThirdPartyCookiePolicy AnonParam1);
C_EXPORT QWebSettings::ThirdPartyCookiePolicy QWebSettings_thirdPartyCookiePolicy(QWebSettingsH handle);
C_EXPORT void QWebSettings_setCSSMediaType(QWebSettingsH handle, PWideString AnonParam1);
C_EXPORT void QWebSettings_cssMediaType(QWebSettingsH handle, PWideString retval);

#endif
