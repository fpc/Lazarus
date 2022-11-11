//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qwebsettings_c.h"

QWebSettingsH QWebSettings_globalSettings()
{
	return (QWebSettingsH) QWebSettings::globalSettings();
}

void QWebSettings_setFontFamily(QWebSettingsH handle, QWebSettings::FontFamily which, PWideString family)
{
	QString t_family;
	copyPWideStringToQString(family, t_family);
	((QWebSettings *)handle)->setFontFamily(which, t_family);
}

void QWebSettings_fontFamily(QWebSettingsH handle, PWideString retval, QWebSettings::FontFamily which)
{
	QString t_retval;
	t_retval = ((QWebSettings *)handle)->fontFamily(which);
	copyQStringToPWideString(t_retval, retval);
}

void QWebSettings_resetFontFamily(QWebSettingsH handle, QWebSettings::FontFamily which)
{
	((QWebSettings *)handle)->resetFontFamily(which);
}

void QWebSettings_setFontSize(QWebSettingsH handle, QWebSettings::FontSize type, int size)
{
	((QWebSettings *)handle)->setFontSize(type, size);
}

int QWebSettings_fontSize(QWebSettingsH handle, QWebSettings::FontSize type)
{
	return (int) ((QWebSettings *)handle)->fontSize(type);
}

void QWebSettings_resetFontSize(QWebSettingsH handle, QWebSettings::FontSize type)
{
	((QWebSettings *)handle)->resetFontSize(type);
}

void QWebSettings_setAttribute(QWebSettingsH handle, QWebSettings::WebAttribute attr, bool on)
{
	((QWebSettings *)handle)->setAttribute(attr, on);
}

bool QWebSettings_testAttribute(QWebSettingsH handle, QWebSettings::WebAttribute attr)
{
	return (bool) ((QWebSettings *)handle)->testAttribute(attr);
}

void QWebSettings_resetAttribute(QWebSettingsH handle, QWebSettings::WebAttribute attr)
{
	((QWebSettings *)handle)->resetAttribute(attr);
}

void QWebSettings_setUserStyleSheetUrl(QWebSettingsH handle, const QUrlH location)
{
	((QWebSettings *)handle)->setUserStyleSheetUrl(*(const QUrl*)location);
}

void QWebSettings_userStyleSheetUrl(QWebSettingsH handle, QUrlH retval)
{
	*(QUrl *)retval = ((QWebSettings *)handle)->userStyleSheetUrl();
}

void QWebSettings_setDefaultTextEncoding(QWebSettingsH handle, PWideString encoding)
{
	QString t_encoding;
	copyPWideStringToQString(encoding, t_encoding);
	((QWebSettings *)handle)->setDefaultTextEncoding(t_encoding);
}

void QWebSettings_defaultTextEncoding(QWebSettingsH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QWebSettings *)handle)->defaultTextEncoding();
	copyQStringToPWideString(t_retval, retval);
}

void QWebSettings_setIconDatabasePath(PWideString location)
{
	QString t_location;
	copyPWideStringToQString(location, t_location);
	QWebSettings::setIconDatabasePath(t_location);
}

void QWebSettings_iconDatabasePath(PWideString retval)
{
	QString t_retval;
	t_retval = QWebSettings::iconDatabasePath();
	copyQStringToPWideString(t_retval, retval);
}

void QWebSettings_clearIconDatabase()
{
	QWebSettings::clearIconDatabase();
}

void QWebSettings_iconForUrl(QIconH retval, const QUrlH url)
{
	*(QIcon *)retval = QWebSettings::iconForUrl(*(const QUrl*)url);
}

void QWebSettings_setWebGraphic(QWebSettings::WebGraphic type, const QPixmapH graphic)
{
	QWebSettings::setWebGraphic(type, *(const QPixmap*)graphic);
}

void QWebSettings_webGraphic(QPixmapH retval, QWebSettings::WebGraphic type)
{
	*(QPixmap *)retval = QWebSettings::webGraphic(type);
}

void QWebSettings_setMaximumPagesInCache(int pages)
{
	QWebSettings::setMaximumPagesInCache(pages);
}

int QWebSettings_maximumPagesInCache()
{
	return (int) QWebSettings::maximumPagesInCache();
}

void QWebSettings_setObjectCacheCapacities(int cacheMinDeadCapacity, int cacheMaxDead, int totalCapacity)
{
	QWebSettings::setObjectCacheCapacities(cacheMinDeadCapacity, cacheMaxDead, totalCapacity);
}

void QWebSettings_setOfflineStoragePath(PWideString path)
{
	QString t_path;
	copyPWideStringToQString(path, t_path);
	QWebSettings::setOfflineStoragePath(t_path);
}

void QWebSettings_offlineStoragePath(PWideString retval)
{
	QString t_retval;
	t_retval = QWebSettings::offlineStoragePath();
	copyQStringToPWideString(t_retval, retval);
}

void QWebSettings_setOfflineStorageDefaultQuota(qint64 maximumSize)
{
	QWebSettings::setOfflineStorageDefaultQuota(maximumSize);
}

qint64 QWebSettings_offlineStorageDefaultQuota()
{
	return (qint64) QWebSettings::offlineStorageDefaultQuota();
}

void QWebSettings_setOfflineWebApplicationCachePath(PWideString path)
{
	QString t_path;
	copyPWideStringToQString(path, t_path);
	QWebSettings::setOfflineWebApplicationCachePath(t_path);
}

void QWebSettings_offlineWebApplicationCachePath(PWideString retval)
{
	QString t_retval;
	t_retval = QWebSettings::offlineWebApplicationCachePath();
	copyQStringToPWideString(t_retval, retval);
}

void QWebSettings_setOfflineWebApplicationCacheQuota(qint64 maximumSize)
{
	QWebSettings::setOfflineWebApplicationCacheQuota(maximumSize);
}

qint64 QWebSettings_offlineWebApplicationCacheQuota()
{
	return (qint64) QWebSettings::offlineWebApplicationCacheQuota();
}

void QWebSettings_setLocalStoragePath(QWebSettingsH handle, PWideString path)
{
	QString t_path;
	copyPWideStringToQString(path, t_path);
	((QWebSettings *)handle)->setLocalStoragePath(t_path);
}

void QWebSettings_localStoragePath(QWebSettingsH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QWebSettings *)handle)->localStoragePath();
	copyQStringToPWideString(t_retval, retval);
}

void QWebSettings_clearMemoryCaches()
{
	QWebSettings::clearMemoryCaches();
}

void QWebSettings_enablePersistentStorage(PWideString path)
{
	QString t_path;
	copyPWideStringToQString(path, t_path);
	QWebSettings::enablePersistentStorage(t_path);
}

void QWebSettings_setThirdPartyCookiePolicy(QWebSettingsH handle, QWebSettings::ThirdPartyCookiePolicy AnonParam1)
{
	((QWebSettings *)handle)->setThirdPartyCookiePolicy(AnonParam1);
}

QWebSettings::ThirdPartyCookiePolicy QWebSettings_thirdPartyCookiePolicy(QWebSettingsH handle)
{
	return (QWebSettings::ThirdPartyCookiePolicy) ((QWebSettings *)handle)->thirdPartyCookiePolicy();
}

void QWebSettings_setCSSMediaType(QWebSettingsH handle, PWideString AnonParam1)
{
	QString t_AnonParam1;
	copyPWideStringToQString(AnonParam1, t_AnonParam1);
	((QWebSettings *)handle)->setCSSMediaType(t_AnonParam1);
}

void QWebSettings_cssMediaType(QWebSettingsH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QWebSettings *)handle)->cssMediaType();
	copyQStringToPWideString(t_retval, retval);
}

