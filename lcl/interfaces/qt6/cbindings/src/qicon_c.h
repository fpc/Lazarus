//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QICON_C_H
#define QICON_C_H

#include <QtGui>
#include "pascalbind.h"

C_EXPORT QIconH QIcon_Create();
C_EXPORT void QIcon_Destroy(QIconH handle);
C_EXPORT QIconH QIcon_Create2(const QPixmapH pixmap);
C_EXPORT QIconH QIcon_Create3(const QIconH other);
C_EXPORT QIconH QIcon_Create4(PWideString fileName);
C_EXPORT QIconH QIcon_Create5(QIconEngineH engine);
C_EXPORT void QIcon_swap(QIconH handle, QIconH other);
C_EXPORT void QIcon_pixmap(QIconH handle, QPixmapH retval, const QSizeH size, QIcon::Mode mode, QIcon::State state);
C_EXPORT void QIcon_pixmap2(QIconH handle, QPixmapH retval, int w, int h, QIcon::Mode mode, QIcon::State state);
C_EXPORT void QIcon_pixmap3(QIconH handle, QPixmapH retval, int extent, QIcon::Mode mode, QIcon::State state);
C_EXPORT void QIcon_actualSize(QIconH handle, PSize retval, const QSizeH size, QIcon::Mode mode, QIcon::State state);
C_EXPORT void QIcon_name(QIconH handle, PWideString retval);
C_EXPORT void QIcon_paint(QIconH handle, QPainterH painter, PRect rect, unsigned int alignment, QIcon::Mode mode, QIcon::State state);
C_EXPORT void QIcon_paint2(QIconH handle, QPainterH painter, int x, int y, int w, int h, unsigned int alignment, QIcon::Mode mode, QIcon::State state);
C_EXPORT bool QIcon_isMask(QIconH handle);
C_EXPORT void QIcon_setIsMask(QIconH handle, bool isMask);
C_EXPORT bool QIcon_isNull(QIconH handle);
C_EXPORT bool QIcon_isDetached(QIconH handle);
C_EXPORT void QIcon_detach(QIconH handle);
C_EXPORT qint64 QIcon_cacheKey(QIconH handle);
C_EXPORT void QIcon_addPixmap(QIconH handle, const QPixmapH pixmap, QIcon::Mode mode, QIcon::State state);
C_EXPORT void QIcon_addFile(QIconH handle, PWideString fileName, const QSizeH size, QIcon::Mode mode, QIcon::State state);
C_EXPORT void QIcon_availableSizes(QIconH handle, PPtrIntArray retval, QIcon::Mode mode, QIcon::State state);

C_EXPORT void QIcon_fromTheme(QIconH retval, PWideString name, const QIconH fallback);
C_EXPORT void QIcon_fromTheme2(QIconH retval, PWideString name);
C_EXPORT bool QIcon_hasThemeIcon(PWideString name);
C_EXPORT void QIcon_themeSearchPaths(QStringListH retval);
C_EXPORT void QIcon_setThemeSearchPaths(const QStringListH searchpath);
C_EXPORT void QIcon_themeName(PWideString retval);
C_EXPORT void QIcon_setThemeName(PWideString path);
C_EXPORT void QIcon_setFallbackThemeName(PWideString name);
C_EXPORT void QIcon_setFallbackSearchPaths(const QStringListH paths);

#endif
