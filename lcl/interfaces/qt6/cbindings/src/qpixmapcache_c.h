//******************************************************************************
//  Copyright (c) 2014-2022 by Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QPIXMAPCACHE_C_H
#define QPIXMAPCACHE_C_H

#include <QtGlobal>
#include <QtGui>
#include "pascalbind.h"

C_EXPORT void QPixmapCache_clear();
C_EXPORT int QPixmapCache_cacheLimit();
C_EXPORT void QPixmapCache_setCacheLimit(int limit);
C_EXPORT bool QPixmapCache_find(const PWideString key, QPixmapH retval);
C_EXPORT bool QPixmapCache_insert(const PWideString key, QPixmapH pixmap);
C_EXPORT void QPixmapCache_remove(const PWideString key);
#endif
