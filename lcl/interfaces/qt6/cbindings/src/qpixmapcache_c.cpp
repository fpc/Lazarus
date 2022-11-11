//******************************************************************************
//  Copyright (c) 2014-2022 by Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************

#include "qpixmapcache_c.h"

void QPixmapCache_clear()
{
  QPixmapCache::clear();
}

int QPixmapCache_cacheLimit()
{
  return (int) QPixmapCache::cacheLimit();
}

void QPixmapCache_setCacheLimit(int limit)
{
  QPixmapCache::setCacheLimit(limit);
}

bool QPixmapCache_find(const PWideString key, QPixmapH retval)
{
  QString t_key;
  copyPWideStringToQString(key, t_key);
  return (bool) QPixmapCache::find(t_key, (QPixmap *)retval);
}

bool QPixmapCache_insert(const PWideString key, QPixmapH pixmap)
{
  QString t_key;
  copyPWideStringToQString(key, t_key);
  return (bool) QPixmapCache::insert(t_key, *(QPixmap *)pixmap);
}

void QPixmapCache_remove(const PWideString key)
{
  QString t_key;
  copyPWideStringToQString(key, t_key);
  QPixmapCache::remove(t_key);
}

