//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qicon_c.h"

QIconH QIcon_Create()
{
	return (QIconH) new QIcon();
}

void QIcon_Destroy(QIconH handle)
{
	delete (QIcon *)handle;
}

QIconH QIcon_Create2(const QPixmapH pixmap)
{
	return (QIconH) new QIcon(*(const QPixmap*)pixmap);
}

QIconH QIcon_Create3(const QIconH other)
{
	return (QIconH) new QIcon(*(const QIcon*)other);
}

QIconH QIcon_Create4(PWideString fileName)
{
	QString t_fileName;
	copyPWideStringToQString(fileName, t_fileName);
	return (QIconH) new QIcon(t_fileName);
}

QIconH QIcon_Create5(QIconEngineH engine)
{
	return (QIconH) new QIcon((QIconEngine*)engine);
}

void QIcon_swap(QIconH handle, QIconH other)
{
	((QIcon *)handle)->swap(*(QIcon*)other);
}

void QIcon_pixmap(QIconH handle, QPixmapH retval, const QSizeH size, QIcon::Mode mode, QIcon::State state)
{
	*(QPixmap *)retval = ((QIcon *)handle)->pixmap(*(const QSize*)size, mode, state);
}

void QIcon_pixmap2(QIconH handle, QPixmapH retval, int w, int h, QIcon::Mode mode, QIcon::State state)
{
	*(QPixmap *)retval = ((QIcon *)handle)->pixmap(w, h, mode, state);
}

void QIcon_pixmap3(QIconH handle, QPixmapH retval, int extent, QIcon::Mode mode, QIcon::State state)
{
	*(QPixmap *)retval = ((QIcon *)handle)->pixmap(extent, mode, state);
}

void QIcon_actualSize(QIconH handle, PSize retval, const QSizeH size, QIcon::Mode mode, QIcon::State state)
{
	*(QSize *)retval = ((QIcon *)handle)->actualSize(*(const QSize*)size, mode, state);
}

void QIcon_availableSizes(QIconH handle, PPtrIntArray retval, QIcon::Mode mode, QIcon::State state)
{
	QList<QSize> t_retval;
	t_retval = ((QIcon *)handle)->availableSizes(mode, state);
	copyQListTemplateToPtrIntArrayWithNew(t_retval, retval);
}

void QIcon_name(QIconH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QIcon *)handle)->name();
	copyQStringToPWideString(t_retval, retval);
}

void QIcon_paint(QIconH handle, QPainterH painter, PRect rect, unsigned int alignment, QIcon::Mode mode, QIcon::State state)
{
	QRect t_rect;
	copyPRectToQRect(rect, t_rect);
	((QIcon *)handle)->paint((QPainter*)painter, t_rect, (Qt::Alignment)alignment, mode, state);
}

void QIcon_paint2(QIconH handle, QPainterH painter, int x, int y, int w, int h, unsigned int alignment, QIcon::Mode mode, QIcon::State state)
{
	((QIcon *)handle)->paint((QPainter*)painter, x, y, w, h, (Qt::Alignment)alignment, mode, state);
}

bool QIcon_isMask(QIconH handle)
{
	return (bool) ((QIcon *)handle)->isMask();
}

void QIcon_setIsMask(QIconH handle, bool isMask)
{
  ((QIcon *)handle)->setIsMask(isMask);
}

bool QIcon_isNull(QIconH handle)
{
	return (bool) ((QIcon *)handle)->isNull();
}

bool QIcon_isDetached(QIconH handle)
{
	return (bool) ((QIcon *)handle)->isDetached();
}

void QIcon_detach(QIconH handle)
{
	((QIcon *)handle)->detach();
}

qint64 QIcon_cacheKey(QIconH handle)
{
	return (qint64) ((QIcon *)handle)->cacheKey();
}

void QIcon_addPixmap(QIconH handle, const QPixmapH pixmap, QIcon::Mode mode, QIcon::State state)
{
	((QIcon *)handle)->addPixmap(*(const QPixmap*)pixmap, mode, state);
}

void QIcon_addFile(QIconH handle, PWideString fileName, const QSizeH size, QIcon::Mode mode, QIcon::State state)
{
	QString t_fileName;
	copyPWideStringToQString(fileName, t_fileName);
	((QIcon *)handle)->addFile(t_fileName, *(const QSize*)size, mode, state);
}

void QIcon_fromTheme(QIconH retval, PWideString name, const QIconH fallback)
{
	QString t_name;
	copyPWideStringToQString(name, t_name);
	*(QIcon *)retval = QIcon::fromTheme(t_name, *(const QIcon*)fallback);
}

void QIcon_fromTheme(QIconH retval, PWideString name)
{
	QString t_name;
	copyPWideStringToQString(name, t_name);
	*(QIcon *)retval = QIcon::fromTheme(t_name);
}

bool QIcon_hasThemeIcon(PWideString name)
{
	QString t_name;
	copyPWideStringToQString(name, t_name);
	return (bool) QIcon::hasThemeIcon(t_name);
}

void QIcon_themeSearchPaths(QStringListH retval)
{
	*(QStringList *)retval = QIcon::themeSearchPaths();
}

void QIcon_setThemeSearchPaths(const QStringListH searchpath)
{
	QIcon::setThemeSearchPaths(*(const QStringList*)searchpath);
}

void QIcon_setFallbackThemeName(PWideString name)
{
	QString t_name;
	copyPWideStringToQString(name, t_name);
  QIcon::setFallbackThemeName(t_name);
}

void QIcon_setFallbackSearchPaths(const QStringListH paths)
{
  QIcon::setFallbackSearchPaths(*(const QStringList*)paths);
}

void QIcon_themeName(PWideString retval)
{
	QString t_retval;
	t_retval = QIcon::themeName();
	copyQStringToPWideString(t_retval, retval);
}

void QIcon_setThemeName(PWideString path)
{
	QString t_path;
	copyPWideStringToQString(path, t_path);
	QIcon::setThemeName(t_path);
}

