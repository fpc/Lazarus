//******************************************************************************
//  Copyright (c) 2017 by Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qwindow_c.h"

QWindowH QWindow_Create(QWindowH parent)
{
  return (QWindowH) new QWindow((QWindow*)parent);
}

QWindowH QWindow_Create2(QScreenH screen)
{
  return (QWindowH) new QWindow((QScreen*)screen);
}

void QWindow_Destroy(QWindowH handle)
{
  delete (QWindow *)handle;
}

void QWindow_setSurfaceType(QWindowH handle, QSurface::SurfaceType surfaceType)
{
  ((QWindow *)handle)->setSurfaceType((QSurface::SurfaceType) surfaceType);
}

QSurface::SurfaceType QWindow_surfaceType(QWindowH handle)
{
  return (QSurface::SurfaceType) ((QWindow *)handle)->surfaceType();
}

bool QWindow_isVisible(QWindowH handle)
{
  return (bool) ((QWindow *)handle)->isVisible();
}

QWindow::Visibility QWindow_visibility(QWindowH handle)
{
  return (QWindow::Visibility) ((QWindow *)handle)->visibility();
}

void QWindow_setVisibility(QWindowH handle, QWindow::Visibility v)
{
  ((QWindow *)handle)->setVisibility((QWindow::Visibility) v);
}

void QWindow_createPlatformResources(QWindowH handle) /* void create(); */
{
  ((QWindow *)handle)->create();
}

void QWindow_destroyPlatformResources(QWindowH handle) /* void destroy(); */
{
  ((QWindow *)handle)->destroy();
}

WId QWindow_winId(QWindowH handle)
{
  return (WId) ((QWindow *)handle)->winId();
}

QWindowH QWindow_parent(QWindowH handle)
{
  return (QWindowH) ((QWindow *)handle)->parent();
}

void QWindow_setParent(QWindowH handle, QWindowH parent)
{
	((QWindow *)handle)->setParent((QWindow*)parent);
}

bool QWindow_isTopLevel(QWindowH handle)
{
	return (bool) ((QWindow *)handle)->isTopLevel();
}


bool QWindow_isModal(QWindowH handle)
{
	return (bool) ((QWindow *)handle)->isModal();
}

Qt::WindowModality QWindow_modality(QWindowH handle)
{
	return (Qt::WindowModality) ((QWindow *)handle)->modality();
}

void QWindow_setModality(QWindowH handle, Qt::WindowModality windowModality)
{
	((QWindow *)handle)->setModality((Qt::WindowModality)windowModality);
}

void QWindow_setFormat(QWindowH handle, const QSurfaceFormatH format)
{
  ((QWindow *)handle)->setFormat(*(const QSurfaceFormat*) format);
}

void QWindow_format(QWindowH handle, QSurfaceFormatH retval)
{
  *(QSurfaceFormat*) retval = ((QWindow *)handle)->format();
}

void QWindow_requestedFormat(QWindowH handle, QSurfaceFormatH retval)
{
  *(QSurfaceFormat*) retval = ((QWindow *)handle)->requestedFormat();
}

void QWindow_setFlags(QWindowH handle, unsigned int flags)
{
  ((QWindow *)handle)->setFlags((Qt::WindowFlags)flags);
}

unsigned int QWindow_flags(QWindowH handle)
{
  return (unsigned int) ((QWindow *)handle)->flags();
}

Qt::WindowType QWindow_type(QWindowH handle)
{
  return (Qt::WindowType) ((QWindow *)handle)->type();
}

void QWindow_title(QWindowH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QWindow *)handle)->title();
	copyQStringToPWideString(t_retval, retval);
}

void QWindow_setOpacity(QWindowH handle, qreal level)
{
	((QWindow *)handle)->setOpacity(level);
}

qreal QWindow_opacity(QWindowH handle)
{
  return (qreal) ((QWindow *)handle)->opacity();
}

void QWindow_setMask(QWindowH handle, const QRegionH AnonParam1)
{
	((QWindow *)handle)->setMask(*(const QRegion*)AnonParam1);
}

void QWindow_mask(QWindowH handle, QRegionH retval)
{
	*(QRegion *)retval = ((QWindow *)handle)->mask();
}

bool QWindow_isActiveWindow(QWindowH handle)
{
	return (bool) ((QWindow *)handle)->isActive();
}

void QWindow_reportContentOrientationChange(QWindowH handle, Qt::ScreenOrientation orientation)
{
  ((QWindow *)handle)->reportContentOrientationChange((Qt::ScreenOrientation) orientation);
}

Qt::ScreenOrientation QWindow_contentOrientation(QWindowH handle)
{
  return (Qt::ScreenOrientation) ((QWindow *)handle)->contentOrientation();
}

qreal QWindow_devicePixelRatio(QWindowH handle)
{
  return (qreal) ((QWindow *)handle)->devicePixelRatio();
}

unsigned int QWindow_windowState(QWindowH handle)
{
	return (unsigned int) ((QWindow *)handle)->windowState();
}

void QWindow_setWindowState(QWindowH handle, unsigned int state)
{
	((QWindow *)handle)->setWindowState((Qt::WindowState)state);
}

void QWindow_setTransientParent(QWindowH handle, QWindowH parent)
{
  ((QWindow *)handle)->setTransientParent((QWindow*)parent);
}

QWindowH QWindow_transientParent(QWindowH handle)
{
	return (QWindowH) ((QWindow *)handle)->transientParent();
}

bool QWindow_isAncestorOf(QWindowH handle, const QWindowH child, QWindow::AncestorMode mode)
{
  return (bool) ((QWindow *)handle)->isAncestorOf((QWindow *)child, (QWindow::AncestorMode) mode);
}

bool QWindow_isExposed(QWindowH handle)
{
  return (bool) ((QWindow *)handle)->isExposed();
}

int QWindow_minimumWidth(QWindowH handle)
{
	return (int) ((QWindow *)handle)->minimumWidth();
}

int QWindow_minimumHeight(QWindowH handle)
{
	return (int) ((QWindow *)handle)->minimumHeight();
}

int QWindow_maximumWidth(QWindowH handle)
{
	return (int) ((QWindow *)handle)->maximumWidth();
}

int QWindow_maximumHeight(QWindowH handle)
{
	return (int) ((QWindow *)handle)->maximumHeight();
}

void QWindow_minimumSize(QWindowH handle, PSize retval)
{
	*(QSize *)retval = ((QWindow *)handle)->minimumSize();
}

void QWindow_maximumSize(QWindowH handle, PSize retval)
{
	*(QSize *)retval = ((QWindow *)handle)->maximumSize();
}

void QWindow_baseSize(QWindowH handle, PSize retval)
{
	*(QSize *)retval = ((QWindow *)handle)->baseSize();
}

void QWindow_sizeIncrement(QWindowH handle, PSize retval)
{
	*(QSize *)retval = ((QWindow *)handle)->sizeIncrement();
}

void QWindow_setMinimumSize(QWindowH handle, const QSizeH AnonParam1)
{
	((QWindow *)handle)->setMinimumSize(*(const QSize*)AnonParam1);
}

void QWindow_setMaximumSize(QWindowH handle, const QSizeH AnonParam1)
{
	((QWindow *)handle)->setMaximumSize(*(const QSize*)AnonParam1);
}

void QWindow_setBaseSize(QWindowH handle, const QSizeH AnonParam1)
{
	((QWindow *)handle)->setBaseSize(*(const QSize*)AnonParam1);
}

void QWindow_setSizeIncrement(QWindowH handle, const QSizeH AnonParam1)
{
	((QWindow *)handle)->setSizeIncrement(*(const QSize*)AnonParam1);
}

void QWindow_setGeometry(QWindowH handle, int x, int y, int w, int h)
{
	((QWindow *)handle)->setGeometry(x, y, w, h);
}

void QWindow_setGeometry2(QWindowH handle, PRect AnonParam1)
{
	QRect t_AnonParam1;
	copyPRectToQRect(AnonParam1, t_AnonParam1);
	((QWindow *)handle)->setGeometry(t_AnonParam1);
}

void QWindow_geometry(QWindowH handle, PRect retval)
{
	QRect t_retval;
	t_retval = ((QWindow *)handle)->geometry();
	copyQRectToPRect(t_retval, retval);
}

void QWindow_frameMargins(QWindowH handle, QMarginsH retval)
{
	*(QMargins *)retval = ((QWindow *)handle)->frameMargins();
}

void QWindow_frameGeometry(QWindowH handle, PRect retval)
{
	QRect t_retval;
	t_retval = ((QWindow *)handle)->frameGeometry();
	copyQRectToPRect(t_retval, retval);
}

void QWindow_framePosition(QWindowH handle, PQtPoint retval)
{
	*(QPoint *)retval = ((QWindow *)handle)->framePosition();
}

void QWindow_setFramePosition(QWindowH handle, const QPointH AnonParam1)
{
  ((QWindow *)handle)->setFramePosition(*(const QPoint*)AnonParam1);
}

int QWindow_width(QWindowH handle)
{
	return (int) ((QWindow *)handle)->width();
}

int QWindow_height(QWindowH handle)
{
	return (int) ((QWindow *)handle)->height();
}

int QWindow_x(QWindowH handle)
{
	return (int) ((QWindow *)handle)->x();
}

int QWindow_y(QWindowH handle)
{
	return (int) ((QWindow *)handle)->y();
}

void QWindow_size(QWindowH handle, PSize retval)
{
	*(QSize *)retval = ((QWindow *)handle)->size();
}

void QWindow_position(QWindowH handle, PQtPoint retval)
{
	*(QPoint *)retval = ((QWindow *)handle)->position();
}

void QWindow_setPosition(QWindowH handle, const QPointH AnonParam1)
{
  ((QWindow *)handle)->setPosition(*(const QPoint*)AnonParam1);
}

void QWindow_setPosition2(QWindowH handle, int posx, int posy)
{
  ((QWindow *)handle)->setPosition(posx, posy);
}

void QWindow_resize(QWindowH handle, const QSizeH AnonParam1)
{
	((QWindow *)handle)->resize(*(const QSize*)AnonParam1);
}

void QWindow_resize2(QWindowH handle, int w, int h)
{
	((QWindow *)handle)->resize(w, h);
}

void QWindow_setFilePath(QWindowH handle, PWideString AnonParam1)
{
	QString t_AnonParam1;
	copyPWideStringToQString(AnonParam1, t_AnonParam1);
	((QWindow *)handle)->setFilePath(t_AnonParam1);
}

void QWindow_filePath(QWindowH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QWindow *)handle)->filePath();
	copyQStringToPWideString(t_retval, retval);
}

void QWindow_setIcon(QWindowH handle, const QIconH icon)
{
	((QWindow *)handle)->setIcon(*(const QIcon*)icon);
}

void QWindow_icon(QWindowH handle, QIconH retval)
{
	*(QIcon *)retval = ((QWindow *)handle)->icon();
}

QScreenH QWindow_screen(QWindowH handle)
{
  return (QScreenH) ((QWindow *)handle)->screen();
}

void QWindow_setScreen(QWindowH handle, QScreenH screen)
{
  ((QWindow *)handle)->setScreen((QScreen *) screen);
}

bool QWindow_setKeyboardGrabEnabled(QWindowH handle, bool grab)
{
  return ((QWindow *)handle)->setKeyboardGrabEnabled(grab);
}

bool QWindow_setMouseGrabEnabled(QWindowH handle, bool grab)
{
  return ((QWindow *)handle)->setMouseGrabEnabled(grab);
}

void QWindow_mapToGlobal(QWindowH handle, PQtPoint retval, const QPointH AnonParam1)
{
	*(QPoint *)retval = ((QWindow *)handle)->mapToGlobal(*(const QPoint*)AnonParam1);
}

void QWindow_mapFromGlobal(QWindowH handle, PQtPoint retval, const QPointH AnonParam1)
{
	*(QPoint *)retval = ((QWindow *)handle)->mapFromGlobal(*(const QPoint*)AnonParam1);
}

void QWindow_cursor(QWindowH handle, QCursorH retval)
{
	*(QCursor *)retval = ((QWindow *)handle)->cursor();
}

void QWindow_setCursor(QWindowH handle, const QCursorH AnonParam1)
{
	((QWindow *)handle)->setCursor(*(const QCursor*)AnonParam1);
}

void QWindow_unsetCursor(QWindowH handle)
{
	((QWindow *)handle)->unsetCursor();
}

QObjectH QWindow_focusObject(QWindowH handle)
{
  return (QObjectH) ((QWindow *)handle)->focusObject();
}

QWindowH QWindow_fromWinID(WId id)
{
  return (QWindowH) QWindow::fromWinId((WId) id);
}

void QWindow_requestActivate(QWindowH handle)
{
  ((QWindow *)handle)->requestActivate();
}

void QWindow_setVisible(QWindowH handle, bool visible)
{
  ((QWindow *)handle)->setVisible(visible);
}

void QWindow_show(QWindowH handle)
{
  ((QWindow *)handle)->show();
}

void QWindow_hide(QWindowH handle)
{
  ((QWindow *)handle)->hide();
}

void QWindow_showMinimized(QWindowH handle)
{
  ((QWindow *)handle)->showMinimized();
}

void QWindow_showMaximized(QWindowH handle)
{
  ((QWindow *)handle)->showMaximized();
}

void QWindow_showFullScreen(QWindowH handle)
{
  ((QWindow *)handle)->showFullScreen();
}

void QWindow_showNormal(QWindowH handle)
{
  ((QWindow *)handle)->showNormal();
}

bool QWindow_close(QWindowH handle)
{
  return (bool) ((QWindow *)handle)->close();
}

void QWindow_raise(QWindowH handle)
{
  ((QWindow *)handle)->raise();
}

void QWindow_lower(QWindowH handle)
{
  ((QWindow *)handle)->lower();
}

void QWindow_setWindowTitle(QWindowH handle, PWideString AnonParam1)
{
	QString t_AnonParam1;
	copyPWideStringToQString(AnonParam1, t_AnonParam1);
	((QWindow *)handle)->setTitle(t_AnonParam1);
}

void QWindow_setX(QWindowH handle, int argx)
{
  ((QWindow *)handle)->setX(argx);
}

void QWindow_setY(QWindowH handle, int argy)
{
  ((QWindow *)handle)->setY(argy);
}

void QWindow_setWidth(QWindowH handle, int argw)
{
  ((QWindow *)handle)->setWidth(argw);
}

void QWindow_setHeight(QWindowH handle, int argh)
{
  ((QWindow *)handle)->setHeight(argh);
}

void QWindow_setMinimumWidth(QWindowH handle, int w)
{
  ((QWindow *)handle)->setMinimumWidth(w);
}

void QWindow_setMinimumHeight(QWindowH handle, int h)
{
  ((QWindow *)handle)->setMinimumHeight(h);
}

void QWindow_setMaximumWidth(QWindowH handle, int w)
{
  ((QWindow *)handle)->setMaximumWidth(w);
}

void QWindow_setMaximumHeight(QWindowH handle, int h)
{
  ((QWindow *)handle)->setMaximumHeight(h);
}

void QWindow_alert(QWindowH handle, int msec)
{
  ((QWindow *)handle)->alert(msec);
}

void QWindow_requestUpdate(QWindowH handle)
{
  ((QWindow *)handle)->requestUpdate();
}

