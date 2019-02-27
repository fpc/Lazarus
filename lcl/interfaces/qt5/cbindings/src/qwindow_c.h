//******************************************************************************
//  Copyright (c) 2007 by Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QWINDOW_C_H
#define QWINDOW_C_H

#include <QtGui>
#include "pascalbind.h"

C_EXPORT QWindowH QWindow_Create(QWindowH parent);
C_EXPORT QWindowH QWindow_Create2(QScreenH screen = Q_NULLPTR);
C_EXPORT void QWindow_Destroy(QWindowH handle);
C_EXPORT void QWindow_setSurfaceType(QWindowH handle, QSurface::SurfaceType surfaceType);
C_EXPORT QSurface::SurfaceType QWindow_surfaceType(QWindowH handle);
C_EXPORT bool QWindow_isVisible(QWindowH handle);
C_EXPORT QWindow::Visibility QWindow_visibility(QWindowH handle);
C_EXPORT void QWindow_setVisibility(QWindowH handle, QWindow::Visibility v);
C_EXPORT void QWindow_createPlatformResources(QWindowH handle); /* void create(); */
C_EXPORT void QWindow_destroyPlatformResources(QWindowH handle); /* void destroy(); */
C_EXPORT WId QWindow_winId(QWindowH handle);
C_EXPORT QWindowH QWindow_parent(QWindowH handle);
C_EXPORT void QWindow_setParent(QWindowH handle, QWindowH parent);
C_EXPORT bool QWindow_isTopLevel(QWindowH handle);
C_EXPORT bool QWindow_isModal(QWindowH handle);
C_EXPORT Qt::WindowModality QWindow_modality(QWindowH handle);
C_EXPORT void QWindow_setModality(QWindowH handle, Qt::WindowModality windowModality);
C_EXPORT void QWindow_setFormat(QWindowH handle, const QSurfaceFormatH format);
C_EXPORT void QWindow_format(QWindowH handle, QSurfaceFormatH retval);
C_EXPORT void QWindow_requestedFormat(QWindowH handle, QSurfaceFormatH retval);
C_EXPORT void QWindow_setFlags(QWindowH handle, unsigned int flags);
C_EXPORT unsigned int QWindow_flags(QWindowH handle);
C_EXPORT Qt::WindowType QWindow_type(QWindowH handle);
C_EXPORT void QWindow_title(QWindowH handle, PWideString retval);
C_EXPORT void QWindow_setOpacity(QWindowH handle, qreal level);
C_EXPORT qreal QWindow_opacity(QWindowH handle);
C_EXPORT void QWindow_setMask(QWindowH handle, const QRegionH AnonParam1);
C_EXPORT void QWindow_mask(QWindowH handle, QRegionH retval);
C_EXPORT bool QWindow_isActive(QWindowH handle);
C_EXPORT void QWindow_reportContentOrientationChange(QWindowH handle, Qt::ScreenOrientation orientation);
C_EXPORT Qt::ScreenOrientation QWindow_contentOrientation(QWindowH handle);
C_EXPORT qreal QWindow_devicePixelRatio(QWindowH handle);
C_EXPORT unsigned int QWindow_windowState(QWindowH handle);
C_EXPORT void QWindow_setWindowState(QWindowH handle, unsigned int state);
C_EXPORT void QWindow_setTransientParent(QWindowH handle, QWindowH parent);
C_EXPORT QWindowH QWindow_transientParent(QWindowH handle);
C_EXPORT bool QWindow_isAncestorOf(QWindowH handle, const QWindowH child, QWindow::AncestorMode mode = QWindow::AncestorMode::IncludeTransients);
C_EXPORT bool QWindow_isExposed(QWindowH handle);
C_EXPORT int QWindow_minimumWidth(QWindowH handle);
C_EXPORT int QWindow_minimumHeight(QWindowH handle);
C_EXPORT int QWindow_maximumWidth(QWindowH handle);
C_EXPORT int QWindow_maximumHeight(QWindowH handle);
C_EXPORT void QWindow_minimumSize(QWindowH handle, PSize retval);
C_EXPORT void QWindow_maximumSize(QWindowH handle, PSize retval);
C_EXPORT void QWindow_baseSize(QWindowH handle, PSize retval);
C_EXPORT void QWindow_sizeIncrement(QWindowH handle, PSize retval);
C_EXPORT void QWindow_setMinimumSize(QWindowH handle, const QSizeH AnonParam1);
C_EXPORT void QWindow_setMaximumSize(QWindowH handle, const QSizeH AnonParam1);
C_EXPORT void QWindow_setBaseSize(QWindowH handle, const QSizeH AnonParam1);
C_EXPORT void QWindow_setSizeIncrement(QWindowH handle, const QSizeH AnonParam1);
C_EXPORT void QWindow_setGeometry(QWindowH handle, int x, int y, int w, int h);
C_EXPORT void QWindow_setGeometry2(QWindowH handle, PRect AnonParam1);
C_EXPORT void QWindow_geometry(QWindowH handle, PRect retval);
C_EXPORT void QWindow_frameMargins(QWindowH handle, QMarginsH retval);
C_EXPORT void QWindow_frameGeometry(QWindowH handle, PRect retval);
C_EXPORT void QWindow_framePosition(QWindowH handle, PQtPoint retval);
C_EXPORT void QWindow_setFramePosition(QWindowH handle, const QPointH AnonParam1);
C_EXPORT int QWindow_width(QWindowH handle);
C_EXPORT int QWindow_height(QWindowH handle);
C_EXPORT int QWindow_x(QWindowH handle);
C_EXPORT int QWindow_y(QWindowH handle);
C_EXPORT void QWindow_size(QWindowH handle, PSize retval);
C_EXPORT void QWindow_position(QWindowH handle, PQtPoint retval);
C_EXPORT void QWindow_setPosition(QWindowH handle, const QPointH AnonParam1);
C_EXPORT void QWindow_setPosition2(QWindowH handle, int posx, int posy);
C_EXPORT void QWindow_resize(QWindowH handle, const QSizeH AnonParam1);
C_EXPORT void QWindow_resize2(QWindowH handle, int w, int h);
C_EXPORT void QWindow_setFilePath(QWindowH handle, PWideString AnonParam1);
C_EXPORT void QWindow_filePath(QWindowH handle, PWideString retval);
C_EXPORT void QWindow_setIcon(QWindowH handle, const QIconH icon);
C_EXPORT void QWindow_icon(QWindowH handle, QIconH retval);
C_EXPORT QScreenH QWindow_screen(QWindowH handle);
C_EXPORT void QWindow_setScreen(QWindowH handle, QScreenH screen);
C_EXPORT bool QWindow_setKeyboardGrabEnabled(QWindowH handle, bool grab);
C_EXPORT bool QWindow_setMouseGrabEnabled(QWindowH handle, bool grab);
C_EXPORT void QWindow_mapToGlobal(QWindowH handle, PQtPoint retval, const QPointH AnonParam1);
C_EXPORT void QWindow_mapFromGlobal(QWindowH handle, PQtPoint retval, const QPointH AnonParam1);
C_EXPORT void QWindow_cursor(QWindowH handle, QCursorH retval);
C_EXPORT void QWindow_setCursor(QWindowH handle, const QCursorH AnonParam1);
C_EXPORT void QWindow_unsetCursor(QWindowH handle);
C_EXPORT QObjectH QWindow_focusObject(QWindowH handle);
C_EXPORT QWindowH QWindow_fromWinID(WId id);
C_EXPORT void QWindow_requestActivate(QWindowH handle);
C_EXPORT void QWindow_setVisible(QWindowH handle, bool visible);
C_EXPORT void QWindow_show(QWindowH handle);
C_EXPORT void QWindow_hide(QWindowH handle);
C_EXPORT void QWindow_showMinimized(QWindowH handle);
C_EXPORT void QWindow_showMaximized(QWindowH handle);
C_EXPORT void QWindow_showFullScreen(QWindowH handle);
C_EXPORT void QWindow_showNormal(QWindowH handle);
C_EXPORT bool QWindow_close(QWindowH handle);
C_EXPORT void QWindow_raise(QWindowH handle);
C_EXPORT void QWindow_lower(QWindowH handle);
C_EXPORT void QWindow_setTitle(QWindowH handle, PWideString AnonParam1);
C_EXPORT void QWindow_setX(QWindowH handle, int argx);
C_EXPORT void QWindow_setY(QWindowH handle, int argy);
C_EXPORT void QWindow_setWidth(QWindowH handle, int argw);
C_EXPORT void QWindow_setHeight(QWindowH handle, int argh);
C_EXPORT void QWindow_setMinimumWidth(QWindowH handle, int w);
C_EXPORT void QWindow_setMinimumHeight(QWindowH handle, int h);
C_EXPORT void QWindow_setMaximumWidth(QWindowH handle, int w);
C_EXPORT void QWindow_setMaximumHeight(QWindowH handle, int h);
C_EXPORT void QWindow_alert(QWindowH handle, int msec);
C_EXPORT void QWindow_requestUpdate(QWindowH handle);

#endif
