//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QGUIAPPLICATION_C_H
#define QGUIAPPLICATION_C_H

#include <QtGui>
#include "pascalbind.h"
/* TODO: applicationDisplayNameChanged(), layoutDirectionChanged signals, desktopFileName, setDesktopFileName */
C_EXPORT QGuiApplicationH QGuiApplication_Create(int* argc, char** argv, int AnonParam3);
C_EXPORT void QGuiApplication_Destroy(QGuiApplicationH handle);
C_EXPORT void QGuiApplication_setApplicationDisplayName(PWideString name);
C_EXPORT void QGuiApplication_applicationDisplayName(PWideString retval);
C_EXPORT void QGuiApplication_allWindows(PPtrIntArray retval);
C_EXPORT void QGuiApplication_topLevelWindows(PPtrIntArray retval);
C_EXPORT QWindowH QGuiApplication_topLevelAt(const QPointH pos);
C_EXPORT void QGuiApplication_platformName(PWideString retval);
C_EXPORT QWindowH QGuiApplication_modalWindow();
C_EXPORT QWindowH QGuiApplication_focusWindow();
C_EXPORT QObjectH QGuiApplication_focusObject();
C_EXPORT QScreenH QGuiApplication_primaryScreen();
C_EXPORT QScreenH QGuiApplication_screenAt(const QPointH pos);
C_EXPORT void QGuiApplication_screens(PPtrIntArray retval);
C_EXPORT qreal QGuiApplication_devicePixelRatio(QGuiApplicationH handle);
C_EXPORT QCursorH QGuiApplication_overrideCursor();
C_EXPORT void QGuiApplication_setOverrideCursor(const QCursorH AnonParam1);
C_EXPORT void QGuiApplication_changeOverrideCursor(const QCursorH AnonParam1);
C_EXPORT void QGuiApplication_restoreOverrideCursor();
C_EXPORT void QGuiApplication_font(QFontH retval);
C_EXPORT void QGuiApplication_setFont(const QFontH AnonParam1);
C_EXPORT QClipboardH QGuiApplication_clipboard();
C_EXPORT void QGuiApplication_palette(QPaletteH retval);
C_EXPORT QPaletteH QGuiApplication_palette2();
C_EXPORT void QGuiApplication_setPalette(const QPaletteH pal);
C_EXPORT unsigned int QGuiApplication_keyboardModifiers();
C_EXPORT unsigned int QGuiApplication_queryKeyboardModifiers();
C_EXPORT unsigned int QGuiApplication_mouseButtons();
C_EXPORT void QGuiApplication_setLayoutDirection(Qt::LayoutDirection direction);
C_EXPORT Qt::LayoutDirection QGuiApplication_layoutDirection();
C_EXPORT bool QGuiApplication_isRightToLeft();
C_EXPORT bool QGuiApplication_isLeftToRight();
C_EXPORT QStyleHintsH QGuiApplication_styleHints();
C_EXPORT void QGuiApplication_setDesktopSettingsAware(bool on);
C_EXPORT bool QGuiApplication_desktopSettingsAware();
C_EXPORT QInputMethodH QGuiApplication_inputMethod();
C_EXPORT void QGuiApplication_setQuitOnLastWindowClosed(bool quit);
C_EXPORT bool QGuiApplication_quitOnLastWindowClosed();
C_EXPORT int QGuiApplication_exec();
C_EXPORT bool QGuiApplication_notify(QGuiApplicationH handle, QObjectH AnonParam1, QEventH AnonParam2);
C_EXPORT bool QGuiApplication_isSessionRestored(QGuiApplicationH handle);
C_EXPORT void QGuiApplication_sessionId(QGuiApplicationH handle, PWideString retval);
C_EXPORT void QGuiApplication_sessionKey(QGuiApplicationH handle, PWideString retval);
C_EXPORT bool QGuiApplication_isSavingSession(QGuiApplicationH handle);
C_EXPORT Qt::ApplicationState QGuiApplication_applicationState();
C_EXPORT Qt::HighDpiScaleFactorRoundingPolicy QGuiApplication_highDpiScaleFactorRoundingPolicy();
C_EXPORT void QGuiApplication_setHighDpiScaleFactorRoundingPolicy(Qt::HighDpiScaleFactorRoundingPolicy policy);
#if defined BINUX
C_EXPORT Display* QGuiApplication_x11Display(QGuiApplicationH handle);
C_EXPORT xcb_connection_t* QGuiApplication_xcbConnection(QGuiApplicationH handle);
#endif

#endif
