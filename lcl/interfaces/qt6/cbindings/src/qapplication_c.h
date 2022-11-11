//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QAPPLICATION_C_H
#define QAPPLICATION_C_H

#include <QtWidgets>
#include "pascalbind.h"

C_EXPORT QApplicationH QApplication_Create(int* argc, char** argv, int AnonParam3);
C_EXPORT void QApplication_Destroy(QApplicationH handle);
C_EXPORT QStyleH QApplication_style();
C_EXPORT void QApplication_setStyle(QStyleH AnonParam1);
C_EXPORT QStyleH QApplication_setStyle2(PWideString AnonParam1);
C_EXPORT void QApplication_palette(QPaletteH retval, const QWidgetH AnonParam1);
C_EXPORT void QApplication_palette2(QPaletteH retval, const char* className);
C_EXPORT void QApplication_setPalette(const QPaletteH AnonParam1, const char* className);
C_EXPORT void QApplication_font(QFontH retval);
C_EXPORT void QApplication_font2(QFontH retval, const QWidgetH AnonParam1);
C_EXPORT void QApplication_font3(QFontH retval, const char* className);
C_EXPORT void QApplication_setFont(const QFontH AnonParam1, const char* className);
C_EXPORT void QApplication_fontMetrics(QFontMetricsH retval);
C_EXPORT void QApplication_setWindowIcon(const QIconH icon);
C_EXPORT void QApplication_windowIcon(QIconH retval);
C_EXPORT QWidgetH QApplication_activePopupWidget();
C_EXPORT QWidgetH QApplication_activeModalWidget();
C_EXPORT QWidgetH QApplication_focusWidget();
C_EXPORT QWidgetH QApplication_activeWindow();
C_EXPORT void QApplication_setActiveWindow(QWidgetH act);
C_EXPORT QWidgetH QApplication_widgetAt(const QPointH p);
C_EXPORT QWidgetH QApplication_widgetAt2(int x, int y);
C_EXPORT QWidgetH QApplication_topLevelAt(const QPointH p);
C_EXPORT QWidgetH QApplication_topLevelAt2(int x, int y);
C_EXPORT void QApplication_beep();
C_EXPORT void QApplication_alert(QWidgetH widget, int duration);
C_EXPORT void QApplication_setCursorFlashTime(int AnonParam1);
C_EXPORT int QApplication_cursorFlashTime();
C_EXPORT void QApplication_setDoubleClickInterval(int AnonParam1);
C_EXPORT int QApplication_doubleClickInterval();
C_EXPORT void QApplication_setKeyboardInputInterval(int AnonParam1);
C_EXPORT int QApplication_keyboardInputInterval();
C_EXPORT void QApplication_setWheelScrollLines(int AnonParam1);
C_EXPORT int QApplication_wheelScrollLines();
C_EXPORT void QApplication_setStartDragTime(int ms);
C_EXPORT int QApplication_startDragTime();
C_EXPORT void QApplication_setStartDragDistance(int l);
C_EXPORT int QApplication_startDragDistance();
C_EXPORT bool QApplication_isEffectEnabled(Qt::UIEffect AnonParam1);
C_EXPORT void QApplication_setEffectEnabled(Qt::UIEffect AnonParam1, bool enable);
C_EXPORT int QApplication_exec();
C_EXPORT bool QApplication_notify(QApplicationH handle, QObjectH AnonParam1, QEventH AnonParam2);
C_EXPORT void QApplication_styleSheet(QApplicationH handle, PWideString retval);
C_EXPORT void QApplication_setStyleSheet(QApplicationH handle, PWideString sheet);
C_EXPORT void QApplication_setAutoSipEnabled(QApplicationH handle, const bool enabled);
C_EXPORT bool QApplication_autoSipEnabled(QApplicationH handle);
C_EXPORT void QApplication_closeAllWindows();
C_EXPORT void QApplication_aboutQt();

#endif
