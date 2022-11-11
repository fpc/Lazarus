//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qapplication_c.h"

QApplicationH QApplication_Create(int* argc, char** argv, int AnonParam3)
{
	return (QApplicationH) new QApplication(*(int*)argc, argv, AnonParam3);
}

void QApplication_Destroy(QApplicationH handle)
{
	delete (QApplication *)handle;
}

QStyleH QApplication_style()
{
	return (QStyleH) QApplication::style();
}

void QApplication_setStyle(QStyleH AnonParam1)
{
	QApplication::setStyle((QStyle*)AnonParam1);
}

QStyleH QApplication_setStyle2(PWideString AnonParam1)
{
	QString t_AnonParam1;
	copyPWideStringToQString(AnonParam1, t_AnonParam1);
	return (QStyleH) QApplication::setStyle(t_AnonParam1);
}

void QApplication_palette(QPaletteH retval, const QWidgetH AnonParam1)
{
	*(QPalette *)retval = QApplication::palette((const QWidget*)AnonParam1);
}

void QApplication_palette2(QPaletteH retval, const char* className)
{
	*(QPalette *)retval = QApplication::palette(className);
}

void QApplication_setPalette(const QPaletteH AnonParam1, const char* className)
{
	QApplication::setPalette(*(const QPalette*)AnonParam1, className);
}

void QApplication_font(QFontH retval)
{
	*(QFont *)retval = QApplication::font();
}

void QApplication_font2(QFontH retval, const QWidgetH AnonParam1)
{
	*(QFont *)retval = QApplication::font((const QWidget*)AnonParam1);
}

void QApplication_font3(QFontH retval, const char* className)
{
	*(QFont *)retval = QApplication::font(className);
}

void QApplication_setFont(const QFontH AnonParam1, const char* className)
{
	QApplication::setFont(*(const QFont*)AnonParam1, className);
}

void QApplication_fontMetrics(QFontMetricsH retval)
{
	*(QFontMetrics *)retval = QApplication::fontMetrics();
}

void QApplication_setWindowIcon(const QIconH icon)
{
	QApplication::setWindowIcon(*(const QIcon*)icon);
}

void QApplication_windowIcon(QIconH retval)
{
	*(QIcon *)retval = QApplication::windowIcon();
}

QWidgetH QApplication_activePopupWidget()
{
	return (QWidgetH) QApplication::activePopupWidget();
}

QWidgetH QApplication_activeModalWidget()
{
	return (QWidgetH) QApplication::activeModalWidget();
}

QWidgetH QApplication_focusWidget()
{
	return (QWidgetH) QApplication::focusWidget();
}

QWidgetH QApplication_activeWindow()
{
	return (QWidgetH) QApplication::activeWindow();
}

void QApplication_setActiveWindow(QWidgetH act)
{
	QApplication::setActiveWindow((QWidget*)act);
}

QWidgetH QApplication_widgetAt(const QPointH p)
{
	return (QWidgetH) QApplication::widgetAt(*(const QPoint*)p);
}

QWidgetH QApplication_widgetAt2(int x, int y)
{
	return (QWidgetH) QApplication::widgetAt(x, y);
}

QWidgetH QApplication_topLevelAt(const QPointH p)
{
	return (QWidgetH) QApplication::topLevelAt(*(const QPoint*)p);
}

QWidgetH QApplication_topLevelAt2(int x, int y)
{
	return (QWidgetH) QApplication::topLevelAt(x, y);
}

void QApplication_beep()
{
	QApplication::beep();
}

void QApplication_alert(QWidgetH widget, int duration)
{
	QApplication::alert((QWidget*)widget, duration);
}

void QApplication_setCursorFlashTime(int AnonParam1)
{
	QApplication::setCursorFlashTime(AnonParam1);
}

int QApplication_cursorFlashTime()
{
	return (int) QApplication::cursorFlashTime();
}

void QApplication_setDoubleClickInterval(int AnonParam1)
{
	QApplication::setDoubleClickInterval(AnonParam1);
}

int QApplication_doubleClickInterval()
{
	return (int) QApplication::doubleClickInterval();
}

void QApplication_setKeyboardInputInterval(int AnonParam1)
{
	QApplication::setKeyboardInputInterval(AnonParam1);
}

int QApplication_keyboardInputInterval()
{
	return (int) QApplication::keyboardInputInterval();
}

void QApplication_setWheelScrollLines(int AnonParam1)
{
	QApplication::setWheelScrollLines(AnonParam1);
}

int QApplication_wheelScrollLines()
{
	return (int) QApplication::wheelScrollLines();
}

void QApplication_setStartDragTime(int ms)
{
	QApplication::setStartDragTime(ms);
}

int QApplication_startDragTime()
{
	return (int) QApplication::startDragTime();
}

void QApplication_setStartDragDistance(int l)
{
	QApplication::setStartDragDistance(l);
}

int QApplication_startDragDistance()
{
	return (int) QApplication::startDragDistance();
}

bool QApplication_isEffectEnabled(Qt::UIEffect AnonParam1)
{
	return (bool) QApplication::isEffectEnabled(AnonParam1);
}

void QApplication_setEffectEnabled(Qt::UIEffect AnonParam1, bool enable)
{
	QApplication::setEffectEnabled(AnonParam1, enable);
}

int QApplication_exec()
{
	return (int) QApplication::exec();
}

bool QApplication_notify(QApplicationH handle, QObjectH AnonParam1, QEventH AnonParam2)
{
	return (bool) ((QApplication *)handle)->notify((QObject*)AnonParam1, (QEvent*)AnonParam2);
}

void QApplication_styleSheet(QApplicationH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QApplication *)handle)->styleSheet();
	copyQStringToPWideString(t_retval, retval);
}

void QApplication_setStyleSheet(QApplicationH handle, PWideString sheet)
{
	QString t_sheet;
	copyPWideStringToQString(sheet, t_sheet);
	((QApplication *)handle)->setStyleSheet(t_sheet);
}

void QApplication_setAutoSipEnabled(QApplicationH handle, const bool enabled)
{
	((QApplication *)handle)->setAutoSipEnabled(enabled);
}

bool QApplication_autoSipEnabled(QApplicationH handle)
{
	return (bool) ((QApplication *)handle)->autoSipEnabled();
}

void QApplication_closeAllWindows()
{
	QApplication::closeAllWindows();
}

void QApplication_aboutQt()
{
	QApplication::aboutQt();
}

