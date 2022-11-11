//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qguiapplication_c.h"

QGuiApplicationH QGuiApplication_Create(int* argc, char** argv, int AnonParam3)
{
	return (QGuiApplicationH) new QGuiApplication(*(int*)argc, argv, AnonParam3);
}

void QGuiApplication_Destroy(QGuiApplicationH handle)
{
	delete (QGuiApplication *)handle;
}

void QGuiApplication_setApplicationDisplayName(PWideString name)
{
	QString t_name;
	copyPWideStringToQString(name, t_name);
	QGuiApplication::setApplicationDisplayName(t_name);
}

void QGuiApplication_applicationDisplayName(PWideString retval)
{
	QString t_retval;
	t_retval = QGuiApplication::applicationDisplayName();
	copyQStringToPWideString(t_retval, retval);
}

void QGuiApplication_allWindows(PPtrIntArray retval)
{
	QWindowList t_retval;
	t_retval = QGuiApplication::allWindows();
	copyQListTemplateToPtrIntArray(t_retval, retval);
}

void QGuiApplication_topLevelWindows(PPtrIntArray retval)
{
	QWindowList t_retval;
	t_retval = QGuiApplication::topLevelWindows();
	copyQListTemplateToPtrIntArray(t_retval, retval);
}

QWindowH QGuiApplication_topLevelAt(const QPointH pos)
{
	return (QWindowH) QGuiApplication::topLevelAt(*(const QPoint*)pos);
}

void QGuiApplication_platformName(PWideString retval)
{
	QString t_retval;
	t_retval = QGuiApplication::platformName();
	copyQStringToPWideString(t_retval, retval);
}

QWindowH QGuiApplication_modalWindow()
{
	return (QWindowH) QGuiApplication::modalWindow();
}

QWindowH QGuiApplication_focusWindow()
{
	return (QWindowH) QGuiApplication::focusWindow();
}

QObjectH QGuiApplication_focusObject()
{
	return (QObjectH) QGuiApplication::focusObject();
}

QScreenH QGuiApplication_primaryScreen()
{
	return (QScreenH) QGuiApplication::primaryScreen();
}

QScreenH QGuiApplication_screenAt(const QPointH pos)
{
	return (QScreenH) QGuiApplication::screenAt(*(const QPoint*)pos);
}

void QGuiApplication_screens(PPtrIntArray retval)
{
	QList<QScreen*> t_retval;
	t_retval = QGuiApplication::screens();
	copyQListTemplateToPtrIntArray(t_retval, retval);
}

qreal QGuiApplication_devicePixelRatio(QGuiApplicationH handle)
{
	return (qreal) ((QGuiApplication *)handle)->devicePixelRatio();
}

QCursorH QGuiApplication_overrideCursor()
{
	return (QCursorH) QGuiApplication::overrideCursor();
}

void QGuiApplication_setOverrideCursor(const QCursorH AnonParam1)
{
	QGuiApplication::setOverrideCursor(*(const QCursor*)AnonParam1);
}

void QGuiApplication_changeOverrideCursor(const QCursorH AnonParam1)
{
	QGuiApplication::changeOverrideCursor(*(const QCursor*)AnonParam1);
}

void QGuiApplication_restoreOverrideCursor()
{
	QGuiApplication::restoreOverrideCursor();
}

void QGuiApplication_font(QFontH retval)
{
	*(QFont *)retval = QGuiApplication::font();
}

void QGuiApplication_setFont(const QFontH AnonParam1)
{
	QGuiApplication::setFont(*(const QFont*)AnonParam1);
}

QClipboardH QGuiApplication_clipboard()
{
	return (QClipboardH) QGuiApplication::clipboard();
}

void QGuiApplication_palette(QPaletteH retval)
{
	*(QPalette *)retval = QGuiApplication::palette();
}

QPaletteH QGuiApplication_palette2()
{
  QPalette* pal = new QPalette(QGuiApplication::palette());
  return (QPaletteH) pal;
}

void QGuiApplication_setPalette(const QPaletteH pal)
{
	QGuiApplication::setPalette(*(const QPalette*)pal);
}

unsigned int QGuiApplication_keyboardModifiers()
{
	return (unsigned int) QGuiApplication::keyboardModifiers();
}

unsigned int QGuiApplication_queryKeyboardModifiers()
{
	return (unsigned int) QGuiApplication::queryKeyboardModifiers();
}

unsigned int QGuiApplication_mouseButtons()
{
	return (unsigned int) QGuiApplication::mouseButtons();
}

void QGuiApplication_setLayoutDirection(Qt::LayoutDirection direction)
{
	QGuiApplication::setLayoutDirection(direction);
}

Qt::LayoutDirection QGuiApplication_layoutDirection()
{
	return (Qt::LayoutDirection) QGuiApplication::layoutDirection();
}

bool QGuiApplication_isRightToLeft()
{
	return (bool) QGuiApplication::isRightToLeft();
}

bool QGuiApplication_isLeftToRight()
{
	return (bool) QGuiApplication::isLeftToRight();
}

QStyleHintsH QGuiApplication_styleHints()
{
	return (QStyleHintsH) QGuiApplication::styleHints();
}

void QGuiApplication_setDesktopSettingsAware(bool on)
{
	QGuiApplication::setDesktopSettingsAware(on);
}

bool QGuiApplication_desktopSettingsAware()
{
	return (bool) QGuiApplication::desktopSettingsAware();
}

QInputMethodH QGuiApplication_inputMethod()
{
	return (QInputMethodH) QGuiApplication::inputMethod();
}

void QGuiApplication_setQuitOnLastWindowClosed(bool quit)
{
	QGuiApplication::setQuitOnLastWindowClosed(quit);
}

bool QGuiApplication_quitOnLastWindowClosed()
{
	return (bool) QGuiApplication::quitOnLastWindowClosed();
}

int QGuiApplication_exec()
{
	return (int) QGuiApplication::exec();
}

bool QGuiApplication_notify(QGuiApplicationH handle, QObjectH AnonParam1, QEventH AnonParam2)
{
	return (bool) ((QGuiApplication *)handle)->notify((QObject*)AnonParam1, (QEvent*)AnonParam2);
}

bool QGuiApplication_isSessionRestored(QGuiApplicationH handle)
{
	return (bool) ((QGuiApplication *)handle)->isSessionRestored();
}

void QGuiApplication_sessionId(QGuiApplicationH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QGuiApplication *)handle)->sessionId();
	copyQStringToPWideString(t_retval, retval);
}

void QGuiApplication_sessionKey(QGuiApplicationH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QGuiApplication *)handle)->sessionKey();
	copyQStringToPWideString(t_retval, retval);
}

bool QGuiApplication_isSavingSession(QGuiApplicationH handle)
{
	return (bool) ((QGuiApplication *)handle)->isSavingSession();
}

Qt::ApplicationState QGuiApplication_applicationState()
{
  return (Qt::ApplicationState) QGuiApplication::applicationState();
}

Qt::HighDpiScaleFactorRoundingPolicy QGuiApplication_highDpiScaleFactorRoundingPolicy()
{
  return (Qt::HighDpiScaleFactorRoundingPolicy) QGuiApplication::highDpiScaleFactorRoundingPolicy();
}

void QGuiApplication_setHighDpiScaleFactorRoundingPolicy(Qt::HighDpiScaleFactorRoundingPolicy policy)
{
  QGuiApplication::setHighDpiScaleFactorRoundingPolicy((Qt::HighDpiScaleFactorRoundingPolicy)policy);
}


/* QTBUG-49667 fixed this workaround, fallback session management does not exist in Qt6
bool QGuiApplication_isFallbackSessionManagementEnabled()
{
	return (bool) QGuiApplication::isFallbackSessionManagementEnabled();
}

void QGuiApplication_setFallbackSessionManagementEnabled(bool enabled)
{
	QGuiApplication::setFallbackSessionManagementEnabled(enabled);
}
*/

