//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qwidget_c.h"

QWidgetH QWidget_Create(QWidgetH parent, unsigned int f)
{
	return (QWidgetH) new QWidget((QWidget*)parent, (Qt::WindowFlags)f);
}

void QWidget_Destroy(QWidgetH handle)
{
	delete (QWidget *)handle;
}

int QWidget_devType(QWidgetH handle)
{
	return (int) ((QWidget *)handle)->devType();
}

WId QWidget_winId(QWidgetH handle)
{
	return (WId) ((QWidget *)handle)->winId();
}

void QWidget_createWinId(QWidgetH handle)
{
	((QWidget *)handle)->createWinId();
}

WId QWidget_internalWinId(QWidgetH handle)
{
	return (WId) ((QWidget *)handle)->internalWinId();
}

WId QWidget_effectiveWinId(QWidgetH handle)
{
	return (WId) ((QWidget *)handle)->effectiveWinId();
}

QStyleH QWidget_style(QWidgetH handle)
{
	return (QStyleH) ((QWidget *)handle)->style();
}

void QWidget_setStyle(QWidgetH handle, QStyleH AnonParam1)
{
	((QWidget *)handle)->setStyle((QStyle*)AnonParam1);
}

bool QWidget_isTopLevel(QWidgetH handle)
{
	return (bool) ((QWidget *)handle)->isTopLevel();
}

bool QWidget_isWindow(QWidgetH handle)
{
	return (bool) ((QWidget *)handle)->isWindow();
}

bool QWidget_isModal(QWidgetH handle)
{
	return (bool) ((QWidget *)handle)->isModal();
}

Qt::WindowModality QWidget_windowModality(QWidgetH handle)
{
	return (Qt::WindowModality) ((QWidget *)handle)->windowModality();
}

void QWidget_setWindowModality(QWidgetH handle, Qt::WindowModality windowModality)
{
	((QWidget *)handle)->setWindowModality(windowModality);
}

bool QWidget_isEnabled(QWidgetH handle)
{
	return (bool) ((QWidget *)handle)->isEnabled();
}

bool QWidget_isEnabledTo(QWidgetH handle, const QWidgetH AnonParam1)
{
	return (bool) ((QWidget *)handle)->isEnabledTo((const QWidget*)AnonParam1);
}

bool QWidget_isEnabledToTLW(QWidgetH handle)
{
	return (bool) ((QWidget *)handle)->isEnabledToTLW();
}

void QWidget_setEnabled(QWidgetH handle, bool AnonParam1)
{
	((QWidget *)handle)->setEnabled(AnonParam1);
}

void QWidget_setDisabled(QWidgetH handle, bool AnonParam1)
{
	((QWidget *)handle)->setDisabled(AnonParam1);
}

void QWidget_setWindowModified(QWidgetH handle, bool AnonParam1)
{
	((QWidget *)handle)->setWindowModified(AnonParam1);
}

void QWidget_frameGeometry(QWidgetH handle, PRect retval)
{
	QRect t_retval;
	t_retval = ((QWidget *)handle)->frameGeometry();
	copyQRectToPRect(t_retval, retval);
}

void QWidget_geometry(QWidgetH handle, PRect retval)
{
	QRect t_retval;
	t_retval = ((QWidget *)handle)->geometry();
	copyQRectToPRect(t_retval, retval);
}

void QWidget_normalGeometry(QWidgetH handle, PRect retval)
{
	QRect t_retval;
	t_retval = ((QWidget *)handle)->normalGeometry();
	copyQRectToPRect(t_retval, retval);
}

int QWidget_x(QWidgetH handle)
{
	return (int) ((QWidget *)handle)->x();
}

int QWidget_y(QWidgetH handle)
{
	return (int) ((QWidget *)handle)->y();
}

void QWidget_pos(QWidgetH handle, PQtPoint retval)
{
	*(QPoint *)retval = ((QWidget *)handle)->pos();
}

void QWidget_frameSize(QWidgetH handle, PSize retval)
{
	*(QSize *)retval = ((QWidget *)handle)->frameSize();
}

void QWidget_size(QWidgetH handle, PSize retval)
{
	*(QSize *)retval = ((QWidget *)handle)->size();
}

int QWidget_width(QWidgetH handle)
{
	return (int) ((QWidget *)handle)->width();
}

int QWidget_height(QWidgetH handle)
{
	return (int) ((QWidget *)handle)->height();
}

void QWidget_rect(QWidgetH handle, PRect retval)
{
	QRect t_retval;
	t_retval = ((QWidget *)handle)->rect();
	copyQRectToPRect(t_retval, retval);
}

void QWidget_childrenRect(QWidgetH handle, PRect retval)
{
	QRect t_retval;
	t_retval = ((QWidget *)handle)->childrenRect();
	copyQRectToPRect(t_retval, retval);
}

void QWidget_childrenRegion(QWidgetH handle, QRegionH retval)
{
	*(QRegion *)retval = ((QWidget *)handle)->childrenRegion();
}

void QWidget_minimumSize(QWidgetH handle, PSize retval)
{
	*(QSize *)retval = ((QWidget *)handle)->minimumSize();
}

void QWidget_maximumSize(QWidgetH handle, PSize retval)
{
	*(QSize *)retval = ((QWidget *)handle)->maximumSize();
}

int QWidget_minimumWidth(QWidgetH handle)
{
	return (int) ((QWidget *)handle)->minimumWidth();
}

int QWidget_minimumHeight(QWidgetH handle)
{
	return (int) ((QWidget *)handle)->minimumHeight();
}

int QWidget_maximumWidth(QWidgetH handle)
{
	return (int) ((QWidget *)handle)->maximumWidth();
}

int QWidget_maximumHeight(QWidgetH handle)
{
	return (int) ((QWidget *)handle)->maximumHeight();
}

void QWidget_setMinimumSize(QWidgetH handle, const QSizeH AnonParam1)
{
	((QWidget *)handle)->setMinimumSize(*(const QSize*)AnonParam1);
}

void QWidget_setMinimumSize2(QWidgetH handle, int minw, int minh)
{
	((QWidget *)handle)->setMinimumSize(minw, minh);
}

void QWidget_setMaximumSize(QWidgetH handle, const QSizeH AnonParam1)
{
	((QWidget *)handle)->setMaximumSize(*(const QSize*)AnonParam1);
}

void QWidget_setMaximumSize2(QWidgetH handle, int maxw, int maxh)
{
	((QWidget *)handle)->setMaximumSize(maxw, maxh);
}

void QWidget_setMinimumWidth(QWidgetH handle, int minw)
{
	((QWidget *)handle)->setMinimumWidth(minw);
}

void QWidget_setMinimumHeight(QWidgetH handle, int minh)
{
	((QWidget *)handle)->setMinimumHeight(minh);
}

void QWidget_setMaximumWidth(QWidgetH handle, int maxw)
{
	((QWidget *)handle)->setMaximumWidth(maxw);
}

void QWidget_setMaximumHeight(QWidgetH handle, int maxh)
{
	((QWidget *)handle)->setMaximumHeight(maxh);
}

void QWidget_sizeIncrement(QWidgetH handle, PSize retval)
{
	*(QSize *)retval = ((QWidget *)handle)->sizeIncrement();
}

void QWidget_setSizeIncrement(QWidgetH handle, const QSizeH AnonParam1)
{
	((QWidget *)handle)->setSizeIncrement(*(const QSize*)AnonParam1);
}

void QWidget_setSizeIncrement2(QWidgetH handle, int w, int h)
{
	((QWidget *)handle)->setSizeIncrement(w, h);
}

void QWidget_baseSize(QWidgetH handle, PSize retval)
{
	*(QSize *)retval = ((QWidget *)handle)->baseSize();
}

void QWidget_setBaseSize(QWidgetH handle, const QSizeH AnonParam1)
{
	((QWidget *)handle)->setBaseSize(*(const QSize*)AnonParam1);
}

void QWidget_setBaseSize2(QWidgetH handle, int basew, int baseh)
{
	((QWidget *)handle)->setBaseSize(basew, baseh);
}

void QWidget_setFixedSize(QWidgetH handle, const QSizeH AnonParam1)
{
	((QWidget *)handle)->setFixedSize(*(const QSize*)AnonParam1);
}

void QWidget_setFixedSize2(QWidgetH handle, int w, int h)
{
	((QWidget *)handle)->setFixedSize(w, h);
}

void QWidget_setFixedWidth(QWidgetH handle, int w)
{
	((QWidget *)handle)->setFixedWidth(w);
}

void QWidget_setFixedHeight(QWidgetH handle, int h)
{
	((QWidget *)handle)->setFixedHeight(h);
}

void QWidget_mapToGlobal(QWidgetH handle, PQtPoint retval, const QPointH AnonParam1)
{
	*(QPoint *)retval = ((QWidget *)handle)->mapToGlobal(*(const QPoint*)AnonParam1);
}

void QWidget_mapFromGlobal(QWidgetH handle, PQtPoint retval, const QPointH AnonParam1)
{
	*(QPoint *)retval = ((QWidget *)handle)->mapFromGlobal(*(const QPoint*)AnonParam1);
}

void QWidget_mapToParent(QWidgetH handle, PQtPoint retval, const QPointH AnonParam1)
{
	*(QPoint *)retval = ((QWidget *)handle)->mapToParent(*(const QPoint*)AnonParam1);
}

void QWidget_mapFromParent(QWidgetH handle, PQtPoint retval, const QPointH AnonParam1)
{
	*(QPoint *)retval = ((QWidget *)handle)->mapFromParent(*(const QPoint*)AnonParam1);
}

void QWidget_mapTo(QWidgetH handle, PQtPoint retval, const QWidgetH AnonParam1, const QPointH AnonParam2)
{
	*(QPoint *)retval = ((QWidget *)handle)->mapTo((const QWidget*)AnonParam1, *(const QPoint*)AnonParam2);
}

void QWidget_mapFrom(QWidgetH handle, PQtPoint retval, const QWidgetH AnonParam1, const QPointH AnonParam2)
{
	*(QPoint *)retval = ((QWidget *)handle)->mapFrom((const QWidget*)AnonParam1, *(const QPoint*)AnonParam2);
}

QWidgetH QWidget_window(QWidgetH handle)
{
	return (QWidgetH) ((QWidget *)handle)->window();
}

QWidgetH QWidget_nativeParentWidget(QWidgetH handle)
{
	return (QWidgetH) ((QWidget *)handle)->nativeParentWidget();
}

QWidgetH QWidget_topLevelWidget(QWidgetH handle)
{
	return (QWidgetH) ((QWidget *)handle)->topLevelWidget();
}

const QPaletteH QWidget_palette(QWidgetH handle)
{
	return (const QPaletteH) &((QWidget *)handle)->palette();
}

void QWidget_setPalette(QWidgetH handle, const QPaletteH AnonParam1)
{
	((QWidget *)handle)->setPalette(*(const QPalette*)AnonParam1);
}

void QWidget_setBackgroundRole(QWidgetH handle, QPalette::ColorRole AnonParam1)
{
	((QWidget *)handle)->setBackgroundRole(AnonParam1);
}

QPalette::ColorRole QWidget_backgroundRole(QWidgetH handle)
{
	return (QPalette::ColorRole) ((QWidget *)handle)->backgroundRole();
}

void QWidget_setForegroundRole(QWidgetH handle, QPalette::ColorRole AnonParam1)
{
	((QWidget *)handle)->setForegroundRole(AnonParam1);
}

QPalette::ColorRole QWidget_foregroundRole(QWidgetH handle)
{
	return (QPalette::ColorRole) ((QWidget *)handle)->foregroundRole();
}

const QFontH QWidget_font(QWidgetH handle)
{
	return (const QFontH) &((QWidget *)handle)->font();
}

void QWidget_setFont(QWidgetH handle, const QFontH AnonParam1)
{
	((QWidget *)handle)->setFont(*(const QFont*)AnonParam1);
}

void QWidget_fontMetrics(QWidgetH handle, QFontMetricsH retval)
{
	*(QFontMetrics *)retval = ((QWidget *)handle)->fontMetrics();
}

void QWidget_fontInfo(QWidgetH handle, QFontInfoH retval)
{
	*(QFontInfo *)retval = ((QWidget *)handle)->fontInfo();
}

void QWidget_cursor(QWidgetH handle, QCursorH retval)
{
	*(QCursor *)retval = ((QWidget *)handle)->cursor();
}

void QWidget_setCursor(QWidgetH handle, const QCursorH AnonParam1)
{
	((QWidget *)handle)->setCursor(*(const QCursor*)AnonParam1);
}

void QWidget_unsetCursor(QWidgetH handle)
{
	((QWidget *)handle)->unsetCursor();
}

void QWidget_setMouseTracking(QWidgetH handle, bool enable)
{
	((QWidget *)handle)->setMouseTracking(enable);
}

bool QWidget_hasMouseTracking(QWidgetH handle)
{
	return (bool) ((QWidget *)handle)->hasMouseTracking();
}

bool QWidget_underMouse(QWidgetH handle)
{
	return (bool) ((QWidget *)handle)->underMouse();
}

void QWidget_setMask(QWidgetH handle, const QBitmapH AnonParam1)
{
	((QWidget *)handle)->setMask(*(const QBitmap*)AnonParam1);
}

void QWidget_setMask2(QWidgetH handle, const QRegionH AnonParam1)
{
	((QWidget *)handle)->setMask(*(const QRegion*)AnonParam1);
}

void QWidget_mask(QWidgetH handle, QRegionH retval)
{
	*(QRegion *)retval = ((QWidget *)handle)->mask();
}

void QWidget_clearMask(QWidgetH handle)
{
	((QWidget *)handle)->clearMask();
}

void QWidget_render(QWidgetH handle, QPaintDeviceH target, const QPointH targetOffset, const QRegionH sourceRegion, unsigned int renderFlags)
{
	((QWidget *)handle)->render((QPaintDevice*)target, *(const QPoint*)targetOffset, *(const QRegion*)sourceRegion, (QWidget::RenderFlags)renderFlags);
}

void QWidget_render2(QWidgetH handle, QPainterH painter, const QPointH targetOffset, const QRegionH sourceRegion, unsigned int renderFlags)
{
	((QWidget *)handle)->render((QPainter*)painter, *(const QPoint*)targetOffset, *(const QRegion*)sourceRegion, (QWidget::RenderFlags)renderFlags);
}

void QWidget_grab(QWidgetH handle, QPixmapH retval, PRect rectangle)
{
	QRect t_rectangle;
	copyPRectToQRect(rectangle, t_rectangle);
	*(QPixmap *)retval = ((QWidget *)handle)->grab(t_rectangle);
}

QGraphicsEffectH QWidget_graphicsEffect(QWidgetH handle)
{
	return (QGraphicsEffectH) ((QWidget *)handle)->graphicsEffect();
}

void QWidget_setGraphicsEffect(QWidgetH handle, QGraphicsEffectH effect)
{
	((QWidget *)handle)->setGraphicsEffect((QGraphicsEffect*)effect);
}

void QWidget_grabGesture(QWidgetH handle, Qt::GestureType type, unsigned int flags)
{
	((QWidget *)handle)->grabGesture(type, (Qt::GestureFlags)flags);
}

void QWidget_ungrabGesture(QWidgetH handle, Qt::GestureType type)
{
	((QWidget *)handle)->ungrabGesture(type);
}

void QWidget_setWindowTitle(QWidgetH handle, PWideString AnonParam1)
{
	QString t_AnonParam1;
	copyPWideStringToQString(AnonParam1, t_AnonParam1);
	((QWidget *)handle)->setWindowTitle(t_AnonParam1);
}

void QWidget_setStyleSheet(QWidgetH handle, PWideString styleSheet)
{
	QString t_styleSheet;
	copyPWideStringToQString(styleSheet, t_styleSheet);
	((QWidget *)handle)->setStyleSheet(t_styleSheet);
}

void QWidget_styleSheet(QWidgetH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QWidget *)handle)->styleSheet();
	copyQStringToPWideString(t_retval, retval);
}

void QWidget_windowTitle(QWidgetH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QWidget *)handle)->windowTitle();
	copyQStringToPWideString(t_retval, retval);
}

void QWidget_setWindowIcon(QWidgetH handle, const QIconH icon)
{
	((QWidget *)handle)->setWindowIcon(*(const QIcon*)icon);
}

void QWidget_windowIcon(QWidgetH handle, QIconH retval)
{
	*(QIcon *)retval = ((QWidget *)handle)->windowIcon();
}

void QWidget_setWindowIconText(QWidgetH handle, PWideString AnonParam1)
{
	QString t_AnonParam1;
	copyPWideStringToQString(AnonParam1, t_AnonParam1);
	((QWidget *)handle)->setWindowIconText(t_AnonParam1);
}

void QWidget_windowIconText(QWidgetH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QWidget *)handle)->windowIconText();
	copyQStringToPWideString(t_retval, retval);
}

void QWidget_setWindowRole(QWidgetH handle, PWideString AnonParam1)
{
	QString t_AnonParam1;
	copyPWideStringToQString(AnonParam1, t_AnonParam1);
	((QWidget *)handle)->setWindowRole(t_AnonParam1);
}

void QWidget_windowRole(QWidgetH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QWidget *)handle)->windowRole();
	copyQStringToPWideString(t_retval, retval);
}

void QWidget_setWindowFilePath(QWidgetH handle, PWideString filePath)
{
	QString t_filePath;
	copyPWideStringToQString(filePath, t_filePath);
	((QWidget *)handle)->setWindowFilePath(t_filePath);
}

void QWidget_windowFilePath(QWidgetH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QWidget *)handle)->windowFilePath();
	copyQStringToPWideString(t_retval, retval);
}

void QWidget_setWindowOpacity(QWidgetH handle, qreal level)
{
	((QWidget *)handle)->setWindowOpacity(level);
}

qreal QWidget_windowOpacity(QWidgetH handle)
{
	return (qreal) ((QWidget *)handle)->windowOpacity();
}

bool QWidget_isWindowModified(QWidgetH handle)
{
	return (bool) ((QWidget *)handle)->isWindowModified();
}

void QWidget_setToolTip(QWidgetH handle, PWideString AnonParam1)
{
	QString t_AnonParam1;
	copyPWideStringToQString(AnonParam1, t_AnonParam1);
	((QWidget *)handle)->setToolTip(t_AnonParam1);
}

void QWidget_toolTip(QWidgetH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QWidget *)handle)->toolTip();
	copyQStringToPWideString(t_retval, retval);
}

void QWidget_setStatusTip(QWidgetH handle, PWideString AnonParam1)
{
	QString t_AnonParam1;
	copyPWideStringToQString(AnonParam1, t_AnonParam1);
	((QWidget *)handle)->setStatusTip(t_AnonParam1);
}

void QWidget_statusTip(QWidgetH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QWidget *)handle)->statusTip();
	copyQStringToPWideString(t_retval, retval);
}

void QWidget_setWhatsThis(QWidgetH handle, PWideString AnonParam1)
{
	QString t_AnonParam1;
	copyPWideStringToQString(AnonParam1, t_AnonParam1);
	((QWidget *)handle)->setWhatsThis(t_AnonParam1);
}

void QWidget_whatsThis(QWidgetH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QWidget *)handle)->whatsThis();
	copyQStringToPWideString(t_retval, retval);
}

void QWidget_accessibleName(QWidgetH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QWidget *)handle)->accessibleName();
	copyQStringToPWideString(t_retval, retval);
}

void QWidget_setAccessibleName(QWidgetH handle, PWideString name)
{
	QString t_name;
	copyPWideStringToQString(name, t_name);
	((QWidget *)handle)->setAccessibleName(t_name);
}

void QWidget_accessibleDescription(QWidgetH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QWidget *)handle)->accessibleDescription();
	copyQStringToPWideString(t_retval, retval);
}

void QWidget_setAccessibleDescription(QWidgetH handle, PWideString description)
{
	QString t_description;
	copyPWideStringToQString(description, t_description);
	((QWidget *)handle)->setAccessibleDescription(t_description);
}

void QWidget_setLayoutDirection(QWidgetH handle, Qt::LayoutDirection direction)
{
	((QWidget *)handle)->setLayoutDirection(direction);
}

Qt::LayoutDirection QWidget_layoutDirection(QWidgetH handle)
{
	return (Qt::LayoutDirection) ((QWidget *)handle)->layoutDirection();
}

void QWidget_unsetLayoutDirection(QWidgetH handle)
{
	((QWidget *)handle)->unsetLayoutDirection();
}

void QWidget_setLocale(QWidgetH handle, const QLocaleH locale)
{
	((QWidget *)handle)->setLocale(*(const QLocale*)locale);
}

void QWidget_locale(QWidgetH handle, QLocaleH retval)
{
	*(QLocale *)retval = ((QWidget *)handle)->locale();
}

void QWidget_unsetLocale(QWidgetH handle)
{
	((QWidget *)handle)->unsetLocale();
}

bool QWidget_isRightToLeft(QWidgetH handle)
{
	return (bool) ((QWidget *)handle)->isRightToLeft();
}

bool QWidget_isLeftToRight(QWidgetH handle)
{
	return (bool) ((QWidget *)handle)->isLeftToRight();
}

void QWidget_setFocus(QWidgetH handle)
{
	((QWidget *)handle)->setFocus();
}

bool QWidget_isActiveWindow(QWidgetH handle)
{
	return (bool) ((QWidget *)handle)->isActiveWindow();
}

void QWidget_activateWindow(QWidgetH handle)
{
	((QWidget *)handle)->activateWindow();
}

void QWidget_clearFocus(QWidgetH handle)
{
	((QWidget *)handle)->clearFocus();
}

void QWidget_setFocus2(QWidgetH handle, Qt::FocusReason reason)
{
	((QWidget *)handle)->setFocus(reason);
}

Qt::FocusPolicy QWidget_focusPolicy(QWidgetH handle)
{
	return (Qt::FocusPolicy) ((QWidget *)handle)->focusPolicy();
}

void QWidget_setFocusPolicy(QWidgetH handle, Qt::FocusPolicy policy)
{
	((QWidget *)handle)->setFocusPolicy(policy);
}

bool QWidget_hasFocus(QWidgetH handle)
{
	return (bool) ((QWidget *)handle)->hasFocus();
}

void QWidget_setTabOrder(QWidgetH AnonParam1, QWidgetH AnonParam2)
{
	QWidget::setTabOrder((QWidget*)AnonParam1, (QWidget*)AnonParam2);
}

void QWidget_setFocusProxy(QWidgetH handle, QWidgetH AnonParam1)
{
	((QWidget *)handle)->setFocusProxy((QWidget*)AnonParam1);
}

QWidgetH QWidget_focusProxy(QWidgetH handle)
{
	return (QWidgetH) ((QWidget *)handle)->focusProxy();
}

Qt::ContextMenuPolicy QWidget_contextMenuPolicy(QWidgetH handle)
{
	return (Qt::ContextMenuPolicy) ((QWidget *)handle)->contextMenuPolicy();
}

void QWidget_setContextMenuPolicy(QWidgetH handle, Qt::ContextMenuPolicy policy)
{
	((QWidget *)handle)->setContextMenuPolicy(policy);
}

void QWidget_grabMouse(QWidgetH handle)
{
	((QWidget *)handle)->grabMouse();
}

void QWidget_grabMouse2(QWidgetH handle, const QCursorH AnonParam1)
{
	((QWidget *)handle)->grabMouse(*(const QCursor*)AnonParam1);
}

void QWidget_releaseMouse(QWidgetH handle)
{
	((QWidget *)handle)->releaseMouse();
}

void QWidget_grabKeyboard(QWidgetH handle)
{
	((QWidget *)handle)->grabKeyboard();
}

void QWidget_releaseKeyboard(QWidgetH handle)
{
	((QWidget *)handle)->releaseKeyboard();
}

int QWidget_grabShortcut(QWidgetH handle, const QKeySequenceH key, Qt::ShortcutContext context)
{
	return (int) ((QWidget *)handle)->grabShortcut(*(const QKeySequence*)key, context);
}

void QWidget_releaseShortcut(QWidgetH handle, int id)
{
	((QWidget *)handle)->releaseShortcut(id);
}

void QWidget_setShortcutEnabled(QWidgetH handle, int id, bool enable)
{
	((QWidget *)handle)->setShortcutEnabled(id, enable);
}

void QWidget_setShortcutAutoRepeat(QWidgetH handle, int id, bool enable)
{
	((QWidget *)handle)->setShortcutAutoRepeat(id, enable);
}

QWidgetH QWidget_mouseGrabber()
{
	return (QWidgetH) QWidget::mouseGrabber();
}

QWidgetH QWidget_keyboardGrabber()
{
	return (QWidgetH) QWidget::keyboardGrabber();
}

bool QWidget_updatesEnabled(QWidgetH handle)
{
	return (bool) ((QWidget *)handle)->updatesEnabled();
}

void QWidget_setUpdatesEnabled(QWidgetH handle, bool enable)
{
	((QWidget *)handle)->setUpdatesEnabled(enable);
}

QGraphicsProxyWidgetH QWidget_graphicsProxyWidget(QWidgetH handle)
{
	return (QGraphicsProxyWidgetH) ((QWidget *)handle)->graphicsProxyWidget();
}

void QWidget_update(QWidgetH handle)
{
	((QWidget *)handle)->update();
}

void QWidget_repaint(QWidgetH handle)
{
	((QWidget *)handle)->repaint();
}

void QWidget_update2(QWidgetH handle, int x, int y, int w, int h)
{
	((QWidget *)handle)->update(x, y, w, h);
}

void QWidget_update3(QWidgetH handle, PRect AnonParam1)
{
	QRect t_AnonParam1;
	copyPRectToQRect(AnonParam1, t_AnonParam1);
	((QWidget *)handle)->update(t_AnonParam1);
}

void QWidget_update4(QWidgetH handle, const QRegionH AnonParam1)
{
	((QWidget *)handle)->update(*(const QRegion*)AnonParam1);
}

void QWidget_repaint2(QWidgetH handle, int x, int y, int w, int h)
{
	((QWidget *)handle)->repaint(x, y, w, h);
}

void QWidget_repaint3(QWidgetH handle, PRect AnonParam1)
{
	QRect t_AnonParam1;
	copyPRectToQRect(AnonParam1, t_AnonParam1);
	((QWidget *)handle)->repaint(t_AnonParam1);
}

void QWidget_repaint4(QWidgetH handle, const QRegionH AnonParam1)
{
	((QWidget *)handle)->repaint(*(const QRegion*)AnonParam1);
}

void QWidget_setVisible(QWidgetH handle, bool visible)
{
	((QWidget *)handle)->setVisible(visible);
}

void QWidget_setHidden(QWidgetH handle, bool hidden)
{
	((QWidget *)handle)->setHidden(hidden);
}

void QWidget_show(QWidgetH handle)
{
	((QWidget *)handle)->show();
}

void QWidget_hide(QWidgetH handle)
{
	((QWidget *)handle)->hide();
}

void QWidget_showMinimized(QWidgetH handle)
{
	((QWidget *)handle)->showMinimized();
}

void QWidget_showMaximized(QWidgetH handle)
{
	((QWidget *)handle)->showMaximized();
}

void QWidget_showFullScreen(QWidgetH handle)
{
	((QWidget *)handle)->showFullScreen();
}

void QWidget_showNormal(QWidgetH handle)
{
	((QWidget *)handle)->showNormal();
}

bool QWidget_close(QWidgetH handle)
{
	return (bool) ((QWidget *)handle)->close();
}

void QWidget_raise(QWidgetH handle)
{
	((QWidget *)handle)->raise();
}

void QWidget_lower(QWidgetH handle)
{
	((QWidget *)handle)->lower();
}

void QWidget_stackUnder(QWidgetH handle, QWidgetH AnonParam1)
{
	((QWidget *)handle)->stackUnder((QWidget*)AnonParam1);
}

void QWidget_move(QWidgetH handle, int x, int y)
{
	((QWidget *)handle)->move(x, y);
}

void QWidget_move2(QWidgetH handle, const QPointH AnonParam1)
{
	((QWidget *)handle)->move(*(const QPoint*)AnonParam1);
}

void QWidget_resize(QWidgetH handle, int w, int h)
{
	((QWidget *)handle)->resize(w, h);
}

void QWidget_resize2(QWidgetH handle, const QSizeH AnonParam1)
{
	((QWidget *)handle)->resize(*(const QSize*)AnonParam1);
}

void QWidget_setGeometry(QWidgetH handle, int x, int y, int w, int h)
{
	((QWidget *)handle)->setGeometry(x, y, w, h);
}

void QWidget_setGeometry2(QWidgetH handle, PRect AnonParam1)
{
	QRect t_AnonParam1;
	copyPRectToQRect(AnonParam1, t_AnonParam1);
	((QWidget *)handle)->setGeometry(t_AnonParam1);
}

void QWidget_saveGeometry(QWidgetH handle, QByteArrayH retval)
{
	*(QByteArray *)retval = ((QWidget *)handle)->saveGeometry();
}

bool QWidget_restoreGeometry(QWidgetH handle, const QByteArrayH geometry)
{
	return (bool) ((QWidget *)handle)->restoreGeometry(*(const QByteArray*)geometry);
}

void QWidget_adjustSize(QWidgetH handle)
{
	((QWidget *)handle)->adjustSize();
}

bool QWidget_isVisible(QWidgetH handle)
{
	return (bool) ((QWidget *)handle)->isVisible();
}

bool QWidget_isVisibleTo(QWidgetH handle, const QWidgetH AnonParam1)
{
	return (bool) ((QWidget *)handle)->isVisibleTo((const QWidget*)AnonParam1);
}

bool QWidget_isHidden(QWidgetH handle)
{
	return (bool) ((QWidget *)handle)->isHidden();
}

bool QWidget_isMinimized(QWidgetH handle)
{
	return (bool) ((QWidget *)handle)->isMinimized();
}

bool QWidget_isMaximized(QWidgetH handle)
{
	return (bool) ((QWidget *)handle)->isMaximized();
}

bool QWidget_isFullScreen(QWidgetH handle)
{
	return (bool) ((QWidget *)handle)->isFullScreen();
}

unsigned int QWidget_windowState(QWidgetH handle)
{
	return (unsigned int) ((QWidget *)handle)->windowState();
}

void QWidget_setWindowState(QWidgetH handle, unsigned int state)
{
	((QWidget *)handle)->setWindowState((Qt::WindowStates)state);
}

void QWidget_overrideWindowState(QWidgetH handle, unsigned int state)
{
	((QWidget *)handle)->overrideWindowState((Qt::WindowStates)state);
}

void QWidget_sizeHint(QWidgetH handle, PSize retval)
{
	*(QSize *)retval = ((QWidget *)handle)->sizeHint();
}

void QWidget_minimumSizeHint(QWidgetH handle, PSize retval)
{
	*(QSize *)retval = ((QWidget *)handle)->minimumSizeHint();
}

void QWidget_sizePolicy(QWidgetH handle, PQSizePolicy retval)
{
	*(QSizePolicy *)retval = ((QWidget *)handle)->sizePolicy();
}

void QWidget_setSizePolicy(QWidgetH handle, PQSizePolicy AnonParam1)
{
	((QWidget *)handle)->setSizePolicy(*(QSizePolicy *)AnonParam1);
}

void QWidget_setSizePolicy2(QWidgetH handle, QSizePolicy::Policy horizontal, QSizePolicy::Policy vertical)
{
	((QWidget *)handle)->setSizePolicy(horizontal, vertical);
}

int QWidget_heightForWidth(QWidgetH handle, int AnonParam1)
{
	return (int) ((QWidget *)handle)->heightForWidth(AnonParam1);
}

bool QWidget_hasHeightForWidth(QWidgetH handle)
{
	return (bool) ((QWidget *)handle)->hasHeightForWidth();
}

void QWidget_visibleRegion(QWidgetH handle, QRegionH retval)
{
	*(QRegion *)retval = ((QWidget *)handle)->visibleRegion();
}

void QWidget_setContentsMargins(QWidgetH handle, int left, int top, int right, int bottom)
{
	((QWidget *)handle)->setContentsMargins(left, top, right, bottom);
}

void QWidget_setContentsMargins2(QWidgetH handle, const QMarginsH margins)
{
	((QWidget *)handle)->setContentsMargins(*(const QMargins*)margins);
}

void QWidget_getContentsMargins(QWidgetH handle, int* left, int* top, int* right, int* bottom)
{
	((QWidget *)handle)->getContentsMargins(left, top, right, bottom);
}

void QWidget_contentsMargins(QWidgetH handle, QMarginsH retval)
{
	*(QMargins *)retval = ((QWidget *)handle)->contentsMargins();
}

void QWidget_contentsRect(QWidgetH handle, PRect retval)
{
	QRect t_retval;
	t_retval = ((QWidget *)handle)->contentsRect();
	copyQRectToPRect(t_retval, retval);
}

QLayoutH QWidget_layout(QWidgetH handle)
{
	return (QLayoutH) ((QWidget *)handle)->layout();
}

void QWidget_setLayout(QWidgetH handle, QLayoutH AnonParam1)
{
	((QWidget *)handle)->setLayout((QLayout*)AnonParam1);
}

void QWidget_updateGeometry(QWidgetH handle)
{
	((QWidget *)handle)->updateGeometry();
}

void QWidget_setParent(QWidgetH handle, QWidgetH parent)
{
	((QWidget *)handle)->setParent((QWidget*)parent);
}

void QWidget_setParent2(QWidgetH handle, QWidgetH parent, unsigned int f)
{
	((QWidget *)handle)->setParent((QWidget*)parent, (Qt::WindowFlags)f);
}

void QWidget_scroll(QWidgetH handle, int dx, int dy)
{
	((QWidget *)handle)->scroll(dx, dy);
}

void QWidget_scroll2(QWidgetH handle, int dx, int dy, PRect AnonParam3)
{
	QRect t_AnonParam3;
	copyPRectToQRect(AnonParam3, t_AnonParam3);
	((QWidget *)handle)->scroll(dx, dy, t_AnonParam3);
}

QWidgetH QWidget_focusWidget(QWidgetH handle)
{
	return (QWidgetH) ((QWidget *)handle)->focusWidget();
}

QWidgetH QWidget_nextInFocusChain(QWidgetH handle)
{
	return (QWidgetH) ((QWidget *)handle)->nextInFocusChain();
}

QWidgetH QWidget_previousInFocusChain(QWidgetH handle)
{
	return (QWidgetH) ((QWidget *)handle)->previousInFocusChain();
}

bool QWidget_acceptDrops(QWidgetH handle)
{
	return (bool) ((QWidget *)handle)->acceptDrops();
}

void QWidget_setAcceptDrops(QWidgetH handle, bool on)
{
	((QWidget *)handle)->setAcceptDrops(on);
}

void QWidget_addAction(QWidgetH handle, QActionH action)
{
	((QWidget *)handle)->addAction((QAction*)action);
}

void QWidget_addActions(QWidgetH handle, PPtrIntArray actions)
{
	QList<QAction*> t_actions;
	copyPtrIntArrayToQListTemplate(actions, t_actions);
	((QWidget *)handle)->addActions(t_actions);
}

void QWidget_insertAction(QWidgetH handle, QActionH before, QActionH action)
{
	((QWidget *)handle)->insertAction((QAction*)before, (QAction*)action);
}

void QWidget_insertActions(QWidgetH handle, QActionH before, PPtrIntArray actions)
{
	QList<QAction*> t_actions;
	copyPtrIntArrayToQListTemplate(actions, t_actions);
	((QWidget *)handle)->insertActions((QAction*)before, t_actions);
}

void QWidget_removeAction(QWidgetH handle, QActionH action)
{
	((QWidget *)handle)->removeAction((QAction*)action);
}

void QWidget_actions(QWidgetH handle, PPtrIntArray retval)
{
	QList<QAction*> t_retval;
	t_retval = ((QWidget *)handle)->actions();
	copyQListTemplateToPtrIntArray(t_retval, retval);
}

QWidgetH QWidget_parentWidget(QWidgetH handle)
{
	return (QWidgetH) ((QWidget *)handle)->parentWidget();
}

void QWidget_setWindowFlags(QWidgetH handle, unsigned int type)
{
	((QWidget *)handle)->setWindowFlags((Qt::WindowFlags)type);
}

unsigned int QWidget_windowFlags(QWidgetH handle)
{
	return (unsigned int) ((QWidget *)handle)->windowFlags();
}

void QWidget_overrideWindowFlags(QWidgetH handle, unsigned int type)
{
	((QWidget *)handle)->overrideWindowFlags((Qt::WindowFlags)type);
}

Qt::WindowType QWidget_windowType(QWidgetH handle)
{
	return (Qt::WindowType) ((QWidget *)handle)->windowType();
}

QWidgetH QWidget_find(WId AnonParam1)
{
	return (QWidgetH) QWidget::find((WId)AnonParam1);
}

QWidgetH QWidget_childAt(QWidgetH handle, int x, int y)
{
	return (QWidgetH) ((QWidget *)handle)->childAt(x, y);
}

QWidgetH QWidget_childAt2(QWidgetH handle, const QPointH p)
{
	return (QWidgetH) ((QWidget *)handle)->childAt(*(const QPoint*)p);
}

void QWidget_setAttribute(QWidgetH handle, Qt::WidgetAttribute AnonParam1, bool on)
{
	((QWidget *)handle)->setAttribute(AnonParam1, on);
}

bool QWidget_testAttribute(QWidgetH handle, Qt::WidgetAttribute AnonParam1)
{
	return (bool) ((QWidget *)handle)->testAttribute(AnonParam1);
}

QPaintEngineH QWidget_paintEngine(QWidgetH handle)
{
	return (QPaintEngineH) ((QWidget *)handle)->paintEngine();
}

void QWidget_ensurePolished(QWidgetH handle)
{
	((QWidget *)handle)->ensurePolished();
}

bool QWidget_isAncestorOf(QWidgetH handle, const QWidgetH child)
{
	return (bool) ((QWidget *)handle)->isAncestorOf((const QWidget*)child);
}

bool QWidget_autoFillBackground(QWidgetH handle)
{
	return (bool) ((QWidget *)handle)->autoFillBackground();
}

void QWidget_setAutoFillBackground(QWidgetH handle, bool enabled)
{
	((QWidget *)handle)->setAutoFillBackground(enabled);
}

QBackingStoreH QWidget_backingStore(QWidgetH handle)
{
	return (QBackingStoreH) ((QWidget *)handle)->backingStore();
}

QWindowH QWidget_windowHandle(QWidgetH handle)
{
	return (QWindowH) ((QWidget *)handle)->windowHandle();
}

QWidgetH QWidget_createWindowContainer(QWindowH window, QWidgetH parent, unsigned int flags)
{
	return (QWidgetH) QWidget::createWindowContainer((QWindow*)window, (QWidget*)parent, (Qt::WindowFlags)flags);
}

void QWidget_inputMethodQuery(QWidgetH handle, QVariantH retval, Qt::InputMethodQuery AnonParam1)
{
	*(QVariant *)retval = ((QWidget *)handle)->inputMethodQuery(AnonParam1);
}

unsigned int QWidget_inputMethodHints(QWidgetH handle)
{
	return (unsigned int) ((QWidget *)handle)->inputMethodHints();
}

void QWidget_setInputMethodHints(QWidgetH handle, unsigned int hints)
{
	((QWidget *)handle)->setInputMethodHints((Qt::InputMethodHints)hints);
}

QPaintDeviceH QWidget_to_QPaintDevice(QWidgetH handle)
{
	return (QPaintDeviceH)(QPaintDevice *)(QWidget *)handle;
}

