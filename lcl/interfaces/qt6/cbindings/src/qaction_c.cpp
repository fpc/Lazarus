//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qaction_c.h"

QActionH QAction_Create(QObjectH parent)
{
	return (QActionH) new QAction((QObject*)parent);
}

void QAction_Destroy(QActionH handle)
{
	delete (QAction *)handle;
}

QActionH QAction_Create2(PWideString text, QObjectH parent)
{
	QString t_text;
	copyPWideStringToQString(text, t_text);
	return (QActionH) new QAction(t_text, (QObject*)parent);
}

QActionH QAction_Create3(const QIconH icon, PWideString text, QObjectH parent)
{
	QString t_text;
	copyPWideStringToQString(text, t_text);
	return (QActionH) new QAction(*(const QIcon*)icon, t_text, (QObject*)parent);
}

void QAction_setActionGroup(QActionH handle, QActionGroupH group)
{
	((QAction *)handle)->setActionGroup((QActionGroup*)group);
}

QActionGroupH QAction_actionGroup(QActionH handle)
{
	return (QActionGroupH) ((QAction *)handle)->actionGroup();
}

void QAction_setIcon(QActionH handle, const QIconH icon)
{
	((QAction *)handle)->setIcon(*(const QIcon*)icon);
}

void QAction_icon(QActionH handle, QIconH retval)
{
	*(QIcon *)retval = ((QAction *)handle)->icon();
}

void QAction_setText(QActionH handle, PWideString text)
{
	QString t_text;
	copyPWideStringToQString(text, t_text);
	((QAction *)handle)->setText(t_text);
}

void QAction_text(QActionH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QAction *)handle)->text();
	copyQStringToPWideString(t_retval, retval);
}

void QAction_setIconText(QActionH handle, PWideString text)
{
	QString t_text;
	copyPWideStringToQString(text, t_text);
	((QAction *)handle)->setIconText(t_text);
}

void QAction_iconText(QActionH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QAction *)handle)->iconText();
	copyQStringToPWideString(t_retval, retval);
}

void QAction_setToolTip(QActionH handle, PWideString tip)
{
	QString t_tip;
	copyPWideStringToQString(tip, t_tip);
	((QAction *)handle)->setToolTip(t_tip);
}

void QAction_toolTip(QActionH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QAction *)handle)->toolTip();
	copyQStringToPWideString(t_retval, retval);
}

void QAction_setStatusTip(QActionH handle, PWideString statusTip)
{
	QString t_statusTip;
	copyPWideStringToQString(statusTip, t_statusTip);
	((QAction *)handle)->setStatusTip(t_statusTip);
}

void QAction_statusTip(QActionH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QAction *)handle)->statusTip();
	copyQStringToPWideString(t_retval, retval);
}

void QAction_setWhatsThis(QActionH handle, PWideString what)
{
	QString t_what;
	copyPWideStringToQString(what, t_what);
	((QAction *)handle)->setWhatsThis(t_what);
}

void QAction_whatsThis(QActionH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QAction *)handle)->whatsThis();
	copyQStringToPWideString(t_retval, retval);
}

void QAction_setPriority(QActionH handle, QAction::Priority priority)
{
	((QAction *)handle)->setPriority(priority);
}

QAction::Priority QAction_priority(QActionH handle)
{
	return (QAction::Priority) ((QAction *)handle)->priority();
}

QMenuH QAction_menu(QActionH handle)
{
	return (QMenuH) ((QAction *)handle)->menu();
}

void QAction_setMenu(QActionH handle, QMenuH menu)
{
	((QAction *)handle)->setMenu((QMenu*)menu);
}

void QAction_setSeparator(QActionH handle, bool b)
{
	((QAction *)handle)->setSeparator(b);
}

bool QAction_isSeparator(QActionH handle)
{
	return (bool) ((QAction *)handle)->isSeparator();
}

void QAction_setShortcut(QActionH handle, const QKeySequenceH shortcut)
{
	((QAction *)handle)->setShortcut(*(const QKeySequence*)shortcut);
}

void QAction_shortcut(QActionH handle, QKeySequenceH retval)
{
	*(QKeySequence *)retval = ((QAction *)handle)->shortcut();
}

void QAction_setShortcutContext(QActionH handle, Qt::ShortcutContext context)
{
	((QAction *)handle)->setShortcutContext(context);
}

Qt::ShortcutContext QAction_shortcutContext(QActionH handle)
{
	return (Qt::ShortcutContext) ((QAction *)handle)->shortcutContext();
}

void QAction_setAutoRepeat(QActionH handle, bool AnonParam1)
{
	((QAction *)handle)->setAutoRepeat(AnonParam1);
}

bool QAction_autoRepeat(QActionH handle)
{
	return (bool) ((QAction *)handle)->autoRepeat();
}

void QAction_setFont(QActionH handle, const QFontH font)
{
	((QAction *)handle)->setFont(*(const QFont*)font);
}

void QAction_font(QActionH handle, QFontH retval)
{
	*(QFont *)retval = ((QAction *)handle)->font();
}

void QAction_setCheckable(QActionH handle, bool AnonParam1)
{
	((QAction *)handle)->setCheckable(AnonParam1);
}

bool QAction_isCheckable(QActionH handle)
{
	return (bool) ((QAction *)handle)->isCheckable();
}

void QAction_data(QActionH handle, QVariantH retval)
{
	*(QVariant *)retval = ((QAction *)handle)->data();
}

void QAction_setData(QActionH handle, const QVariantH var)
{
	((QAction *)handle)->setData(*(const QVariant*)var);
}

bool QAction_isChecked(QActionH handle)
{
	return (bool) ((QAction *)handle)->isChecked();
}

bool QAction_isEnabled(QActionH handle)
{
	return (bool) ((QAction *)handle)->isEnabled();
}

bool QAction_isVisible(QActionH handle)
{
	return (bool) ((QAction *)handle)->isVisible();
}

void QAction_activate(QActionH handle, QAction::ActionEvent event)
{
	((QAction *)handle)->activate(event);
}

bool QAction_showStatusText(QActionH handle, QWidgetH widget)
{
	return (bool) ((QAction *)handle)->showStatusText((QWidget*)widget);
}

void QAction_setMenuRole(QActionH handle, QAction::MenuRole menuRole)
{
	((QAction *)handle)->setMenuRole(menuRole);
}

QAction::MenuRole QAction_menuRole(QActionH handle)
{
	return (QAction::MenuRole) ((QAction *)handle)->menuRole();
}

void QAction_setIconVisibleInMenu(QActionH handle, bool visible)
{
	((QAction *)handle)->setIconVisibleInMenu(visible);
}

bool QAction_isIconVisibleInMenu(QActionH handle)
{
	return (bool) ((QAction *)handle)->isIconVisibleInMenu();
}

QWidgetH QAction_parentWidget(QActionH handle)
{
	return (QWidgetH) ((QAction *)handle)->parentWidget();
}

void QAction_associatedWidgets(QActionH handle, PPtrIntArray retval)
{
	QList<QWidget*> t_retval;
	t_retval = ((QAction *)handle)->associatedWidgets();
	copyQListTemplateToPtrIntArray(t_retval, retval);
}

void QAction_associatedGraphicsWidgets(QActionH handle, PPtrIntArray retval)
{
	QList<QGraphicsWidget*> t_retval;
	t_retval = ((QAction *)handle)->associatedGraphicsWidgets();
	copyQListTemplateToPtrIntArray(t_retval, retval);
}

void QAction_trigger(QActionH handle)
{
	((QAction *)handle)->trigger();
}

void QAction_hover(QActionH handle)
{
	((QAction *)handle)->hover();
}

void QAction_setChecked(QActionH handle, bool AnonParam1)
{
	((QAction *)handle)->setChecked(AnonParam1);
}

void QAction_toggle(QActionH handle)
{
	((QAction *)handle)->toggle();
}

void QAction_setEnabled(QActionH handle, bool AnonParam1)
{
	((QAction *)handle)->setEnabled(AnonParam1);
}

void QAction_setDisabled(QActionH handle, bool b)
{
	((QAction *)handle)->setDisabled(b);
}

void QAction_setVisible(QActionH handle, bool AnonParam1)
{
	((QAction *)handle)->setVisible(AnonParam1);
}

