//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qwebpage_c.h"

QWebPageH QWebPage_Create(QObjectH parent)
{
	return (QWebPageH) new QWebPage((QObject*)parent);
}

void QWebPage_Destroy(QWebPageH handle)
{
	delete (QWebPage *)handle;
}

QWebFrameH QWebPage_mainFrame(QWebPageH handle)
{
	return (QWebFrameH) ((QWebPage *)handle)->mainFrame();
}

QWebFrameH QWebPage_currentFrame(QWebPageH handle)
{
	return (QWebFrameH) ((QWebPage *)handle)->currentFrame();
}

QWebFrameH QWebPage_frameAt(QWebPageH handle, const QPointH pos)
{
	return (QWebFrameH) ((QWebPage *)handle)->frameAt(*(const QPoint*)pos);
}

QWebHistoryH QWebPage_history(QWebPageH handle)
{
	return (QWebHistoryH) ((QWebPage *)handle)->history();
}

QWebSettingsH QWebPage_settings(QWebPageH handle)
{
	return (QWebSettingsH) ((QWebPage *)handle)->settings();
}

void QWebPage_setView(QWebPageH handle, QWidgetH view)
{
	((QWebPage *)handle)->setView((QWidget*)view);
}

QWidgetH QWebPage_view(QWebPageH handle)
{
	return (QWidgetH) ((QWebPage *)handle)->view();
}

bool QWebPage_isModified(QWebPageH handle)
{
	return (bool) ((QWebPage *)handle)->isModified();
}

QUndoStackH QWebPage_undoStack(QWebPageH handle)
{
	return (QUndoStackH) ((QWebPage *)handle)->undoStack();
}

void QWebPage_setNetworkAccessManager(QWebPageH handle, QNetworkAccessManagerH manager)
{
	((QWebPage *)handle)->setNetworkAccessManager((QNetworkAccessManager*)manager);
}

QNetworkAccessManagerH QWebPage_networkAccessManager(QWebPageH handle)
{
	return (QNetworkAccessManagerH) ((QWebPage *)handle)->networkAccessManager();
}

void QWebPage_setPluginFactory(QWebPageH handle, QWebPluginFactoryH factory)
{
	((QWebPage *)handle)->setPluginFactory((QWebPluginFactory*)factory);
}

QWebPluginFactoryH QWebPage_pluginFactory(QWebPageH handle)
{
	return (QWebPluginFactoryH) ((QWebPage *)handle)->pluginFactory();
}

quint64 QWebPage_totalBytes(QWebPageH handle)
{
	return (quint64) ((QWebPage *)handle)->totalBytes();
}

quint64 QWebPage_bytesReceived(QWebPageH handle)
{
	return (quint64) ((QWebPage *)handle)->bytesReceived();
}

bool QWebPage_hasSelection(QWebPageH handle)
{
	return (bool) ((QWebPage *)handle)->hasSelection();
}

void QWebPage_selectedText(QWebPageH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QWebPage *)handle)->selectedText();
	copyQStringToPWideString(t_retval, retval);
}

void QWebPage_selectedHtml(QWebPageH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QWebPage *)handle)->selectedHtml();
	copyQStringToPWideString(t_retval, retval);
}

QActionH QWebPage_action(QWebPageH handle, QWebPage::WebAction action)
{
	return (QActionH) ((QWebPage *)handle)->action(action);
}

void QWebPage_triggerAction(QWebPageH handle, QWebPage::WebAction action, bool checked)
{
	((QWebPage *)handle)->triggerAction(action, checked);
}

void QWebPage_viewportSize(QWebPageH handle, PSize retval)
{
	*(QSize *)retval = ((QWebPage *)handle)->viewportSize();
}

void QWebPage_setViewportSize(QWebPageH handle, const QSizeH size)
{
	((QWebPage *)handle)->setViewportSize(*(const QSize*)size);
}

void QWebPage_preferredContentsSize(QWebPageH handle, PSize retval)
{
	*(QSize *)retval = ((QWebPage *)handle)->preferredContentsSize();
}

void QWebPage_setPreferredContentsSize(QWebPageH handle, const QSizeH size)
{
	((QWebPage *)handle)->setPreferredContentsSize(*(const QSize*)size);
}

void QWebPage_setActualVisibleContentRect(QWebPageH handle, PRect rect)
{
	QRect t_rect;
	copyPRectToQRect(rect, t_rect);
	((QWebPage *)handle)->setActualVisibleContentRect(t_rect);
}

bool QWebPage_event(QWebPageH handle, QEventH AnonParam1)
{
	return (bool) ((QWebPage *)handle)->event((QEvent*)AnonParam1);
}

bool QWebPage_focusNextPrevChild(QWebPageH handle, bool next)
{
	return (bool) ((QWebPage *)handle)->focusNextPrevChild(next);
}

void QWebPage_inputMethodQuery(QWebPageH handle, QVariantH retval, Qt::InputMethodQuery property)
{
	*(QVariant *)retval = ((QWebPage *)handle)->inputMethodQuery(property);
}

bool QWebPage_findText(QWebPageH handle, PWideString subString, unsigned int options)
{
	QString t_subString;
	copyPWideStringToQString(subString, t_subString);
	return (bool) ((QWebPage *)handle)->findText(t_subString, (QWebPage::FindFlags)options);
}

void QWebPage_setForwardUnsupportedContent(QWebPageH handle, bool forward)
{
	((QWebPage *)handle)->setForwardUnsupportedContent(forward);
}

bool QWebPage_forwardUnsupportedContent(QWebPageH handle)
{
	return (bool) ((QWebPage *)handle)->forwardUnsupportedContent();
}

void QWebPage_setLinkDelegationPolicy(QWebPageH handle, QWebPage::LinkDelegationPolicy policy)
{
	((QWebPage *)handle)->setLinkDelegationPolicy(policy);
}

QWebPage::LinkDelegationPolicy QWebPage_linkDelegationPolicy(QWebPageH handle)
{
	return (QWebPage::LinkDelegationPolicy) ((QWebPage *)handle)->linkDelegationPolicy();
}

void QWebPage_setPalette(QWebPageH handle, const QPaletteH palette)
{
	((QWebPage *)handle)->setPalette(*(const QPalette*)palette);
}

void QWebPage_palette(QWebPageH handle, QPaletteH retval)
{
	*(QPalette *)retval = ((QWebPage *)handle)->palette();
}

void QWebPage_setContentEditable(QWebPageH handle, bool editable)
{
	((QWebPage *)handle)->setContentEditable(editable);
}

bool QWebPage_isContentEditable(QWebPageH handle)
{
	return (bool) ((QWebPage *)handle)->isContentEditable();
}

bool QWebPage_swallowContextMenuEvent(QWebPageH handle, QContextMenuEventH event)
{
	return (bool) ((QWebPage *)handle)->swallowContextMenuEvent((QContextMenuEvent*)event);
}

void QWebPage_updatePositionDependentActions(QWebPageH handle, const QPointH pos)
{
	((QWebPage *)handle)->updatePositionDependentActions(*(const QPoint*)pos);
}

QMenuH QWebPage_createStandardContextMenu(QWebPageH handle)
{
	return (QMenuH) ((QWebPage *)handle)->createStandardContextMenu();
}

void QWebPage_setFeaturePermission(QWebPageH handle, QWebFrameH frame, QWebPage::Feature feature, QWebPage::PermissionPolicy policy)
{
	((QWebPage *)handle)->setFeaturePermission((QWebFrame*)frame, feature, policy);
}

void QWebPage_supportedContentTypes(QWebPageH handle, QStringListH retval)
{
	*(QStringList *)retval = ((QWebPage *)handle)->supportedContentTypes();
}

bool QWebPage_supportsContentType(QWebPageH handle, PWideString mimeType)
{
	QString t_mimeType;
	copyPWideStringToQString(mimeType, t_mimeType);
	return (bool) ((QWebPage *)handle)->supportsContentType(t_mimeType);
}

bool QWebPage_supportsExtension(QWebPageH handle, QWebPage::Extension extension)
{
	return (bool) ((QWebPage *)handle)->supportsExtension(extension);
}

bool QWebPage_shouldInterruptJavaScript(QWebPageH handle)
{
	return (bool) ((QWebPage *)handle)->shouldInterruptJavaScript();
}

