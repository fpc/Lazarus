//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qwebenginepage_c.h"

QWebEnginePageH QWebEnginePage_Create(QObjectH parent)
{
	return (QWebEnginePageH) new QWebEnginePage((QObject*)parent);
}

void QWebEnginePage_Destroy(QWebEnginePageH handle)
{
	delete (QWebEnginePage *)handle;
}

/* QWebFrameH QWebEnginePage_mainFrame(QWebEnginePageH handle)
{
	return (QWebFrameH) ((QWebEnginePage *)handle)->mainFrame();
}

QWebFrameH QWebEnginePage_currentFrame(QWebEnginePageH handle)
{
	return (QWebFrameH) ((QWebEnginePage *)handle)->currentFrame();
}

QWebFrameH QWebEnginePage_frameAt(QWebEnginePageH handle, const QPointH pos)
{
	return (QWebFrameH) ((QWebEnginePage *)handle)->frameAt(*(const QPoint*)pos);
}
*/
QWebEngineHistoryH QWebEnginePage_history(QWebEnginePageH handle)
{
	return (QWebEngineHistoryH) ((QWebEnginePage *)handle)->history();
}

QWebEngineSettingsH QWebEnginePage_settings(QWebEnginePageH handle)
{
	return (QWebEngineSettingsH) ((QWebEnginePage *)handle)->settings();
}

void QWebEnginePage_setView(QWebEnginePageH handle, QWidgetH view)
{
	((QWebEnginePage *)handle)->setView((QWidget*)view);
}

QWidgetH QWebEnginePage_view(QWebEnginePageH handle)
{
	return (QWidgetH) ((QWebEnginePage *)handle)->view();
}

/* bool QWebEnginePage_isModified(QWebEnginePageH handle)
{
	return (bool) ((QWebEnginePage *)handle)->isModified();
}

QUndoStackH QWebEnginePage_undoStack(QWebEnginePageH handle)
{
	return (QUndoStackH) ((QWebEnginePage *)handle)->undoStack();
}

void QWebEnginePage_setNetworkAccessManager(QWebEnginePageH handle, QNetworkAccessManagerH manager)
{
	((QWebEnginePage *)handle)->setNetworkAccessManager((QNetworkAccessManager*)manager);
}

QNetworkAccessManagerH QWebEnginePage_networkAccessManager(QWebEnginePageH handle)
{
	return (QNetworkAccessManagerH) ((QWebEnginePage *)handle)->networkAccessManager();
}

void QWebEnginePage_setPluginFactory(QWebEnginePageH handle, QWebPluginFactoryH factory)
{
	((QWebEnginePage *)handle)->setPluginFactory((QWebPluginFactory*)factory);
}

QWebPluginFactoryH QWebEnginePage_pluginFactory(QWebEnginePageH handle)
{
	return (QWebPluginFactoryH) ((QWebEnginePage *)handle)->pluginFactory();
}

quint64 QWebEnginePage_totalBytes(QWebEnginePageH handle)
{
	return (quint64) ((QWebEnginePage *)handle)->totalBytes();
}

quint64 QWebEnginePage_bytesReceived(QWebEnginePageH handle)
{
	return (quint64) ((QWebEnginePage *)handle)->bytesReceived();
}*/

bool QWebEnginePage_hasSelection(QWebEnginePageH handle)
{
	return (bool) ((QWebEnginePage *)handle)->hasSelection();
}

void QWebEnginePage_selectedText(QWebEnginePageH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QWebEnginePage *)handle)->selectedText();
	copyQStringToPWideString(t_retval, retval);
}

/* void QWebEnginePage_selectedHtml(QWebEnginePageH handle, PWideString retval)
{
	QString t_retval;
	t_retval = ((QWebEnginePage *)handle)->selectedHtml();
	copyQStringToPWideString(t_retval, retval);
}*/

QActionH QWebEnginePage_action(QWebEnginePageH handle, QWebEnginePage::WebAction action)
{
	return (QActionH) ((QWebEnginePage *)handle)->action(action);
}

void QWebEnginePage_triggerAction(QWebEnginePageH handle, QWebEnginePage::WebAction action, bool checked)
{
	((QWebEnginePage *)handle)->triggerAction(action, checked);
}

/* void QWebEnginePage_viewportSize(QWebEnginePageH handle, PSize retval)
{
	*(QSize *)retval = ((QWebEnginePage *)handle)->viewportSize();
}

void QWebEnginePage_setViewportSize(QWebEnginePageH handle, const QSizeH size)
{
	((QWebEnginePage *)handle)->setViewportSize(*(const QSize*)size);
}

// contentsSize() is in Qt-5.7, so nothing todo here from QWebEnginePage
void QWebEnginePage_preferredContentsSize(QWebEnginePageH handle, PSize retval)
{
	*(QSize *)retval = ((QWebEnginePage *)handle)->preferredContentsSize();
}

void QWebEnginePage_setPreferredContentsSize(QWebEnginePageH handle, const QSizeH size)
{
	((QWebEnginePage *)handle)->setPreferredContentsSize(*(const QSize*)size);
}


void QWebEnginePage_setActualVisibleContentRect(QWebEnginePageH handle, PRect rect)
{
	QRect t_rect;
	copyPRectToQRect(rect, t_rect);
	((QWebEnginePage *)handle)->setActualVisibleContentRect(t_rect);
}
*/

bool QWebEnginePage_event(QWebEnginePageH handle, QEventH AnonParam1)
{
	return (bool) ((QWebEnginePage *)handle)->event((QEvent*)AnonParam1);
}

/*
bool QWebEnginePage_focusNextPrevChild(QWebEnginePageH handle, bool next)
{
	return (bool) ((QWebEnginePage *)handle)->focusNextPrevChild(next);
}

void QWebEnginePage_inputMethodQuery(QWebEnginePageH handle, QVariantH retval, Qt::InputMethodQuery property)
{
	*(QVariant *)retval = ((QWebEnginePage *)handle)->inputMethodQuery(property);
}
*/
bool QWebEnginePage_findText(QWebEnginePageH handle, PWideString subString, unsigned int options)
{
	QString t_subString;
	copyPWideStringToQString(subString, t_subString);
	return (bool) ((QWebEnginePage *)handle)->findText(t_subString, (QWebEnginePage::FindFlags)options);
}

/*
void QWebEnginePage_setForwardUnsupportedContent(QWebEnginePageH handle, bool forward)
{
	((QWebEnginePage *)handle)->setForwardUnsupportedContent(forward);
}

bool QWebEnginePage_forwardUnsupportedContent(QWebEnginePageH handle)
{
	return (bool) ((QWebEnginePage *)handle)->forwardUnsupportedContent();
}

void QWebEnginePage_setLinkDelegationPolicy(QWebEnginePageH handle, QWebEnginePage::LinkDelegationPolicy policy)
{
	((QWebEnginePage *)handle)->setLinkDelegationPolicy(policy);
}

QWebEnginePage::LinkDelegationPolicy QWebEnginePage_linkDelegationPolicy(QWebEnginePageH handle)
{
	return (QWebEnginePage::LinkDelegationPolicy) ((QWebEnginePage *)handle)->linkDelegationPolicy();
}

void QWebEnginePage_setPalette(QWebEnginePageH handle, const QPaletteH palette)
{
	((QWebEnginePage *)handle)->setPalette(*(const QPalette*)palette);
}

void QWebEnginePage_palette(QWebEnginePageH handle, QPaletteH retval)
{
	*(QPalette *)retval = ((QWebEnginePage *)handle)->palette();
}



void QWebEnginePage_setContentEditable(QWebEnginePageH handle, bool editable)
{
	((QWebEnginePage *)handle)->setContentEditable(editable);
}

bool QWebEnginePage_isContentEditable(QWebEnginePageH handle)
{
	return (bool) ((QWebEnginePage *)handle)->isContentEditable();
}


bool QWebEnginePage_swallowContextMenuEvent(QWebEnginePageH handle, QContextMenuEventH event)
{
	return (bool) ((QWebEnginePage *)handle)->swallowContextMenuEvent((QContextMenuEvent*)event);
}

void QWebEnginePage_updatePositionDependentActions(QWebEnginePageH handle, const QPointH pos)
{
	((QWebEnginePage *)handle)->updatePositionDependentActions(*(const QPoint*)pos);
}

*/

QMenuH QWebEnginePage_createStandardContextMenu(QWebEnginePageH handle)
{
	return (QMenuH) ((QWebEnginePage *)handle)->createStandardContextMenu();
}

/* void QWebEnginePage_setFeaturePermission(QWebEnginePageH handle, QWebFrameH frame, QWebEnginePage::Feature feature, QWebEnginePage::PermissionPolicy policy)
{
	((QWebEnginePage *)handle)->setFeaturePermission((QWebFrame*)frame, feature, policy);
}*/

/*
void QWebEnginePage_supportedContentTypes(QWebEnginePageH handle, QStringListH retval)
{
	*(QStringList *)retval = ((QWebEnginePage *)handle)->supportedContentTypes();
}

bool QWebEnginePage_supportsContentType(QWebEnginePageH handle, PWideString mimeType)
{
	QString t_mimeType;
	copyPWideStringToQString(mimeType, t_mimeType);
	return (bool) ((QWebEnginePage *)handle)->supportsContentType(t_mimeType);
}

bool QWebEnginePage_supportsExtension(QWebEnginePageH handle, QWebEnginePage::Extension extension)
{
	return (bool) ((QWebEnginePage *)handle)->supportsExtension(extension);
}

bool QWebEnginePage_shouldInterruptJavaScript(QWebEnginePageH handle)
{
	return (bool) ((QWebEnginePage *)handle)->shouldInterruptJavaScript();
}
*/
