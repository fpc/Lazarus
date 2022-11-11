//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QWEBPAGE_C_H
#define QWEBPAGE_C_H

#include <QtWebKitWidgets>
#include "pascalbind.h"

C_EXPORT QWebPageH QWebPage_Create(QObjectH parent);
C_EXPORT void QWebPage_Destroy(QWebPageH handle);
C_EXPORT QWebFrameH QWebPage_mainFrame(QWebPageH handle);
C_EXPORT QWebFrameH QWebPage_currentFrame(QWebPageH handle);
C_EXPORT QWebFrameH QWebPage_frameAt(QWebPageH handle, const QPointH pos);
C_EXPORT QWebHistoryH QWebPage_history(QWebPageH handle);
C_EXPORT QWebSettingsH QWebPage_settings(QWebPageH handle);
C_EXPORT void QWebPage_setView(QWebPageH handle, QWidgetH view);
C_EXPORT QWidgetH QWebPage_view(QWebPageH handle);
C_EXPORT bool QWebPage_isModified(QWebPageH handle);
C_EXPORT QUndoStackH QWebPage_undoStack(QWebPageH handle);
C_EXPORT void QWebPage_setNetworkAccessManager(QWebPageH handle, QNetworkAccessManagerH manager);
C_EXPORT QNetworkAccessManagerH QWebPage_networkAccessManager(QWebPageH handle);
C_EXPORT void QWebPage_setPluginFactory(QWebPageH handle, QWebPluginFactoryH factory);
C_EXPORT QWebPluginFactoryH QWebPage_pluginFactory(QWebPageH handle);
C_EXPORT quint64 QWebPage_totalBytes(QWebPageH handle);
C_EXPORT quint64 QWebPage_bytesReceived(QWebPageH handle);
C_EXPORT bool QWebPage_hasSelection(QWebPageH handle);
C_EXPORT void QWebPage_selectedText(QWebPageH handle, PWideString retval);
C_EXPORT void QWebPage_selectedHtml(QWebPageH handle, PWideString retval);
C_EXPORT QActionH QWebPage_action(QWebPageH handle, QWebPage::WebAction action);
C_EXPORT void QWebPage_triggerAction(QWebPageH handle, QWebPage::WebAction action, bool checked);
C_EXPORT void QWebPage_viewportSize(QWebPageH handle, PSize retval);
C_EXPORT void QWebPage_setViewportSize(QWebPageH handle, const QSizeH size);
C_EXPORT void QWebPage_preferredContentsSize(QWebPageH handle, PSize retval);
C_EXPORT void QWebPage_setPreferredContentsSize(QWebPageH handle, const QSizeH size);
C_EXPORT void QWebPage_setActualVisibleContentRect(QWebPageH handle, PRect rect);
C_EXPORT bool QWebPage_event(QWebPageH handle, QEventH AnonParam1);
C_EXPORT bool QWebPage_focusNextPrevChild(QWebPageH handle, bool next);
C_EXPORT void QWebPage_inputMethodQuery(QWebPageH handle, QVariantH retval, Qt::InputMethodQuery property);
C_EXPORT bool QWebPage_findText(QWebPageH handle, PWideString subString, unsigned int options);
C_EXPORT void QWebPage_setForwardUnsupportedContent(QWebPageH handle, bool forward);
C_EXPORT bool QWebPage_forwardUnsupportedContent(QWebPageH handle);
C_EXPORT void QWebPage_setLinkDelegationPolicy(QWebPageH handle, QWebPage::LinkDelegationPolicy policy);
C_EXPORT QWebPage::LinkDelegationPolicy QWebPage_linkDelegationPolicy(QWebPageH handle);
C_EXPORT void QWebPage_setPalette(QWebPageH handle, const QPaletteH palette);
C_EXPORT void QWebPage_palette(QWebPageH handle, QPaletteH retval);
C_EXPORT void QWebPage_setContentEditable(QWebPageH handle, bool editable);
C_EXPORT bool QWebPage_isContentEditable(QWebPageH handle);
C_EXPORT bool QWebPage_swallowContextMenuEvent(QWebPageH handle, QContextMenuEventH event);
C_EXPORT void QWebPage_updatePositionDependentActions(QWebPageH handle, const QPointH pos);
C_EXPORT QMenuH QWebPage_createStandardContextMenu(QWebPageH handle);
C_EXPORT void QWebPage_setFeaturePermission(QWebPageH handle, QWebFrameH frame, QWebPage::Feature feature, QWebPage::PermissionPolicy policy);
C_EXPORT void QWebPage_supportedContentTypes(QWebPageH handle, QStringListH retval);
C_EXPORT bool QWebPage_supportsContentType(QWebPageH handle, PWideString mimeType);
C_EXPORT bool QWebPage_supportsExtension(QWebPageH handle, QWebPage::Extension extension);
C_EXPORT bool QWebPage_shouldInterruptJavaScript(QWebPageH handle);

#endif
