//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QWEBENGINEPAGE_C_H
#define QWEBENGINEPAGE_C_H

#include <QtWebEngineWidgets>
#include "pascalbind.h"

C_EXPORT QWebEnginePageH QWebEnginePage_Create(QObjectH parent);
C_EXPORT void QWebEnginePage_Destroy(QWebEnginePageH handle);
C_EXPORT QWebEngineHistoryH QWebEnginePage_history(QWebEnginePageH handle);
C_EXPORT QWebEngineSettingsH QWebEnginePage_settings(QWebEnginePageH handle);
C_EXPORT void QWebEnginePage_setView(QWebEnginePageH handle, QWidgetH view);
C_EXPORT QWidgetH QWebEnginePage_view(QWebEnginePageH handle);
/*C_EXPORT bool QWebEnginePage_isModified(QWebEnginePageH handle);
C_EXPORT QUndoStackH QWebEnginePage_undoStack(QWebEnginePageH handle);
C_EXPORT void QWebEnginePage_setNetworkAccessManager(QWebEnginePageH handle, QNetworkAccessManagerH manager);
C_EXPORT QNetworkAccessManagerH QWebEnginePage_networkAccessManager(QWebEnginePageH handle);
C_EXPORT void QWebEnginePage_setPluginFactory(QWebEnginePageH handle, QWebPluginFactoryH factory);
C_EXPORT QWebPluginFactoryH QWebEnginePage_pluginFactory(QWebEnginePageH handle);
C_EXPORT quint64 QWebEnginePage_totalBytes(QWebEnginePageH handle);
C_EXPORT quint64 QWebEnginePage_bytesReceived(QWebEnginePageH handle);
*/
C_EXPORT bool QWebEnginePage_hasSelection(QWebEnginePageH handle);
C_EXPORT void QWebEnginePage_selectedText(QWebEnginePageH handle, PWideString retval);
/* C_EXPORT void QWebEnginePage_selectedHtml(QWebEnginePageH handle, PWideString retval); */
C_EXPORT QActionH QWebEnginePage_action(QWebEnginePageH handle, QWebEnginePage::WebAction action);
C_EXPORT void QWebEnginePage_triggerAction(QWebEnginePageH handle, QWebEnginePage::WebAction action, bool checked);
/*C_EXPORT void QWebEnginePage_viewportSize(QWebEnginePageH handle, PSize retval);
C_EXPORT void QWebEnginePage_setViewportSize(QWebEnginePageH handle, const QSizeH size);
C_EXPORT void QWebEnginePage_preferredContentsSize(QWebEnginePageH handle, PSize retval);
C_EXPORT void QWebEnginePage_setPreferredContentsSize(QWebEnginePageH handle, const QSizeH size);
C_EXPORT void QWebEnginePage_setActualVisibleContentRect(QWebEnginePageH handle, PRect rect); */
C_EXPORT bool QWebEnginePage_event(QWebEnginePageH handle, QEventH AnonParam1);
/*C_EXPORT bool QWebEnginePage_focusNextPrevChild(QWebEnginePageH handle, bool next);
C_EXPORT void QWebEnginePage_inputMethodQuery(QWebEnginePageH handle, QVariantH retval, Qt::InputMethodQuery property);*/
C_EXPORT bool QWebEnginePage_findText(QWebEnginePageH handle, PWideString subString, unsigned int options);
/*C_EXPORT void QWebEnginePage_setForwardUnsupportedContent(QWebEnginePageH handle, bool forward);
C_EXPORT bool QWebEnginePage_forwardUnsupportedContent(QWebEnginePageH handle);
C_EXPORT void QWebEnginePage_setLinkDelegationPolicy(QWebEnginePageH handle, QWebEnginePage::LinkDelegationPolicy policy);
C_EXPORT QWebEnginePage::LinkDelegationPolicy QWebEnginePage_linkDelegationPolicy(QWebEnginePageH handle);
C_EXPORT void QWebEnginePage_setPalette(QWebEnginePageH handle, const QPaletteH palette);
C_EXPORT void QWebEnginePage_palette(QWebEnginePageH handle, QPaletteH retval);
C_EXPORT void QWebEnginePage_setContentEditable(QWebEnginePageH handle, bool editable);
C_EXPORT bool QWebEnginePage_isContentEditable(QWebEnginePageH handle);
C_EXPORT bool QWebEnginePage_swallowContextMenuEvent(QWebEnginePageH handle, QContextMenuEventH event);
C_EXPORT void QWebEnginePage_updatePositionDependentActions(QWebEnginePageH handle, const QPointH pos);*/
C_EXPORT QMenuH QWebEnginePage_createStandardContextMenu(QWebEnginePageH handle);
/* C_EXPORT void QWebEnginePage_supportedContentTypes(QWebEnginePageH handle, QStringListH retval);
C_EXPORT bool QWebEnginePage_supportsContentType(QWebEnginePageH handle, PWideString mimeType);
C_EXPORT bool QWebEnginePage_supportsExtension(QWebEnginePageH handle, QWebEnginePage::Extension extension);
C_EXPORT bool QWebEnginePage_shouldInterruptJavaScript(QWebEnginePageH handle); */

#endif
