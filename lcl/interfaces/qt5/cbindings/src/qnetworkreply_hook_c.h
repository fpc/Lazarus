//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QNETWORKREPLY_HOOK_C_H
#define QNETWORKREPLY_HOOK_C_H

#include "qnetworkreply_hook.h"

C_EXPORT QNetworkReply_hookH QNetworkReply_hook_Create(QObjectH handle);
C_EXPORT void QNetworkReply_hook_Destroy(QNetworkReply_hookH handle);
C_EXPORT void QNetworkReply_hook_hook_metaDataChanged(QNetworkReply_hookH handle, QHookH hook);
C_EXPORT void QNetworkReply_hook_hook_finished(QNetworkReply_hookH handle, QHookH hook);
C_EXPORT void QNetworkReply_hook_hook_error(QNetworkReply_hookH handle, QHookH hook);
C_EXPORT void QNetworkReply_hook_hook_encrypted(QNetworkReply_hookH handle, QHookH hook);
C_EXPORT void QNetworkReply_hook_hook_uploadProgress(QNetworkReply_hookH handle, QHookH hook);
C_EXPORT void QNetworkReply_hook_hook_downloadProgress(QNetworkReply_hookH handle, QHookH hook);

#endif
