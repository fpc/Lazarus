//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QNETWORKREPLY_HOOK_H
#define QNETWORKREPLY_HOOK_H

#include <qnetworkreply.h>

#include "qiodevice_hook.h"

class QNetworkReply_hook : public QIODevice_hook {
  Q_OBJECT
  public:
    QNetworkReply_hook(QObject *handle) : QIODevice_hook(handle) {
      metaDataChanged_event.func = NULL;
      finished_event.func = NULL;
      error_event.func = NULL;
      encrypted_event.func = NULL;
      uploadProgress_event.func = NULL;
      downloadProgress_event.func = NULL;
    }
    void hook_metaDataChanged(QHook &hook) { 
      if ( !metaDataChanged_event.func )
        connect(handle, SIGNAL(metaDataChanged()), this, SLOT(metaDataChanged_hook()));
      metaDataChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(metaDataChanged()), this, SLOT(metaDataChanged_hook()));
    }
    void hook_finished(QHook &hook) { 
      if ( !finished_event.func )
        connect(handle, SIGNAL(finished()), this, SLOT(finished_hook()));
      finished_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(finished()), this, SLOT(finished_hook()));
    }
    void hook_error(QHook &hook) { 
      if ( !error_event.func )
        connect(handle, SIGNAL(error(QNetworkReply::NetworkError)), this, SLOT(error_hook(QNetworkReply::NetworkError)));
      error_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(error(QNetworkReply::NetworkError)), this, SLOT(error_hook(QNetworkReply::NetworkError)));
    }
    void hook_encrypted(QHook &hook) { 
      if ( !encrypted_event.func )
        connect(handle, SIGNAL(encrypted()), this, SLOT(encrypted_hook()));
      encrypted_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(encrypted()), this, SLOT(encrypted_hook()));
    }
    void hook_uploadProgress(QHook &hook) { 
      if ( !uploadProgress_event.func )
        connect(handle, SIGNAL(uploadProgress(qint64, qint64)), this, SLOT(uploadProgress_hook(qint64, qint64)));
      uploadProgress_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(uploadProgress(qint64, qint64)), this, SLOT(uploadProgress_hook(qint64, qint64)));
    }
    void hook_downloadProgress(QHook &hook) { 
      if ( !downloadProgress_event.func )
        connect(handle, SIGNAL(downloadProgress(qint64, qint64)), this, SLOT(downloadProgress_hook(qint64, qint64)));
      downloadProgress_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(downloadProgress(qint64, qint64)), this, SLOT(downloadProgress_hook(qint64, qint64)));
    }

  private slots:
    void metaDataChanged_hook() {
      if ( metaDataChanged_event.func ) {
        typedef void (*func_type)(void *data);
	(*(func_type)metaDataChanged_event.func)(metaDataChanged_event.data);
      }
    }
    void finished_hook() {
      if ( finished_event.func ) {
        typedef void (*func_type)(void *data);
	(*(func_type)finished_event.func)(finished_event.data);
      }
    }
    void error_hook(QNetworkReply::NetworkError AnonParam1) {
      if ( error_event.func ) {
        typedef void (*func_type)(void *data, QNetworkReply::NetworkError AnonParam1);
	(*(func_type)error_event.func)(error_event.data, AnonParam1);
      }
    }
    void encrypted_hook() {
      if ( encrypted_event.func ) {
        typedef void (*func_type)(void *data);
	(*(func_type)encrypted_event.func)(encrypted_event.data);
      }
    }
    void uploadProgress_hook(qint64 bytesSent, qint64 bytesTotal) {
      if ( uploadProgress_event.func ) {
        typedef void (*func_type)(void *data, qint64 bytesSent, qint64 bytesTotal);
	(*(func_type)uploadProgress_event.func)(uploadProgress_event.data, bytesSent, bytesTotal);
      }
    }
    void downloadProgress_hook(qint64 bytesReceived, qint64 bytesTotal) {
      if ( downloadProgress_event.func ) {
        typedef void (*func_type)(void *data, qint64 bytesReceived, qint64 bytesTotal);
	(*(func_type)downloadProgress_event.func)(downloadProgress_event.data, bytesReceived, bytesTotal);
      }
    }
  private:
    QHook metaDataChanged_event;
    QHook finished_event;
    QHook error_event;
    QHook encrypted_event;
    QHook uploadProgress_event;
    QHook downloadProgress_event;
};


#endif
