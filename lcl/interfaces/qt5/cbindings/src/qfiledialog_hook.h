//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QFILEDIALOG_HOOK_H
#define QFILEDIALOG_HOOK_H

#include <qfiledialog.h>

#include "qdialog_hook.h"

class QFileDialog_hook : public QDialog_hook {
  Q_OBJECT
  public:
    QFileDialog_hook(QObject *handle) : QDialog_hook(handle) {
      fileSelected_event.func = NULL;
      filesSelected_event.func = NULL;
      currentChanged_event.func = NULL;
      directoryEntered_event.func = NULL;
      filterSelected_event.func = NULL;
    }
    void hook_fileSelected(QHook &hook) { 
      if ( !fileSelected_event.func )
        connect(handle, SIGNAL(fileSelected(const QString&)), this, SLOT(fileSelected_hook(const QString&)));
      fileSelected_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(fileSelected(const QString&)), this, SLOT(fileSelected_hook(const QString&)));
    }
    void hook_filesSelected(QHook &hook) { 
      if ( !filesSelected_event.func )
        connect(handle, SIGNAL(filesSelected(const QStringList&)), this, SLOT(filesSelected_hook(const QStringList&)));
      filesSelected_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(filesSelected(const QStringList&)), this, SLOT(filesSelected_hook(const QStringList&)));
    }
    void hook_currentChanged(QHook &hook) { 
      if ( !currentChanged_event.func )
        connect(handle, SIGNAL(currentChanged(const QString&)), this, SLOT(currentChanged_hook(const QString&)));
      currentChanged_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(currentChanged(const QString&)), this, SLOT(currentChanged_hook(const QString&)));
    }
    void hook_directoryEntered(QHook &hook) { 
      if ( !directoryEntered_event.func )
        connect(handle, SIGNAL(directoryEntered(const QString&)), this, SLOT(directoryEntered_hook(const QString&)));
      directoryEntered_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(directoryEntered(const QString&)), this, SLOT(directoryEntered_hook(const QString&)));
    }
    void hook_filterSelected(QHook &hook) { 
      if ( !filterSelected_event.func )
        connect(handle, SIGNAL(filterSelected(const QString&)), this, SLOT(filterSelected_hook(const QString&)));
      filterSelected_event = hook;
      if ( !hook.func )
        disconnect(handle, SIGNAL(filterSelected(const QString&)), this, SLOT(filterSelected_hook(const QString&)));
    }

  private slots:
    void fileSelected_hook(const QString& file) {
      if ( fileSelected_event.func ) {
        typedef void (*func_type)(void *data, PWideString file);
	PWideString t_file;
	initializePWideString(t_file);
	copyQStringToPWideString(file, t_file);
	(*(func_type)fileSelected_event.func)(fileSelected_event.data, t_file);
	finalizePWideString(t_file);
      }
    }
    void filesSelected_hook(const QStringList& files) {
      if ( filesSelected_event.func ) {
        typedef void (*func_type)(void *data, const QStringListH files);
	(*(func_type)filesSelected_event.func)(filesSelected_event.data, (const QStringListH)&files);
      }
    }
    void currentChanged_hook(const QString& path) {
      if ( currentChanged_event.func ) {
        typedef void (*func_type)(void *data, PWideString path);
	PWideString t_path;
	initializePWideString(t_path);
	copyQStringToPWideString(path, t_path);
	(*(func_type)currentChanged_event.func)(currentChanged_event.data, t_path);
	finalizePWideString(t_path);
      }
    }
    void directoryEntered_hook(const QString& directory) {
      if ( directoryEntered_event.func ) {
        typedef void (*func_type)(void *data, PWideString directory);
	PWideString t_directory;
	initializePWideString(t_directory);
	copyQStringToPWideString(directory, t_directory);
	(*(func_type)directoryEntered_event.func)(directoryEntered_event.data, t_directory);
	finalizePWideString(t_directory);
      }
    }
    void filterSelected_hook(const QString& filter) {
      if ( filterSelected_event.func ) {
        typedef void (*func_type)(void *data, PWideString filter);
	PWideString t_filter;
	initializePWideString(t_filter);
	copyQStringToPWideString(filter, t_filter);
	(*(func_type)filterSelected_event.func)(filterSelected_event.data, t_filter);
	finalizePWideString(t_filter);
      }
    }
  private:
    QHook fileSelected_event;
    QHook filesSelected_event;
    QHook currentChanged_event;
    QHook directoryEntered_event;
    QHook filterSelected_event;
};


#endif
