//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QCOREAPPLICATION_C_H
#define QCOREAPPLICATION_C_H

#include <QtCore>
#include "pascalbind.h"

C_EXPORT QCoreApplicationH QCoreApplication_Create(int* argc, char** argv, int AnonParam3);
C_EXPORT void QCoreApplication_Destroy(QCoreApplicationH handle);
C_EXPORT void QCoreApplication_arguments(QStringListH retval);
C_EXPORT void QCoreApplication_setAttribute(Qt::ApplicationAttribute attribute, bool on);
C_EXPORT bool QCoreApplication_testAttribute(Qt::ApplicationAttribute attribute);
C_EXPORT void QCoreApplication_setOrganizationDomain(PWideString orgDomain);
C_EXPORT void QCoreApplication_organizationDomain(PWideString retval);
C_EXPORT void QCoreApplication_setOrganizationName(PWideString orgName);
C_EXPORT void QCoreApplication_organizationName(PWideString retval);
C_EXPORT void QCoreApplication_setApplicationName(PWideString application);
C_EXPORT void QCoreApplication_applicationName(PWideString retval);
C_EXPORT void QCoreApplication_setApplicationVersion(PWideString version);
C_EXPORT void QCoreApplication_applicationVersion(PWideString retval);
C_EXPORT QCoreApplicationH QCoreApplication_instance();
C_EXPORT int QCoreApplication_exec();
C_EXPORT void QCoreApplication_processEvents(unsigned int flags);
C_EXPORT void QCoreApplication_processEvents2(unsigned int flags, int maxtime);
C_EXPORT void QCoreApplication_exit(int retcode);
C_EXPORT bool QCoreApplication_sendEvent(QObjectH receiver, QEventH event);
C_EXPORT void QCoreApplication_postEvent(QObjectH receiver, QEventH event, int priority);
C_EXPORT void QCoreApplication_sendPostedEvents(QObjectH receiver, int event_type);
C_EXPORT void QCoreApplication_removePostedEvents(QObjectH receiver, int eventType);
C_EXPORT QAbstractEventDispatcherH QCoreApplication_eventDispatcher();
C_EXPORT void QCoreApplication_setEventDispatcher(QAbstractEventDispatcherH eventDispatcher);
C_EXPORT bool QCoreApplication_notify(QCoreApplicationH handle, QObjectH AnonParam1, QEventH AnonParam2);
C_EXPORT bool QCoreApplication_startingUp();
C_EXPORT bool QCoreApplication_closingDown();
C_EXPORT void QCoreApplication_applicationDirPath(PWideString retval);
C_EXPORT void QCoreApplication_applicationFilePath(PWideString retval);
C_EXPORT qint64 QCoreApplication_applicationPid();
C_EXPORT void QCoreApplication_setLibraryPaths(const QStringListH AnonParam1);
C_EXPORT void QCoreApplication_libraryPaths(QStringListH retval);
C_EXPORT void QCoreApplication_addLibraryPath(PWideString AnonParam1);
C_EXPORT void QCoreApplication_removeLibraryPath(PWideString AnonParam1);
C_EXPORT bool QCoreApplication_installTranslator(QTranslatorH messageFile);
C_EXPORT bool QCoreApplication_removeTranslator(QTranslatorH messageFile);
C_EXPORT void QCoreApplication_translate(PWideString retval, const char* context, const char* key, const char* disambiguation, int n);
C_EXPORT void QCoreApplication_installNativeEventFilter(QCoreApplicationH handle, QAbstractNativeEventFilterH filterObj);
C_EXPORT void QCoreApplication_removeNativeEventFilter(QCoreApplicationH handle, QAbstractNativeEventFilterH filterObj);
C_EXPORT bool QCoreApplication_isQuitLockEnabled();
C_EXPORT void QCoreApplication_setQuitLockEnabled(bool enabled);
C_EXPORT void QCoreApplication_quit();

#endif
