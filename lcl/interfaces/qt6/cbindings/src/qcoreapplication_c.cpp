//******************************************************************************
//  Copyright (c) 2005-2022 by Jan Van hijfte, Željan Rikalo
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#include "qcoreapplication_c.h"

QCoreApplicationH QCoreApplication_Create(int* argc, char** argv, int AnonParam3)
{
	return (QCoreApplicationH) new QCoreApplication(*(int*)argc, argv, AnonParam3);
}

void QCoreApplication_Destroy(QCoreApplicationH handle)
{
	delete (QCoreApplication *)handle;
}

void QCoreApplication_arguments(QStringListH retval)
{
	*(QStringList *)retval = QCoreApplication::arguments();
}

void QCoreApplication_setAttribute(Qt::ApplicationAttribute attribute, bool on)
{
	QCoreApplication::setAttribute(attribute, on);
}

bool QCoreApplication_testAttribute(Qt::ApplicationAttribute attribute)
{
	return (bool) QCoreApplication::testAttribute(attribute);
}

void QCoreApplication_setOrganizationDomain(PWideString orgDomain)
{
	QString t_orgDomain;
	copyPWideStringToQString(orgDomain, t_orgDomain);
	QCoreApplication::setOrganizationDomain(t_orgDomain);
}

void QCoreApplication_organizationDomain(PWideString retval)
{
	QString t_retval;
	t_retval = QCoreApplication::organizationDomain();
	copyQStringToPWideString(t_retval, retval);
}

void QCoreApplication_setOrganizationName(PWideString orgName)
{
	QString t_orgName;
	copyPWideStringToQString(orgName, t_orgName);
	QCoreApplication::setOrganizationName(t_orgName);
}

void QCoreApplication_organizationName(PWideString retval)
{
	QString t_retval;
	t_retval = QCoreApplication::organizationName();
	copyQStringToPWideString(t_retval, retval);
}

void QCoreApplication_setApplicationName(PWideString application)
{
	QString t_application;
	copyPWideStringToQString(application, t_application);
	QCoreApplication::setApplicationName(t_application);
}

void QCoreApplication_applicationName(PWideString retval)
{
	QString t_retval;
	t_retval = QCoreApplication::applicationName();
	copyQStringToPWideString(t_retval, retval);
}

void QCoreApplication_setApplicationVersion(PWideString version)
{
	QString t_version;
	copyPWideStringToQString(version, t_version);
	QCoreApplication::setApplicationVersion(t_version);
}

void QCoreApplication_applicationVersion(PWideString retval)
{
	QString t_retval;
	t_retval = QCoreApplication::applicationVersion();
	copyQStringToPWideString(t_retval, retval);
}

QCoreApplicationH QCoreApplication_instance()
{
	return (QCoreApplicationH) QCoreApplication::instance();
}

int QCoreApplication_exec()
{
	return (int) QCoreApplication::exec();
}

void QCoreApplication_processEvents(unsigned int flags)
{
	QCoreApplication::processEvents((QEventLoop::ProcessEventsFlags)flags);
}

void QCoreApplication_processEvents2(unsigned int flags, int maxtime)
{
	QCoreApplication::processEvents((QEventLoop::ProcessEventsFlags)flags, maxtime);
}

void QCoreApplication_exit(int retcode)
{
	QCoreApplication::exit(retcode);
}

bool QCoreApplication_sendEvent(QObjectH receiver, QEventH event)
{
	return (bool) QCoreApplication::sendEvent((QObject*)receiver, (QEvent*)event);
}

void QCoreApplication_postEvent(QObjectH receiver, QEventH event, int priority)
{
	QCoreApplication::postEvent((QObject*)receiver, (QEvent*)event, priority);
}

void QCoreApplication_sendPostedEvents(QObjectH receiver, int event_type)
{
	QCoreApplication::sendPostedEvents((QObject*)receiver, event_type);
}

void QCoreApplication_removePostedEvents(QObjectH receiver, int eventType)
{
	QCoreApplication::removePostedEvents((QObject*)receiver, eventType);
}

QAbstractEventDispatcherH QCoreApplication_eventDispatcher()
{
	return (QAbstractEventDispatcherH) QCoreApplication::eventDispatcher();
}

void QCoreApplication_setEventDispatcher(QAbstractEventDispatcherH eventDispatcher)
{
	QCoreApplication::setEventDispatcher((QAbstractEventDispatcher*)eventDispatcher);
}

bool QCoreApplication_notify(QCoreApplicationH handle, QObjectH AnonParam1, QEventH AnonParam2)
{
	return (bool) ((QCoreApplication *)handle)->notify((QObject*)AnonParam1, (QEvent*)AnonParam2);
}

bool QCoreApplication_startingUp()
{
	return (bool) QCoreApplication::startingUp();
}

bool QCoreApplication_closingDown()
{
	return (bool) QCoreApplication::closingDown();
}

void QCoreApplication_applicationDirPath(PWideString retval)
{
	QString t_retval;
	t_retval = QCoreApplication::applicationDirPath();
	copyQStringToPWideString(t_retval, retval);
}

void QCoreApplication_applicationFilePath(PWideString retval)
{
	QString t_retval;
	t_retval = QCoreApplication::applicationFilePath();
	copyQStringToPWideString(t_retval, retval);
}

qint64 QCoreApplication_applicationPid()
{
	return (qint64) QCoreApplication::applicationPid();
}

void QCoreApplication_setLibraryPaths(const QStringListH AnonParam1)
{
	QCoreApplication::setLibraryPaths(*(const QStringList*)AnonParam1);
}

void QCoreApplication_libraryPaths(QStringListH retval)
{
	*(QStringList *)retval = QCoreApplication::libraryPaths();
}

void QCoreApplication_addLibraryPath(PWideString AnonParam1)
{
	QString t_AnonParam1;
	copyPWideStringToQString(AnonParam1, t_AnonParam1);
	QCoreApplication::addLibraryPath(t_AnonParam1);
}

void QCoreApplication_removeLibraryPath(PWideString AnonParam1)
{
	QString t_AnonParam1;
	copyPWideStringToQString(AnonParam1, t_AnonParam1);
	QCoreApplication::removeLibraryPath(t_AnonParam1);
}

bool QCoreApplication_installTranslator(QTranslatorH messageFile)
{
	return (bool) QCoreApplication::installTranslator((QTranslator*)messageFile);
}

bool QCoreApplication_removeTranslator(QTranslatorH messageFile)
{
	return (bool) QCoreApplication::removeTranslator((QTranslator*)messageFile);
}

void QCoreApplication_translate(PWideString retval, const char* context, const char* key, const char* disambiguation, int n)
{
	QString t_retval;
	t_retval = QCoreApplication::translate(context, key, disambiguation, n);
	copyQStringToPWideString(t_retval, retval);
}

void QCoreApplication_installNativeEventFilter(QCoreApplicationH handle, QAbstractNativeEventFilterH filterObj)
{
	((QCoreApplication *)handle)->installNativeEventFilter((QAbstractNativeEventFilter*)filterObj);
}

void QCoreApplication_removeNativeEventFilter(QCoreApplicationH handle, QAbstractNativeEventFilterH filterObj)
{
	((QCoreApplication *)handle)->removeNativeEventFilter((QAbstractNativeEventFilter*)filterObj);
}

bool QCoreApplication_isQuitLockEnabled()
{
	return (bool) QCoreApplication::isQuitLockEnabled();
}

void QCoreApplication_setQuitLockEnabled(bool enabled)
{
	QCoreApplication::setQuitLockEnabled(enabled);
}

void QCoreApplication_quit()
{
	QCoreApplication::quit();
}

