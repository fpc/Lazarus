#******************************************************************************
#  Copyright (c) 2005-2013 by Jan Van hijfte
#  
#  See the included file COPYING.TXT for details about the copyright.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
#******************************************************************************



# Binding Release Version 2.6 against Qt5 5.6 LTS release.
# WebKit widgets are disabled until webenginewidgets are implemented.

VERSION = 1.2.8

QT += gui network printsupport
TARGET = Qt5Pas
TEMPLATE = lib
VPATH = src
MOC_DIR = tmp
OBJECTS_DIR = tmp
QMAKE_CXXFLAGS += -Wfatal-errors

# Match Intel x86_64 i686 i586 i386 x86 ...
is86 = $$find(QMAKE_HOST.arch, ".*86")
# Match 64 bit
is64 = $$find(QMAKE_HOST.arch, ".*64")

!mac:!isEmpty(is86):isEmpty(is64) {
  message("Added stack alignment options to CXXFLAGS in support of SSE on arch:" $$QMAKE_HOST.arch)
  QMAKE_CXXFLAGS += -mstackrealign -mincoming-stack-boundary=2 
}  

haiku:isEmpty(is64) {
  message("Added stack alignment options to CXXFLAGS in support of SSE i386 on Haiku")
  QMAKE_CXXFLAGS += -mstackrealign -mincoming-stack-boundary=2
}

!equals(QT_MAJOR_VERSION,5)|!equals(QT_MINOR_VERSION,6) {
  message("Note: This binding version was generated for Qt 5.6.1. Current Qt is" $$QT_VERSION)
  message("Qt documents binary compatibility in each Version Change Note: http://qt.nokia.com/developer/changes")
}

# Available Qt5 Pascal Binding Platforms: see http://wiki.freepascal.org/Qt5_binding#Supported_Platforms  

target.path = $$[QT_INSTALL_LIBS]
win32 : {
  PLATFORM = MSWINDOWS
  target.path = $$[QT_INSTALL_BINS]
  }
                           
embedded:PLATFORM = QTOPIA  

unix:!embedded:!mac:!haiku:PLATFORM = BINUX

mac {
  PLATFORM = DARWIN
  CONFIG += lib_bundle
  }

contains(PLATFORM, BINUX): {
  message("Adding x11extras for XOrg platform.")
  QT += x11extras
}

contains(CONFIG, maemo5): {
  message("Detected Maemo5")
  PLATFORM = MAEMO5
  QT += maemo5
  }
  
CONFIG -= debug_and_release
CONFIG -= debug_and_release_target
CONFIG -= debug
CONFIG -= warn_on
CONFIG -= create_prl
CONFIG -= link_prl

CONFIG += release
CONFIG += dll
CONFIG += warn_off
  
message("Pascal Qt Interface for binding platform:" $$PLATFORM)
DEFINES += $$PLATFORM   
message("Install location:" $$target.path)

INSTALLS += target


HEADERS +=  \
           chandles.h \
           qobject_hook.h \
           qobject_hook_c.h \
           pascalbind.h \
           flatfuncs.h \
           qobject_c.h \
           qobjectdefs_c.h \
           qvariant_c.h \
           qmetaobject_c.h \
           qcoreevent_c.h \
           qeventloop_c.h \
           qcoreapplication_c.h \
           qcoreapplication_hook.h \
           qtranslator_c.h \
           qtimer_c.h \
           qtimer_hook.h \
           qabstracteventdispatcher_c.h \
           qmimedata_c.h \
           qsocketnotifier_c.h \
           qsocketnotifier_hook.h \
           qabstractitemmodel_c.h \
           qabstractitemmodel_hook.h \
           qitemselectionmodel_c.h \
           qitemselectionmodel_hook.h \
           qthread_c.h \
           qthread_hook.h \
           qlclthread_c.h \
           qchar_c.h \
           qsize_c.h \
           qstring_c.h \
           qstringlist_c.h \
           qsurface_c.h \           
           qsurfaceformat_c.h \
           qrect_c.h \
           qmargins_c.h \                      
           qdatetime_c.h \
           qbytearray_c.h \
           qlocale_c.h \
           qiodevice_c.h \
           qiodevice_hook.h \
           qprocess_c.h \
           qprocess_hook.h \
           qbuffer_c.h \           
           qfiledevice_c.h \
           qfile_c.h \
           qfileinfo_c.h \
           qdir_c.h \
           qurl_c.h \
           qfilesystemwatcher_c.h \
           qfilesystemwatcher_hook.h \
           qpalette_c.h \
           qkeysequence_c.h \
           qevent_c.h \
           qlclmessageevent_c.h \
           qcursor_c.h \
           qclipboard_c.h \
           qclipboard_hook.h \
           qdrag_c.h \
           qdrag_hook.h \
           qsessionmanager_c.h \
           qguiapplication_c.h \
           qguiapplication_hook.h \
           qapplication_c.h \
           qapplication_hook.h \
           qwhatsthis_c.h \
           qsizepolicy_c.h \
           qscreen_c.h \
           qscreen_hook.h \
           qwindow_c.h \
           qwindow_hook.h \           
           qwidget_c.h \
           qwidget_hook.h \
           qlclopenglwidget_c.h \ 
           qlayoutitem_c.h \
           qlayout_c.h \
           qlayout_hook.h \
           qboxlayout_c.h \
           qstackedlayout_c.h \
           qstackedlayout_hook.h \
           qaction_c.h \
           qaction_hook.h \
           qactiongroup_c.h \
           qactiongroup_hook.h \
           qgridlayout_c.h \
           qdesktopwidget_c.h \
           qdesktopwidget_hook.h \
           qtooltip_c.h \
           qx11info_x11_c.h \
           qshortcut_c.h \
           qshortcut_hook.h \
           qgesture_c.h \
           qgesturerecognizer_c.h \
           qcolor_c.h \
           qmatrix_c.h \
           qbrush_c.h \
           qpen_c.h \
           qpolygon_c.h \
           qbackingstore_c.h \
           qpainter_c.h \
           qpaintengine_c.h \
           qpaintdevice_c.h \
           qregion_c.h \
           qpainterpath_c.h \
           qtransform_c.h \
           qpagedpaintdevice_c.h \
           qprinter_c.h \
           qprinterinfo_c.h \
           qfont_c.h \
           qfontdatabase_c.h \
           qtextformat_c.h \
           qtextcursor_c.h \
           qtextoption_c.h \
           qfontmetrics_c.h \
           qfontinfo_c.h \
           qtextdocument_c.h \
           qabstracttextdocumentlayout_c.h \
           qabstracttextdocumentlayout_hook.h \
           qtextlayout_c.h \
           qtextobject_c.h \
           qtextdocumentwriter_c.h \
           qicon_c.h \
           qpixmap_c.h \
           qimage_c.h \
           qbitmap_c.h \
           qpicture_c.h \
           qimageiohandler_c.h \
           qimagereader_c.h \
           qimagewriter_c.h \
           qvalidator_c.h \
           qstandarditemmodel_c.h \
           qstandarditemmodel_hook.h \
           qframe_c.h \
           qframe_hook.h \
           qstackedwidget_c.h \
           qstackedwidget_hook.h \
           qabstractscrollarea_c.h \
           qabstractscrollarea_hook.h \
           qlclabstractscrollarea_c.h \
           qabstractslider_c.h \
           qabstractslider_hook.h \
           qscrollbar_c.h \
           qscrollbar_hook.h \
           qmenu_c.h \
           qmenu_hook.h \
           qmenubar_c.h \
           qmenubar_hook.h \
           qbuttongroup_c.h \
           qbuttongroup_hook.h \
           qabstractbutton_c.h \
           qabstractbutton_hook.h \
           qpushbutton_c.h \
           qpushbutton_hook.h \
           qradiobutton_c.h \
           qlineedit_c.h \
           qlineedit_hook.h \
           qplaintextedit_c.h \
           qplaintextedit_hook.h \
           qtextedit_c.h \
           qtextedit_hook.h \
           qtabwidget_c.h \
           qtabwidget_hook.h \
           qlcltabwidget_c.h \
           qmainwindow_c.h \
           qmainwindow_hook.h \
           qtoolbar_c.h \
           qtoolbar_hook.h \
           qsizegrip_c.h \
           qlcdnumber_c.h \
           qlcdnumber_hook.h \
           qabstractspinbox_c.h \
           qabstractspinbox_hook.h \
           qlclabstractspinbox_c.h \
           qspinbox_c.h \
           qspinbox_hook.h \
           qsplitter_c.h \
           qsplitter_hook.h \
           qcombobox_c.h \
           qcombobox_hook.h \
           qcheckbox_c.h \
           qcheckbox_hook.h \
           qslider_c.h \
           qslider_hook.h \
           qtextbrowser_c.h \
           qtextbrowser_hook.h \
           qlabel_c.h \
           qlabel_hook.h \
           qgroupbox_c.h \
           qgroupbox_hook.h \
           qdockwidget_c.h \
           qdockwidget_hook.h \
           qtabbar_c.h \
           qtabbar_hook.h \
           qprogressbar_c.h \
           qprogressbar_hook.h \
           qstatusbar_c.h \
           qstatusbar_hook.h \
           qtoolbox_c.h \
           qtoolbox_hook.h \
           qtoolbutton_c.h \
           qtoolbutton_hook.h \
           qscrollarea_c.h \
           qmdiarea_c.h \
           qmdiarea_hook.h \
           qmdisubwindow_c.h \
           qmdisubwindow_hook.h \
           qcalendarwidget_c.h \
           qcalendarwidget_hook.h \
           qrubberband_c.h \
           qfontcombobox_c.h \
           qfontcombobox_hook.h \
           qabstractitemview_c.h \
           qabstractitemview_hook.h \
           qlistview_c.h \
           qlistview_hook.h \
           qlistwidget_c.h \
           qlistwidget_hook.h \
           qtreeview_c.h \
           qtreeview_hook.h \
           qtreewidget_c.h \
           qtreewidget_hook.h \
           qheaderview_c.h \
           qheaderview_hook.h \
           qabstractitemdelegate_c.h \
           qabstractitemdelegate_hook.h \
           qitemdelegate_c.h \
           qlclitemdelegate_c.h \
           qtableview_c.h \
           qtableview_hook.h \
           qtablewidget_c.h \
           qtablewidget_hook.h \
           qitemeditorfactory_c.h \
           qstyleditemdelegate_c.h \
           qdialog_c.h \
           qdialog_hook.h \
           qfontdialog_c.h \
           qmessagebox_c.h \
           qinputdialog_c.h \
           qcolordialog_c.h \
           qfiledialog_c.h \
           qfiledialog_hook.h \
           qprogressdialog_c.h \
           qprogressdialog_hook.h \
           qabstractprintdialog_c.h \
           qabstractprintdialog_hook.h \
           qprintdialog_c.h \
           qprintdialog_hook.h \
           qpagesetupdialog_c.h \
           qprintpreviewdialog_c.h \
           qprintpreviewdialog_hook.h \
           qprintpreviewwidget_c.h \
           qprintpreviewwidget_hook.h \
           qsystemtrayicon_c.h \
           qsystemtrayicon_hook.h \
           qdesktopservices_c.h \
           qstyle_c.h \
           qstyleoption_c.h \
           qstylefactory_c.h \
           qgraphicsscene_c.h \
           qgraphicsscene_hook.h \
           qgraphicsview_c.h \
           qsslcipher_c.h \
           qsslkey_c.h \
           qsslerror_c.h \
           qabstractsocket_c.h \
           qabstractsocket_hook.h \
           qudpsocket_c.h \
           qudpsocket_hook.h \
           qtcpsocket_c.h \
           qtcpsocket_hook.h \
           qtcpserver_c.h \
           qtcpserver_hook.h \
           qsslconfiguration_c.h \
           qsslsocket_c.h \
           qnetworkaccessmanager_c.h \
           qnetworkaccessmanager_hook.h \
           qnetworkrequest_c.h \
           qnetworkreply_c.h \
           qnetworkreply_hook.h \
           qnetworkcookiejar_c.h \
           qnetworkproxy_c.h \
           qauthenticator_c.h \
           qcoreapplication_hook_c.h \
           qtimer_hook_c.h \
           qsocketnotifier_hook_c.h \
           qabstractitemmodel_hook_c.h \
           qitemselectionmodel_hook_c.h \
           qthread_hook_c.h \
           qiodevice_hook_c.h \
           qprocess_hook_c.h \
           qfilesystemwatcher_hook_c.h \
           qclipboard_hook_c.h \
           qdrag_hook_c.h \
           qguiapplication_hook_c.h \
           qapplication_hook_c.h \
           qscreen_hook_c.h \           
           qwindow_hook_c.h \
           qwidget_hook_c.h \
           qlayout_hook_c.h \
           qstackedlayout_hook_c.h \
           qaction_hook_c.h \
           qactiongroup_hook_c.h \
           qdesktopwidget_hook_c.h \
           qshortcut_hook_c.h \
           qabstracttextdocumentlayout_hook_c.h \
           qstandarditemmodel_hook_c.h \
           qframe_hook_c.h \
           qstackedwidget_hook_c.h \
           qabstractscrollarea_hook_c.h \
           qabstractslider_hook_c.h \
           qscrollbar_hook_c.h \
           qmenu_hook_c.h \
           qmenubar_hook_c.h \
           qbuttongroup_hook_c.h \
           qabstractbutton_hook_c.h \
           qpushbutton_hook_c.h \
           qlineedit_hook_c.h \
           qplaintextedit_hook_c.h \
           qtextedit_hook_c.h \
           qtabwidget_hook_c.h \
           qmainwindow_hook_c.h \
           qtoolbar_hook_c.h \
           qlcdnumber_hook_c.h \
           qabstractspinbox_hook_c.h \
           qspinbox_hook_c.h \
           qsplitter_hook_c.h \
           qcombobox_hook_c.h \
           qcheckbox_hook_c.h \
           qslider_hook_c.h \
           qtextbrowser_hook_c.h \
           qlabel_hook_c.h \
           qgroupbox_hook_c.h \
           qdockwidget_hook_c.h \
           qtabbar_hook_c.h \
           qprogressbar_hook_c.h \
           qstatusbar_hook_c.h \
           qtoolbox_hook_c.h \
           qtoolbutton_hook_c.h \
           qmdiarea_hook_c.h \
           qmdisubwindow_hook_c.h \
           qcalendarwidget_hook_c.h \
           qfontcombobox_hook_c.h \
           qabstractitemview_hook_c.h \
           qlistview_hook_c.h \
           qlistwidget_hook_c.h \
           qtreeview_hook_c.h \
           qtreewidget_hook_c.h \
           qheaderview_hook_c.h \
           qabstractitemdelegate_hook_c.h \
           qtableview_hook_c.h \
           qtablewidget_hook_c.h \
           qdialog_hook_c.h \
           qfiledialog_hook_c.h \
           qprogressdialog_hook_c.h \
           qabstractprintdialog_hook_c.h \
           qprintdialog_hook_c.h \
           qprintpreviewdialog_hook_c.h \
           qprintpreviewwidget_hook_c.h \
           qsystemtrayicon_hook_c.h \
           qgraphicsscene_hook_c.h \
           qabstractsocket_hook_c.h \
           qudpsocket_hook_c.h \
           qtcpsocket_hook_c.h \
           qtcpserver_hook_c.h \
           qnetworkaccessmanager_hook_c.h \
           qnetworkreply_hook_c.h 
SOURCES +=  \
           qobject_hook_c.cpp \
           pascalbind.cpp \
           flatfuncs.cpp \
           qobject_c.cpp \
           qobjectdefs_c.cpp \
           qvariant_c.cpp \
           qmetaobject_c.cpp \
           qcoreevent_c.cpp \
           qeventloop_c.cpp \
           qcoreapplication_c.cpp \
           qtranslator_c.cpp \
           qtimer_c.cpp \
           qabstracteventdispatcher_c.cpp \
           qmimedata_c.cpp \
           qsocketnotifier_c.cpp \
           qabstractitemmodel_c.cpp \
           qitemselectionmodel_c.cpp \
           qthread_c.cpp \
           qlclthread_c.cpp \
           qchar_c.cpp \
           qsize_c.cpp \
           qstring_c.cpp \
           qstringlist_c.cpp \
           qsurface_c.cpp \
           qsurfaceformat_c.cpp \
           qrect_c.cpp \
           qmargins_c.cpp \           
           qdatetime_c.cpp \
           qbytearray_c.cpp \
           qlocale_c.cpp \
           qiodevice_c.cpp \
           qprocess_c.cpp \
           qbuffer_c.cpp \           
           qfiledevice_c.cpp \
           qfile_c.cpp \
           qfileinfo_c.cpp \
           qdir_c.cpp \
           qurl_c.cpp \
           qfilesystemwatcher_c.cpp \
           qpalette_c.cpp \
           qkeysequence_c.cpp \
           qevent_c.cpp \
           qlclmessageevent_c.cpp \
           qcursor_c.cpp \
           qclipboard_c.cpp \
           qdrag_c.cpp \
           qsessionmanager_c.cpp \
           qguiapplication_c.cpp \
           qapplication_c.cpp \
           qwhatsthis_c.cpp \
           qsizepolicy_c.cpp \
           qscreen_c.cpp \
           qwindow_c.cpp \           
           qwidget_c.cpp \
           qlclopenglwidget_c.cpp \ 
           qlayoutitem_c.cpp \
           qlayout_c.cpp \
           qboxlayout_c.cpp \
           qstackedlayout_c.cpp \
           qaction_c.cpp \
           qactiongroup_c.cpp \
           qgridlayout_c.cpp \
           qdesktopwidget_c.cpp \
           qtooltip_c.cpp \
           qx11info_x11_c.cpp \
           qshortcut_c.cpp \
           qgesture_c.cpp \
           qgesturerecognizer_c.cpp \
           qcolor_c.cpp \
           qmatrix_c.cpp \
           qbrush_c.cpp \
           qpen_c.cpp \
           qpolygon_c.cpp \
           qbackingstore_c.cpp \
           qpainter_c.cpp \
           qpaintengine_c.cpp \
           qpaintdevice_c.cpp \
           qregion_c.cpp \
           qpainterpath_c.cpp \
           qtransform_c.cpp \
           qpagedpaintdevice_c.cpp \
           qprinter_c.cpp \
           qprinterinfo_c.cpp \
           qfont_c.cpp \
           qfontdatabase_c.cpp \
           qtextformat_c.cpp \
           qtextcursor_c.cpp \
           qtextoption_c.cpp \
           qfontmetrics_c.cpp \
           qfontinfo_c.cpp \
           qtextdocument_c.cpp \
           qabstracttextdocumentlayout_c.cpp \
           qtextlayout_c.cpp \
           qtextobject_c.cpp \
           qtextdocumentwriter_c.cpp \
           qicon_c.cpp \
           qpixmap_c.cpp \
           qimage_c.cpp \
           qbitmap_c.cpp \
           qpicture_c.cpp \
           qimageiohandler_c.cpp \
           qimagereader_c.cpp \
           qimagewriter_c.cpp \
           qvalidator_c.cpp \
           qstandarditemmodel_c.cpp \
           qframe_c.cpp \
           qstackedwidget_c.cpp \
           qabstractscrollarea_c.cpp \
           qlclabstractscrollarea_c.cpp \
           qabstractslider_c.cpp \
           qscrollbar_c.cpp \
           qmenu_c.cpp \
           qmenubar_c.cpp \
           qbuttongroup_c.cpp \
           qabstractbutton_c.cpp \
           qpushbutton_c.cpp \
           qradiobutton_c.cpp \
           qlineedit_c.cpp \
           qplaintextedit_c.cpp \
           qtextedit_c.cpp \
           qtabwidget_c.cpp \
           qlcltabwidget_c.cpp \
           qmainwindow_c.cpp \
           qtoolbar_c.cpp \
           qsizegrip_c.cpp \
           qlcdnumber_c.cpp \
           qabstractspinbox_c.cpp \
           qlclabstractspinbox_c.cpp \
           qspinbox_c.cpp \
           qsplitter_c.cpp \
           qcombobox_c.cpp \
           qcheckbox_c.cpp \
           qslider_c.cpp \
           qtextbrowser_c.cpp \
           qlabel_c.cpp \
           qgroupbox_c.cpp \
           qdockwidget_c.cpp \
           qtabbar_c.cpp \
           qprogressbar_c.cpp \
           qstatusbar_c.cpp \
           qtoolbox_c.cpp \
           qtoolbutton_c.cpp \
           qscrollarea_c.cpp \
           qmdiarea_c.cpp \
           qmdisubwindow_c.cpp \
           qcalendarwidget_c.cpp \
           qrubberband_c.cpp \
           qfontcombobox_c.cpp \
           qabstractitemview_c.cpp \
           qlistview_c.cpp \
           qlistwidget_c.cpp \
           qtreeview_c.cpp \
           qtreewidget_c.cpp \
           qheaderview_c.cpp \
           qabstractitemdelegate_c.cpp \
           qitemdelegate_c.cpp \
           qlclitemdelegate_c.cpp \
           qtableview_c.cpp \
           qtablewidget_c.cpp \
           qitemeditorfactory_c.cpp \
           qstyleditemdelegate_c.cpp \
           qdialog_c.cpp \
           qfontdialog_c.cpp \
           qmessagebox_c.cpp \
           qinputdialog_c.cpp \
           qcolordialog_c.cpp \
           qfiledialog_c.cpp \
           qprogressdialog_c.cpp \
           qabstractprintdialog_c.cpp \
           qprintdialog_c.cpp \
           qpagesetupdialog_c.cpp \
           qprintpreviewdialog_c.cpp \
           qprintpreviewwidget_c.cpp \
           qsystemtrayicon_c.cpp \
           qdesktopservices_c.cpp \
           qstyle_c.cpp \
           qstyleoption_c.cpp \
           qstylefactory_c.cpp \
           qgraphicsscene_c.cpp \
           qgraphicsview_c.cpp \
           qsslcipher_c.cpp \
           qsslkey_c.cpp \
           qsslerror_c.cpp \
           qabstractsocket_c.cpp \
           qudpsocket_c.cpp \
           qtcpsocket_c.cpp \
           qtcpserver_c.cpp \
           qsslconfiguration_c.cpp \
           qsslsocket_c.cpp \
           qnetworkaccessmanager_c.cpp \
           qnetworkrequest_c.cpp \
           qnetworkreply_c.cpp \
           qnetworkcookiejar_c.cpp \
           qnetworkproxy_c.cpp \
           qauthenticator_c.cpp \
           qcoreapplication_hook_c.cpp \
           qtimer_hook_c.cpp \
           qsocketnotifier_hook_c.cpp \
           qabstractitemmodel_hook_c.cpp \
           qitemselectionmodel_hook_c.cpp \
           qthread_hook_c.cpp \
           qiodevice_hook_c.cpp \
           qprocess_hook_c.cpp \
           qfilesystemwatcher_hook_c.cpp \
           qclipboard_hook_c.cpp \
           qdrag_hook_c.cpp \
           qguiapplication_hook_c.cpp \
           qapplication_hook_c.cpp \
           qscreen_hook_c.cpp \           
           qwindow_hook_c.cpp \
           qwidget_hook_c.cpp \
           qlayout_hook_c.cpp \
           qstackedlayout_hook_c.cpp \
           qaction_hook_c.cpp \
           qactiongroup_hook_c.cpp \
           qdesktopwidget_hook_c.cpp \
           qshortcut_hook_c.cpp \
           qabstracttextdocumentlayout_hook_c.cpp \
           qstandarditemmodel_hook_c.cpp \
           qframe_hook_c.cpp \
           qstackedwidget_hook_c.cpp \
           qabstractscrollarea_hook_c.cpp \
           qabstractslider_hook_c.cpp \
           qscrollbar_hook_c.cpp \
           qmenu_hook_c.cpp \
           qmenubar_hook_c.cpp \
           qbuttongroup_hook_c.cpp \
           qabstractbutton_hook_c.cpp \
           qpushbutton_hook_c.cpp \
           qlineedit_hook_c.cpp \
           qplaintextedit_hook_c.cpp \
           qtextedit_hook_c.cpp \
           qtabwidget_hook_c.cpp \
           qmainwindow_hook_c.cpp \
           qtoolbar_hook_c.cpp \
           qlcdnumber_hook_c.cpp \
           qabstractspinbox_hook_c.cpp \
           qspinbox_hook_c.cpp \
           qsplitter_hook_c.cpp \
           qcombobox_hook_c.cpp \
           qcheckbox_hook_c.cpp \
           qslider_hook_c.cpp \
           qtextbrowser_hook_c.cpp \
           qlabel_hook_c.cpp \
           qgroupbox_hook_c.cpp \
           qdockwidget_hook_c.cpp \
           qtabbar_hook_c.cpp \
           qprogressbar_hook_c.cpp \
           qstatusbar_hook_c.cpp \
           qtoolbox_hook_c.cpp \
           qtoolbutton_hook_c.cpp \
           qmdiarea_hook_c.cpp \
           qmdisubwindow_hook_c.cpp \
           qcalendarwidget_hook_c.cpp \
           qfontcombobox_hook_c.cpp \
           qabstractitemview_hook_c.cpp \
           qlistview_hook_c.cpp \
           qlistwidget_hook_c.cpp \
           qtreeview_hook_c.cpp \
           qtreewidget_hook_c.cpp \
           qheaderview_hook_c.cpp \
           qabstractitemdelegate_hook_c.cpp \
           qtableview_hook_c.cpp \
           qtablewidget_hook_c.cpp \
           qdialog_hook_c.cpp \
           qfiledialog_hook_c.cpp \
           qprogressdialog_hook_c.cpp \
           qabstractprintdialog_hook_c.cpp \
           qprintdialog_hook_c.cpp \
           qprintpreviewdialog_hook_c.cpp \
           qprintpreviewwidget_hook_c.cpp \
           qsystemtrayicon_hook_c.cpp \
           qgraphicsscene_hook_c.cpp \
           qabstractsocket_hook_c.cpp \
           qudpsocket_hook_c.cpp \
           qtcpsocket_hook_c.cpp \
           qtcpserver_hook_c.cpp \
           qnetworkaccessmanager_hook_c.cpp \
           qnetworkreply_hook_c.cpp
# end of file
