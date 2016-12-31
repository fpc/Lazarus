//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QABSTRACTPRINTDIALOG_HOOK_H
#define QABSTRACTPRINTDIALOG_HOOK_H

#include <qabstractprintdialog.h>

#include "qdialog_hook.h"

class QAbstractPrintDialog_hook : public QDialog_hook {
  Q_OBJECT
  public:
    QAbstractPrintDialog_hook(QObject *handle) : QDialog_hook(handle) {
    }
};


#endif
