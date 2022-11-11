//******************************************************************************
//  Copyright (c) 2005-2013 by Jan Van hijfte
//
//  See the included file COPYING.TXT for details about the copyright.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//******************************************************************************


#ifndef QLISTVIEW_HOOK_H
#define QLISTVIEW_HOOK_H

#include <qlistview.h>

#include "qabstractitemview_hook.h"

class QListView_hook : public QAbstractItemView_hook {
  Q_OBJECT
  public:
    QListView_hook(QObject *handle) : QAbstractItemView_hook(handle) {
    }
};


#endif
