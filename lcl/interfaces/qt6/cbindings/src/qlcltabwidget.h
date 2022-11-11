#ifndef QLCLTABWIDGET_H
#define QLCLTABWIDGET_H

#include <QTabWidget>


class QLCLTabWidget : public QTabWidget
{
public:
  static QTabBar *tabBarHandle(QTabWidget *protectedhandle) 
    {
    return reinterpret_cast<QLCLTabWidget *>(protectedhandle)->tabBar();
    } 
};

#endif
