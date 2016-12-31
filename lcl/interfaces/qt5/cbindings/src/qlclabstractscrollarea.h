#ifndef QLCLABSTRACTSCROLLAREA_H
#define QLCLABSTRACTSCROLLAREA_H

#include <QAbstractScrollArea>
#include "pascalbind.h"

class QLCLAbstractScrollArea : public QAbstractScrollArea {

public:

  //==================================================================================== 
  QLCLAbstractScrollArea(QWidget * parent = 0) : QAbstractScrollArea(parent) {
    viewportEventOverride.func = NULL;
  };

  //==================================================================================== 
  void override_viewportEvent(const QOverrideHook hook) {
    viewportEventOverride = hook; 
  }

  //==================================================================================== 
  bool InheritedViewportEvent(QEvent * event ) {
    return QAbstractScrollArea::viewportEvent(event);
  };


private:

  //==================================================================================== 
  QOverrideHook viewportEventOverride;

  //==================================================================================== 
  bool viewportEvent(QEvent * event ) {
    
    bool result = false;
   
    if (viewportEventOverride.func) {

      typedef void (*func_type)(void *data,const QEventH event, bool * result);
      (*(func_type)viewportEventOverride.func)(viewportEventOverride.data, (const QEventH)event,(bool *) &result);
      }
   else result = QAbstractScrollArea::viewportEvent(event);

   return result;

  };
};

#endif
