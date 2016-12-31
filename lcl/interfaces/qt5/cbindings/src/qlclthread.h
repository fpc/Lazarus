#ifndef QLCLTHREAD_H
#define QLCLTHREAD_H

#include <QThread>
#include "pascalbind.h"
 

class QLCLThread  : public QThread {

public:

  //==================================================================================== 
  QLCLThread(QObject * parent = 0) : QThread(parent) {
    runOverride.func = NULL;
  };

  //==================================================================================== 
  void override_run(const QOverrideHook hook) {
    runOverride = hook;
  };

  //==================================================================================== 
  int exec() {
    return QThread::exec();
  };


protected:

  void run () {
  if (runOverride.func) {
    typedef void (*func_type)(void *data);
    (*(func_type)runOverride.func)(runOverride.data);
    } 
  else QThread::run();
  }; 


private:

  //==================================================================================== 
  QOverrideHook runOverride;

};

#endif
