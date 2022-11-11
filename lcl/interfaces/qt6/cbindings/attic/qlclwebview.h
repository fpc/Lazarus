#ifndef QLCLWEBVIEW_H
#define QLCLWEBVIEW_H

#include <QWebView>
#include "pascalbind.h"


class QLCLWebView  : public QWebView {

public:

  //==================================================================================== 
  QLCLWebView(QWidget * parent = 0 ) : QWebView(parent) {
    createWindowOverride.func = NULL;
  };

  //==================================================================================== 
  void override_createWindow(const QOverrideHook hook) {
    createWindowOverride = hook;
  };

protected:

  QWebView *createWindow ( QWebPage::WebWindowType type ) {
  if (createWindowOverride.func) {
    typedef QWebView * (*func_type)(void *data, QWebPage::WebWindowType type);
    return (*(func_type)createWindowOverride.func)(createWindowOverride.data, type);
    } 
  else return QWebView::createWindow(type);
  }; 


private:

  //==================================================================================== 
  QHook createWindowOverride;


};

#endif
