#ifndef QLCLWEBPAGE_H
#define QLCLWEBPAGE_H

#include <QWebPage>
#include "pascalbind.h"
 


class QLCLWebPage  : public QWebPage {

public:

  //==================================================================================== 
  QLCLWebPage(QObject * parent = 0 ) : QWebPage(parent) {
    userAgentForUrlOverride.func = NULL;
  };

  //==================================================================================== 
  void override_userAgentForUrl(const QOverrideHook hook) {
    userAgentForUrlOverride = hook;
  };

  QString DefaultUserAgentForUrl(const QUrl& url) const {
    return QWebPage::userAgentForUrl(url);
  }


protected:

  QString userAgentForUrl(const QUrl& url) const {
    if (userAgentForUrlOverride.func) {
      PWideString t_userAgent;
      initializePWideString(t_userAgent);
      typedef void (*func_type)(void *data, const QUrlH url, PWideString userAgent);
      (*(func_type)userAgentForUrlOverride.func)(userAgentForUrlOverride.data, (const QUrlH)&url, t_userAgent);
      QString userAgent;
      copyPWideStringToQString(t_userAgent, userAgent);
      return(userAgent); 
    } 
    else return QWebPage::userAgentForUrl(url);
  }

private:

  //==================================================================================== 
  QHook userAgentForUrlOverride;


};

#endif
