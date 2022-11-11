#ifndef QLCLACCESSIBLEWIDGET_H
#define QLCLACCESSIBLEWIDGET_H

#include <QAccessibleWidget>
#include "pascalbind.h"

class QLCLAccessibleWidget : public QAccessibleWidget {

public:

  //====================================================================================
  QLCLAccessibleWidget(QWidget *o, QAccessible::Role r = QAccessible::Client, const QString& name = QString()) :  QAccessibleWidget (o, r, name) {
    actionNamesOverride.func = NULL;
	childOverride.func = NULL;
    childAtOverride.func = NULL;
    childCountOverride.func = NULL;
    doActionOverride.func = NULL;
    indexOfChildOverride.func = NULL;
    parentOverride.func = NULL;
    rectOverride.func = NULL;
    roleOverride.func = NULL;
    stateOverride.func = NULL;
    textOverride.func = NULL;
  }

  //==================================================================================== 
  void override_actionNames(const QOverrideHook hook) {
    actionNamesOverride = hook;
  }

  void override_child(const QOverrideHook hook) {
    childOverride = hook; 
  }

  //==================================================================================== 
  void override_childAt(const QOverrideHook hook) {
    childAtOverride = hook; 
  }

  void override_childCount(const QOverrideHook hook) {
    childCountOverride = hook; 
  }

  void override_doAction(const QOverrideHook hook) {
    doActionOverride = hook;
  }

  void override_indexOfChild(const QOverrideHook hook) {
    indexOfChildOverride = hook;
  }

  void override_parent(const QOverrideHook hook) {
    parentOverride = hook; 
  }

  void override_rect(const QOverrideHook hook) {
    rectOverride = hook; 
  }

  void override_role(const QOverrideHook hook) {
    roleOverride = hook; 
  }

  void override_state(const QOverrideHook hook) {
    stateOverride = hook; 
  }

  void override_text(const QOverrideHook hook) {
    textOverride = hook; 
  }

private:

  //==================================================================================== 
  QOverrideHook actionNamesOverride;
  QOverrideHook childOverride;
  QOverrideHook childAtOverride;
  QOverrideHook childCountOverride;
  QOverrideHook doActionOverride;
  QOverrideHook indexOfChildOverride;
  QOverrideHook parentOverride;
  QOverrideHook rectOverride;
  QOverrideHook roleOverride;
  QOverrideHook stateOverride;
  QOverrideHook textOverride;

  //====================================================================================
  QStringList actionNames() const {

    QStringList names;

    if (actionNamesOverride.func) {

      typedef void (*func_type)(void *data, PWideString pwstr);
      PWideString t_str;
	  QString str;
      initializePWideString(t_str);
      (*(func_type)actionNamesOverride.func)(actionNamesOverride.data, t_str);
      copyPWideStringToQString(t_str, str);
	  names = str.split(',');

      }
      else {
        names = QAccessibleWidget::actionNames();
      }

    return names;

  };

  //====================================================================================
  QAccessibleInterface *child(int index) const {

    QAccessibleInterface *childInterface = 0;

    if (childOverride.func) {

      typedef void (*func_type)(void *data, int index, QAccessibleInterface *child);
      (*(func_type)childOverride.func)(childOverride.data, index, (QAccessibleInterface *)&childInterface);

      }
      else {
        childInterface = QAccessibleWidget::child(index); 
      }

    return childInterface;

  };

  //==================================================================================== 
  QAccessibleInterface *childAt(int x, int y) const {

    QAccessibleInterface *childInterface = 0;

    if (childAtOverride.func) {

      typedef void (*func_type)(void *data, int x, int y, QAccessibleInterface *child);
      (*(func_type)childAtOverride.func)(childAtOverride.data, x, y, (QAccessibleInterface *)&childInterface);

      }
      else
        childInterface = QAccessibleWidget::childAt(x, y); 

    return childInterface;

  };


  //==================================================================================== 
  int childCount() const {

    int result = 0;

    if (childCountOverride.func) {

      typedef void (*func_type)(void *data, int *result);
      (*(func_type)childCountOverride.func)(childCountOverride.data, (int *)&result);

      }
      else
        result = QAccessibleWidget::childCount(); 

    return result;

  };

  //==================================================================================== 
  int indexOfChild(QAccessibleInterface *child) const {

    int index;

    if (indexOfChildOverride.func) {

      typedef void (*func_type)(void *data, QAccessibleInterface *child, int *index);
      (*(func_type)indexOfChildOverride.func)(indexOfChildOverride.data, child, (int *)&index);

      }
      else {
        index = QAccessibleWidget::indexOfChild(child); 
      }

    return index;

  };

  //====================================================================================
  void doAction(const QString &actionName) {

    if (doActionOverride.func) {
      PWideString t_actionName;
	  initializePWideString(t_actionName);
	  copyQStringToPWideString(actionName, t_actionName);

      typedef void (*func_type)(void *data, PWideString name);
      (*(func_type)doActionOverride.func)(doActionOverride.data, (PWideString) t_actionName);

      }
      else {
        QAccessibleWidget::doAction(actionName);
      }

  };

  //====================================================================================
  QAccessibleInterface *parent() const {

    QAccessibleInterface *parent = 0;

      if (parentOverride.func) {

        typedef void (*func_type)(void *data, QAccessibleInterface *parent);
        (*(func_type)parentOverride.func)(parentOverride.data, (QAccessibleInterface *)&parent);

        }
        else
          parent = QAccessibleWidget::parent(); 

      return parent;

  };

  //==================================================================================== 
  QRect rect() const {

    int left, top, width, height;

    if (rectOverride.func) {

      typedef void (*func_type)(void *data, int *left, int *right, int *width, int *height);
      (*(func_type)rectOverride.func)(rectOverride.data, (int *)&left, (int *)&top, (int *)&width, (int *)&height);
      return QRect(left, top, width, height);
      }
      else
        return QAccessibleWidget::rect();
  };

  //==================================================================================== 
  QAccessible::Role role() const {

    QAccessible::Role result = QAccessible::Client;

    if (roleOverride.func) {

      typedef void (*func_type)(void *data, QAccessible::Role *result);
      (*(func_type)roleOverride.func)(roleOverride.data, (QAccessible::Role *)&result);

      }
      else
        result = QAccessibleWidget::role(); 

    return result;
  };

  //==================================================================================== 
  QAccessible::State state() const {

    QAccessible::State result;

    if (stateOverride.func) {

      typedef void (*func_type)(void *data, QAccessible::State *result);
      (*(func_type)stateOverride.func)(stateOverride.data, (QAccessible::State *)&result);

      }
      else
        result = QAccessibleWidget::state();

    return result;

  };

  //==================================================================================== 
  QString text(QAccessible::Text t) const {

    QString str;

    if (textOverride.func) {

      typedef void (*func_type)(void *data, QAccessible::Text t,  PWideString pwstr);
      PWideString t_str;
      initializePWideString(t_str);
      (*(func_type)textOverride.func)(textOverride.data, t, t_str);
      copyPWideStringToQString(t_str, str);

      }
      else
        str = QAccessibleWidget::text(t); 

    return str;

  };
};

#endif
