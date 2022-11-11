#ifndef QLCLABSTRACTSPINBOX_H
#define QLCLABSTRACTSPINBOX_H

#include <QAbstractSpinBox>



class QLCLAbstractSpinBox : public QAbstractSpinBox
{
public:
  static QLineEdit *lineEditHandle(QAbstractSpinBox *protectedhandle) 
    {
    return reinterpret_cast<QLCLAbstractSpinBox *>(protectedhandle)->lineEdit();
    } 
};

#endif
