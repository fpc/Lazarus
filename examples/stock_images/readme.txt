This example allows to traverse stock images which can be requeted from LCL.

There are 2 kind of such images:

1. Dialog icons. They are used mostly by MessageDlg. If underlying widgetset
   has own stock icons LCL will use them in other case LCL provides own default
   icons. LCL does not have defaults for all images. Only those images which are
   used by MessageDlg have defaults in LCL.

2. Button icons. They are mostly used by TBitBtn control. For every Kind except bkCustom
   LCL has default stock image which is used in the case when widgetset does not has own.
   There are more stock images than kinds of TBitBtn. For example you can request stock
   Save or Open button icon and in case widgetset = gtk or qt you will get it.

Next thing shown by this example is how to get caption for stock image.