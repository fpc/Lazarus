#------------------------------------------------------------------------------
# Class: ContactManager
# Author: Clifford E. Baeseman CodeWizard
#------------------------------------------------------------------------------
import os,sys,posix


#change these to match your configuration
COMPILER_LOC = '/opt/fpc/ppc386'
LAZARUS_DIR = '/opt/fpc/lazarus'
LCL_DIR = '/opt/fpc/lazarus/lcl'

LAZARUS_FILE = '/opt/fpc/lazarus/lazarus.pp'

#these are passed to the linker
LIBRARY_DIRS = ['/lib','/usr/lib']
#these are directories for .inc files
INCLUDE_DIRS = ['/opt/fpc/lazarus/lcl/include','/opt/fpc/lazarus/lcl/interfaces/gtk']
#these are for unit search path
UNIT_DIRS = ['/opt/fpc/rtl/linux','/opt/fpc/units/linux','/opt/fpc/lazarus/lcl/','/opt/fpc/fcl/linux/','/opt/fpc/lazarus/lcl/interfaces/gtk','/opt/fpc/packages/gtk','/opt/fpc/lazarus/components/mwedit92/']



#**Main Class Object**
class cls_main:

    #constructor
    def __init__(self):
      #file list buffer
      self.filelist = []

    def GetPPFiles(self,dirname):
      print dirname
      os.chdir(dirname)
      print 'here'
      flist = os.listdir(dirname)
      print flist
      for line in flist:
        try:
          fpart = string.split(line,'.')
          if fpart[1] == 'pp':
            self.filelist.append(fpart[0]+'.'+'pp')
        except:
          pass

    def GetSwitches(self):
      sbuffer = ""
      for line in LIBRARY_DIRS:
        sbuffer = sbuffer + ' -Fl' + line
      for line in UNIT_DIRS:
        sbuffer = sbuffer + ' -Fu' + line
      for line in INCLUDE_DIRS:
        sbuffer = sbuffer + ' -Fi' + line

      return sbuffer

    def LazarusBuild(self):
      print 'STARTING LAZARUS BUILD->>'
      os.chdir(LAZARUS_DIR)
      execstr = COMPILER_LOC + ' -vwel ' + self.GetSwitches() + ' ' + LAZARUS_FILE
      print 'COMMAND->>' + execstr
      cpout = os.popen(execstr)
      lines = cpout.readlines()
      for line in lines:
        print line

      print 'FINISHED LAZARUS BUILD->>'


#**Main Function**
def main():
    cmain = cls_main()
    cmain.LazarusBuild()




if __name__ == '__main__':
    main()