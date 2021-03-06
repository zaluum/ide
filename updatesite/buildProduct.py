#!/usr/bin/env python
import os
import re
import subprocess
import logging
import optparse

logging.basicConfig(level=logging.INFO, format='%(asctime)s %(levelname)s %(message)s')
features = [
  "org.eclipse.jdt.feature.group",
  "org.zaluum.nide.feature.feature.group",
  "org.zaluum.jdt.patch.feature.group",
  "org.eclipse.draw2d.feature.group"]
oses = [
  ["macosx","cocoa","x86_64"],
  ["macosx","cocoa","x86"],
  ["linux","gtk","x86_64"],
  ["linux","gtk","x86"],
  ["win32","win32","x86"],
  ["win32","win32","x86_64"]]
def main():
  usage = "usage: %prog [options]"
  parser = optparse.OptionParser(usage)
  parser.add_option("-e", "--eclipse", dest="eclipseCmd",
		  default='/home/frede/devel/eclipse-3.7.1/eclipse',
		  help="Point to eclipse binary")
  parser.add_option("-o", "--os", dest="os")
  parser.add_option("-w", "--ws", dest="ws")
  parser.add_option("-a", "--arch", dest="arch")  
  options, args = parser.parse_args()

  def exportProduct(osStr,wsStr,archStr):
    currentdir = os.getcwd()
    destination = currentdir + "/target/products/final/" + osStr + "/" + wsStr + "/" + archStr + "/zaluum/"
    print "destination:" + destination
    def callDirector(iu,extraOpts):
      c = options.eclipseCmd + """ -application org.eclipse.equinox.p2.director -nosplash -repository http://127.0.0.1/3.7UpdatesMirror,http://127.0.0.1/indigoMirror,http://127.0.0.1/repo/,file:"""+currentdir+"""/target/repository/ -installIU """+ iu + """ -destination """+ destination
      print c
      output,e = call_command(c)
      print output
      print e
    logging.info('Materializing base product for ' + osStr + " " + wsStr + " " + archStr)
    callDirector("zaluum.product", " -profile DefaultProfile -profileProperties org.eclipse.update.install.features=true -roaming -p2.os %s -p2.ws %s -p2.arch %s" % (osStr,wsStr,archStr))
    for feature in features:
      logging.info("Installing " + feature) 
      callDirector(feature,"")
    logging.info('Done ' + osStr + " " + wsStr + " " + archStr) 

  if (options.os and options.ws and options.arch):
    print "Exporting for "
    exportProduct(options.os,options.ws,options.arch)
  else:
    print "Exporting all oses"
    for o in oses:
      exportProduct(o[0],o[1],o[2])

def call_command(command):
  process = subprocess.Popen(command.split(' '),
			      stdout=subprocess.PIPE,
			      stderr=subprocess.PIPE)
  return process.communicate()
if __name__ == "__main__":
  main()
