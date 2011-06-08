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
def main():
  usage = "usage: %prog [options]"
  parser = optparse.OptionParser(usage)
  parser.add_option("-e", "--eclipse", dest="eclipseCmd",
		  default='/home/frede/devel/eclipse3.6/eclipse',
		  help="Point to eclipse binary")
  options, args = parser.parse_args()
  osStr,wsStr,archStr = "linux","gtk","x86_64"
  currentdir = os.getcwd()
  destination = currentdir + "/target/products/zaluum.product/" + osStr + "/" + wsStr + "/" + archStr + "/zaluum"
  # Merging branch requires latest merged master
  def callDirector(iu,extraOpts):
    c = options.eclipseCmd + """ -application org.eclipse.equinox.p2.director -nosplash -consolelog -repository http://127.0.0.1/3.6UpdatesMirror,http://download.eclipse.org/tools/gef/updates/releases/,http://127.0.0.1/heliosMirror,http://127.0.0.1/repo/,file:"""+currentdir+"""/target/repository/ -installIU """+ iu + """ -destination """+ destination + extraOpts 
    output,_ = call_command(c)
  logging.info('Materializing base product')
  callDirector("zaluum.product", " -profile DefaultProfile -profileProperties org.eclipse.update.install.features=true -roaming -p2.os %s -p2.ws %s -p2.arch %s" % (osStr,wsStr,archStr))
  for feature in features:
    logging.info("Installing " + feature) 
    callDirector(feature,"")
  logging.info('Done') 
def call_command(command):
  process = subprocess.Popen(command.split(' '),
			      stdout=subprocess.PIPE,
			      stderr=subprocess.PIPE)
  return process.communicate()
if __name__ == "__main__":
  main()
