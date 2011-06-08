#!/bin/sh
/home/frede/devel/eclipse3.6/eclipse \
-application org.eclipse.equinox.p2.director \
-repository http://127.0.0.1/3.6UpdatesMirror,http://download.eclipse.org/tools/gef/updates/releases/,http://127.0.0.1/heliosMirror,http://127.0.0.1/repo/,file:/home/frede/devel/zaluum/updatesite/target/repository/ \
-installIU zaluum.product \
-destination /home/frede/devel/zaluum/updatesite/target/products/zaluum.product/linux/gtk/x86_64/zaluum \
-profile DefaultProfile \
-profileProperties org.eclipse.update.install.features=true \
-roaming \
-p2.os linux -p2.ws gtk -p2.arch x86_64