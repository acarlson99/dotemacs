# run this to populate files for web-server-org.service
mkdir -p /etc/web-server-org/     # file host dir
mkdir -p /opt/web-server-org/     # location for elisp files
rm       /opt/web-server-org/*.el # remove old install if exists

FILES=`cat<<EOF
https://raw.githubusercontent.com/acarlson99/dotemacs/master/lisp/web-server-org/web-server-org.el
https://raw.githubusercontent.com/acarlson99/dotemacs/master/lisp/web-server-org/setup.el
https://raw.githubusercontent.com/acarlson99/dotemacs/master/lisp/el-log.el
https://raw.githubusercontent.com/acarlson99/dotemacs/master/lisp/argparse.el
https://raw.githubusercontent.com/acarlson99/dotemacs/master/lisp/OwOify.el
EOF`

cd /opt/web-server-org/ && \
wget $FILES

emacs --script /opt/web-server-org/setup.el
