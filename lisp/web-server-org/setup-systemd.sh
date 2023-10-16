set -e

# TODO: this should create a new user with restricted permissions
# running fileserver as root seems like a bad idea

# run this to populate files for web-server-org.service
mkdir -p /etc/web-server-org/     # file host dir
mkdir -p /opt/web-server-org/     # location for elisp files
rm       /opt/web-server-org/*.el # remove old install if exists

FILES=`cat<<EOF
https://raw.githubusercontent.com/acarlson99/dotemacs/master/lisp/web-server-org/web-server-org.el
https://raw.githubusercontent.com/acarlson99/dotemacs/master/lisp/web-server-org/setup-pkg.el
https://raw.githubusercontent.com/acarlson99/dotemacs/master/lisp/el-log.el
https://raw.githubusercontent.com/acarlson99/dotemacs/master/lisp/argparse.el
https://raw.githubusercontent.com/acarlson99/dotemacs/master/lisp/OwOify.el
EOF`

cd /opt/web-server-org/ && \
wget $FILES

wget -O/lib/systemd/system/web-server-org.service \
    https://raw.githubusercontent.com/acarlson99/dotemacs/master/lisp/web-server-org/web-server-org.service

emacs --script /opt/web-server-org/setup-pkg.el

cat<<EOF>/opt/web-server-org/env
ORG_SERVER_HOST=localhost
ORG_SERVER_PORT=8002
ORG_SERVER_FILE_DIRECTORY=/etc/web-server-org/
EOF

echo 'installation complete; you may need to edit /opt/web-server-org/env to set hostname'
