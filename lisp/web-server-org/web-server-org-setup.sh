# run this to populate files for web-server-org.service
rm /opt/web-server-org/*.el
mkdir -p /opt/web-server-org/

cd /opt/web-server-org/ && \
wget https://raw.githubusercontent.com/acarlson99/dotemacs/master/lisp/web-server-org/web-server-org.el && \
    wget https://raw.githubusercontent.com/acarlson99/dotemacs/master/lisp/web-server-org/setup.el && \
wget https://raw.githubusercontent.com/acarlson99/dotemacs/master/lisp/el-log.el && \
    wget https://raw.githubusercontent.com/acarlson99/dotemacs/master/lisp/argparse.el && \
    wget https://raw.githubusercontent.com/acarlson99/dotemacs/master/lisp/OwOify.el
