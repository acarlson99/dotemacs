# File Server for Emacs Org Mode

run fileserver for emacs org mode files

Routes:
/path/to/file.org -- edit file.org
/path/to/file.html -- render file.org as html
/path/to/file.tex -- render file.org as latex
/path/to/file.txt -- render file.org as plaintext
/path/to/file -- edit file and display HTML render alongside
/path/to/dir/ -- display directory (trailing '/' required)

## Deploy

### Docker

```sh
docker pull bombblob/emacs-org-fileserver
docker run -d --restart=always bombblob/emacs-org-fileserver -p 8080:8080
```

### systemctl

```sh
curl https://raw.githubusercontent.com/acarlson99/dotemacs/master/lisp/web-server-org/web-server-org-setup.sh | sudo bash
sudo wget -O/lib/systemd/system/web-server-org.service \
    https://raw.githubusercontent.com/acarlson99/dotemacs/master/lisp/web-server-org/web-server-org.service
sudo systemctl enable web-server-org.service
sudo systemctl start web-server-org.service
```
