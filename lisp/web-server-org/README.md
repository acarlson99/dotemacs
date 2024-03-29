# File Server for Emacs Org Mode

run fileserver for emacs org mode files

Routes:
* /path/to/file.org -- edit file.org
* /path/to/file.html -- render file.org as html
* /path/to/file.tex -- render file.org as latex
* /path/to/file.txt -- render file.org as plaintext
* /path/to/file -- edit file and display HTML render alongside
* /path/to/dir/ -- display directory (trailing '/' required)

## Deploy

### Docker

```sh
docker pull bombblob/emacs-org-fileserver
docker run -d --restart=always bombblob/emacs-org-fileserver -p 8080:8080
```

### systemctl

```sh
curl https://raw.githubusercontent.com/acarlson99/dotemacs/master/lisp/web-server-org/setup-systemd.sh | sudo bash
# note that you may need to modify `/opt/web-server-org/env'
# to set hostname, port, dir
sudo systemctl enable web-server-org.service
sudo systemctl start web-server-org.service
```

# TODO

This project has many incomplete features

* [ ] Authentication
  * There should be some authentication to edit files (e.g. OAuth2)
* [ ] systemd setup should create a new user with restricted permissions
  * Currently runs fileserver as root; this seems unwise
* [ ] HTTPS support (currently only supports HTTP)
* [ ] support drop-in CSS and/or org style themes (e.g. `(require 'ox)`)
* [ ] Race conditions/versioning
  * Currently there is no check when uploading a file; it just writes the POST request data to the file with no versioning or backups
  * This can lead to accidentally overwriting important data
  * A fix could be to include revisions numbers or timestamps e.g.
    * A checkout file at revision 4
    * B checkout rev. 4
    * A upload rev. 4->5
    * B upload rev. 4->5 which is out of date, so the file upload should be rejected with a warning
