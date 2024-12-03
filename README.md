### Ninx
This package holds code for applications hosted on our main server. All applications that have not yet grown are held here.
Separation will occur when they mature and need their own resources.

#### Technology.
| Type | Technology |
| Language | Common Lisp, HTML, MD, Python|
| DSLs | CL-WHO, Parenscript, CL-CSS, PY4CL2 |
| PDF generation | Cl-typesetting, CL-PDF |
| CL PM | Quicklisp |
| Database | Postgresql-16, SQLITE |
| OS | Ubuntu 24/ FreeBSD 16 |
| Version manager | ASDF, not lisp's asdf |
| Server | Hunchentoot |

#### Setup variables
Add the following to .bashrc, change the values to correct values
```bash

## set up variables
export NINX_HTTP_PORT=8000
export NINX_HTTPS_PORT=8443
export NINX_SSL_CERT=""
export NINX_SSL_KEY=""
export NINXAI_GEMINI_KEY=""
export AZURE_OPENAI_KEY=""
export POSTGRES_PASSWORD=""
export NINX_DOCUMENT_ROOT="~/common-lisp/ninx/priv/"
export NINX_LOGS_DIR="~/common-lisp/ninx/logs"
export PAYPAL_CLIENT_ID=""
export PAYPAL_CLIENT_SECRET=""

## SET UP SITES TO BE HOSTED
export DECKLM_HOST="decklm"
export NINX_HOST="ninx"
export NINX_BLOG_HOST="blog.ninx"

```
Add the sites to `/etc/hosts/`

### Install ASDF.
[Installation guide on ASDF's homepage](https://asdf-vm.com)

Then install latest SBCL (last tested on Oct, 2024 release: 2.4.9)
```bash
asdf plugin add sbcl
asdf install sbcl latest
asdf global sbcl latest
```

### Setup postgres with the saved password.

### Install SSL certificates.
[Installation guide on official site](https://certbot.eff.org/)

After use the resulting paths to set cert and keys values in .bashrc above

### Install quicklisp
[Installation guide on official site](https://www.quicklisp.org/beta/)
Also setup ultralisp

### Packages
Some packages have to be installed manually: 
Cl-pdf and cl-typesetting from our forks on github. Cl-smtp from gitlab.
Cl-maxminddb from github for ip lookups.; this requires libffi-dev `sudo apt install libffi-dev`
Download the cities geoip mmdb from https://github.com/P3TERX/GeoLite.mmdb and put it `~/common-lisp/ninx/apps/decklm/geoip/` and it must refreshed monthly
Others require ultralisp.


### Install python-pptx global
We require python-pptx to generate the pptx files, install it globally so that it can be used with no env.

## Running the server
Server requires root permissions to run on ports 80 and 443.
```bash
sudo -E $(which sbcl)
```

Then in SBCL's shell
```lisp
(ql:quickload :ninx)
(in-package :server)
;; you can run tests 
(decklm::do-tests)

(start-server)
```

### For SpotPDF.
install default-jre libreoffice-java-common  calibre imagemagick img2pdf poppler-utils pkg-config
=> these below aren't required for now.
texlive-latex-base pandoc texlive-latex-recommended

ebook convert requires liblxml2, which is installed when installing python-pptx as a dep. 

install libxml2-dev via apt and then html5_parser via sudo pip3
pandoc is currently not being used by anything.

### setup venv
sudo apt install pythonx.xx-venv
python3 -m venv decklm

./decklm/bin/pip3 install python-pptx

setup the env with py4cl2:initialize

### Using sudo is a bad idea because very many things break when we do.
Let's instead redirect 80 to 8000 and 443 to 8443 via iptables. this is easier to handle as the application will be deployed once, the same is dev as in prod.


to do so, allow traffic on ports 8000 and 443
sudo ufw allow 8000/tcp
sudo ufw allow 8443/tcp
sudo ufw allow 80/tcp
sudo ufw allow 443/tcp

add the following to /etc/ufw/before.rules
```

*nat
:PREROUTING ACCEPT [0:0]

# Redirect external HTTP traffic to port 8000
-A PREROUTING -p tcp --dport 80 -j REDIRECT --to-port 8000

# Redirect external HTTPS traffic to port 8443
-A PREROUTING -p tcp --dport 443 -j REDIRECT --to-port 8443

COMMIT

```
run `sudo systemctl restart ufw`
Local redirects cause alot of problems. hunchentoot keeps getting a yet to be identified signal. that swamps the resources.
REMEMBER TO OPEN UP PORTS 80/8000 AND 443/8443
### Setting up ssl
when you renew;
do the following;
copy the files to ssl
then adjust ownership and permisions
sudo chown -R lam ssl/
sudo chmod -R 750 ssl
certbot renew --deploy-hook "cp -r /etc/letsencrypt/live/spotpdf.com /home/lam/ssl && chown -R lam /home/lam/ssl && chmod -R 700 /home/lam/ssl"

