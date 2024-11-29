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

### For GoodPDF.
install default-jre libreoffice-java texlive-latex-base pandoc texlive-latex-recommended calibre
