# DECKLM.
DeckLM is a web application that generates PDFs of presentations from PDFs and images + a prompt.

#### Technology.
| Type | Technology |
| Language | Common Lisp, HTML, MD, Python|
| DSLs | CL-WHO, Parenscript, CL-CSS, PY4CL2 |
| PDF generation | Cl-typesetting, CL-PDF |
| CL PM | Quicklisp |
| Database | Postgresql |
| OS | Ubuntu 24/ FreeBSD 16 |
| Version manager | ASDF, not lisp's asdf |
| Server | Hunchentoot |

#### Setup variables
Add the following to .bashrc, change the values to correct values
```bash
export DECKLM_HTTP_PORT=8000
export DECKLM_HTTPS_PORT=8443
export DECKLM_SSL_CERT="~/ssl/cert.pem"
export DECKLM_SSL_KEY="~/ssl/key.pem"
export NINXAI_GEMINI_KEY="GEMINI KEY"
export POSTGRES_PASSWORD="PASSWORD"
export DECKLM_DOCUMENT_ROOT="~/common-lisp/decklm/priv/"
export DECKLM_LOGS_DIR="~/common-lisp/decklm/logs"
export DECKLM_HOST="localhost:8443"
export PAYPAL_CLIENT_ID=""
export PAYPAL_CLIENT_SECRET=""
export PYTHONPATH="$PYTHONPATH:~/common-lisp/decklm/src/py/"
export AZURE_OPENAI_KEY=""
```

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

### Packages
Some packages have to be installed manually: 
Cl-pdf and cl-typesetting from our forks on github. Cl-smtp from gitlab.
Cl-maxminddb from github for ip lookups.; this requires libffi-dev `sudo apt install libffi-dev`
Download the cities geoip mmdb from https://github.com/P3TERX/GeoLite.mmdb and put it `~/common-lisp/decklm/geoip/` and it must refreshed monthly
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
(ql:quickload :decklm)
(in-package :decklm)
;; you can run tests 
(do-tests)

(decklm:start-server)
```
