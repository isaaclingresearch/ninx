# NINX SETUP
## Installation.

## SQLITE
This should already be installed by default on the system.

## Sbcl
On ubuntu:

Install deps 
```sh
sudo apt install libzstd-dev zlib1g-dev libev-dev screen 
```
Install asdf vm. [Follow instructions here](https://asdf-vm.com/guide/getting-started.html#_3-install-asdf)
Add the following to both .profile and .bashrc
```sh
. "$HOME/.asdf/asdf.sh"
. "$HOME/.asdf/completions/asdf.bash"
```

Add the sbcl plugin.
```sh
asdf plugin add sbcl
```
Install the latest sbcl.
```sh
asdf install sbcl latest
```
Enable it globally.
```sh
asdf global sbcl latest
```

On Freebsd
```sh
sudo pkg install sbcl
```

## Quicklisp
Setup quicklisp. [Follow the instructions here](https://www.quicklisp.org/beta/)

## GH
On ubuntu
```sh
sudo apt install gh
```

On Freebsd
```sh
sudo pkg install gh
```

Login with your token
## Clone the repos and deps
```sh
gh repo clone ninxtech/ninx-cl ~/common-lisp/ninx-cl/
```

## Setup environmental vars in .profile and .bashrc. Change to your values
```sh
export NINX_DOCUMENT_ROOT="~/common-lisp/ninx-cl/www/"
export NINX_SSL_KEY=""
export NINX_SSL_CERT=""
export NINX_HTTP_PORT=""
export NINX_HTTPS_PORT=""
export NINX_HTTPS_URI=""
PATH="~/.asdf/installs/sbcl/$version/bin:$PATH"
```

Add them to the sudoers file with `sudo visudo`

```sh
Defaults env_keep += NINX_DOCUMENT_ROOT
Defaults env_keep += NINX_SSL_KEY
Defaults env_keep += NINX_SSL_CERT
Defaults env_keep += NINX_HTTPS_PORT
Defaults env_keep += NINX_HTTP_PORT
Defaults env_keep += NINX_HTTPS_URI
```

## Start
Start a screen session
```sh
screen -S ninx
```

if you're running on privileged ports (80 and 443)
```sh
sudo -E env PATH=$PATH sbcl
```

else
```sh
sbcl
```

```sh
; run this only the first time
* (ql:register-local-projects) 
NIL
* (ql:quickload :ninx)
:NINX
* (ninx:start-server)
```

Detach the screen session with Ctrl+A Ctrl+D

