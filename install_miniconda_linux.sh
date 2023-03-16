#!/bin/bash

curl https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh --output ~/miniconda_install.sh
chmod +x ~/miniconda_install.sh
~/miniconda_install.sh -b
rm ~/miniconda_install.sh