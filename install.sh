#!/bin/bash

get_os () {
  if [[ "$OSTYPE" == "linux-gnu" ]]; then
    echo "linux"
  elif [[ "$OSTYPE" == "freebsd"* ]]; then
    echo "linux"
  elif [[ "$OSTYPE" == "darwin"* ]]; then
    echo "macos"
  elif [[ "$OSTYPE" == "cygwin" ]]; then
    echo "windows"
  elif [[ "$OSTYPE" == "msys" ]]; then
    echo "windows"
  elif [[ "$OSTYPE" == "win32" ]]; then
    echo "windows"
  else
    return -1
  fi
}

OS=`get_os`

cd /tmp
echo "Downloading latest ${OS} release..."
curl -sLo unfog.tar.gz "https://github.com/soywod/unfog/releases/latest/download/unfog-${OS}.tar.gz"
echo "Installing binaries..."
tar -xzf unfog.tar.gz
rm unfog.tar.gz
chmod u+x unfog*
sudo mv unfog* /usr/local/bin/

if [ -d /etc/bash_completion.d/ ]; then
  echo "Installing bash completion..."
  unfog --bash-completion-script `which unfog` > unfog.bash
  sudo mv unfog.bash /etc/bash_completion.d/
  source /etc/bash_completion.d/unfog.bash
fi

echo "Unfog v$(unfog version) installed!"
