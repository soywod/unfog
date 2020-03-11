#!/bin/sh

get_os () {
  if [ "$OSTYPE" == "linux-gnu" ]; then
    echo "linux"
  elif [ "$OSTYPE" == "darwin"* ]; then
    echo "osx"
  elif [ "$OSTYPE" == "cygwin" ]; then
    echo "windows"
  elif [ "$OSTYPE" == "msys" ]; then
    echo "windows"
  elif [ "$OSTYPE" == "win32" ]; then
    echo "windows"
  elif [ "$OSTYPE" == "freebsd"* ]; then
    echo "linux"
  else
    return -1
  fi
}

cd /tmp
os=`get_os`
echo "Downloading latest ${os} release..."
curl -sLo unfog.tar.gz "https://github.com/unfog-io/unfog-cli/releases/latest/download/unfog-${os}.tar.gz"
echo "Installing binaries..."
tar xzf unfog.tar.gz
rm unfog.tar.gz
chmod u+x unfog*
sudo mv unfog* /usr/local/bin/
curl -sLo unfog-completion.bash "https://raw.githubusercontent.com/unfog-io/unfog-cli/master/bin/completion.bash"
source unfog-completion.bash
sudo mv unfog-completion.bash /etc/bash_completion.d/
echo "Unfog installed!"
