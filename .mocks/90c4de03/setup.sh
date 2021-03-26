#! /bin/sh

set -e
set -u

NDK_LINUX="https://dl.google.com/android/repository/android-ndk-r21d-linux-x86_64.zip"
NDK_DARWIN="https://dl.google.com/android/repository/android-ndk-r21d-darwin-x86_64.zip"
NDK_FOLDER="android-ndk-r21d"

OS=""
if [[ "$(uname)" == 'Linux' ]]; then
  OS="linux"
  curl -o $cur__target_dir/ndk.zip $NDK_LINUX
elif [[ "$(uname)" == 'Darwin' ]]; then
  OS="darwin"
  curl -o $cur__target_dir/ndk.zip $NDK_DARWIN
fi

unzip $cur__target_dir/ndk.zip -d $cur__target_dir
mv "$cur__target_dir/$NDK_FOLDER" $cur__install/ndk

# patch #include <version> to #include <__cxx_version>
CXX_INCLUDE_DIR="$cur__install/ndk/toolchains/llvm/prebuilt/$OS-x86_64/sysroot/usr/include/c++/v1"
pushd $CXX_INCLUDE_DIR
cp version __cxx_version
find . -type f -exec sed -i.bak "s/<version>/<__cxx_version>/g" {} \;
popd

