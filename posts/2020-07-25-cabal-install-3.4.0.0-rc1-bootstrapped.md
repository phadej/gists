haskell---
title: cabal-install-3.4.0.0-rc1 bootstrapping environments
author: Oleg Grenrus
---

These are my notes for how
https://mail.haskell.org/pipermail/cabal-devel/2020-July/010483.html
is made.

Lately I have been working on improving the bootstrapping
and packaging story of `cabal-install`.
Bootstrapping is a needed on the systems without
existing (recent) `cabal-install` executable available.
Ben Gamari have rewritten the previous Bash script in Python,
which makes it more pleasant to improve further.
I have further developed it, and give it a try on few platforms.

I try to use GHC-8.8 as the bootstrapping compiler.
Ideally the good parts of these will be encoded
as CI setup scripts.

x86\_64 Linux
------------

There are at least four Debian and Ubuntu derivatives maintained
at each point of time. E.g. now there is Debian stretch (9, oldstable), 
and Debian buster (10, stable), Ubuntu Xenial (16.04), Bionic (18.04),
and Focal (20.04).

It would take some time to build package for each one.
As long as C-dependencies used by `cabal-install` are the same,
we can get away with a single binary. (I'll get to static linking later).

As my computer is `x86_64`, I can use Docker to get clean environment.

It's worth picking oldest available system to build artifacts.
[HVR's PPA](https://launchpad.net/~hvr/+archive/ubuntu/ghc)
has GHC-8.8.3 for Ubuntu-14.04. Lets try that.

```sh
docker run --rm -ti ubuntu:14.04
```

And in the container:

```sh
apt-get update
apt-get install -y software-properties-common
add-apt-repository ppa:hvr/ghc
add-apt-repository ppa:deadsnakes/ppa
apt-get update
apt-get install python3.7 ghc-8.8.3 zlib1g-dev git

git clone https://github.com/phadej/cabal.git
cd cabal
git checkout release-py-3.4        # branch with modified scripts
$EDITOR bootstrap/linux-8.8.3.json # change +ofd_locking to -ofd_locking - old libc headers! 
python3.7 bootstrap/bootstrap.py --deps bootstrap/linux-8.8.3.json --with-compiler=/opt/ghc/8.8.3/bin/ghc
```

We need to install newer Python, because `f"string {interpolation}"` is so
nice feature, but is available only since Python 3.6!

We also need to turn off `+ofd_locking` flag of `lukko` on this
old machine.
Then `cabal` will use `flock` locks, which are more widely available, but not as good as OFD ones.
It's fine for cabal-install.

The bootstrap script printed

```
Bootstrapping finished!

The resulting cabal-install executable can be found at

    /root/cabal/_build/bin/cabal

It have been archived for distribution in

    /root/cabal/_build/artifacts/cabal-install-3.4.0.0-x86_64-trusty-linux-bootstrapped.tar.xz

You now should use this to build a full cabal-install distribution
using v2-build.
```

at the end. I then uploaded the tarball somewhere where I can get it from.

It's worth checking `ldd` output, as the executable is dynamically
linked against C-dependencies:

```
root@1c435ff9e0d3:~/cabal# ldd /root/cabal/_build/bin/cabal
	linux-vdso.so.1 =>  (0x00007ffcf75db000)
	libm.so.6 => /lib/x86_64-linux-gnu/libm.so.6 (0x00007f8610cc4000)
	libresolv.so.2 => /lib/x86_64-linux-gnu/libresolv.so.2 (0x00007f8610aa9000)
	libz.so.1 => /lib/x86_64-linux-gnu/libz.so.1 (0x00007f8610890000)
	librt.so.1 => /lib/x86_64-linux-gnu/librt.so.1 (0x00007f8610688000)
	libutil.so.1 => /lib/x86_64-linux-gnu/libutil.so.1 (0x00007f8610485000)
	libdl.so.2 => /lib/x86_64-linux-gnu/libdl.so.2 (0x00007f8610281000)
	libpthread.so.0 => /lib/x86_64-linux-gnu/libpthread.so.0 (0x00007f8610063000)
	libgmp.so.10 => /usr/lib/x86_64-linux-gnu/libgmp.so.10 (0x00007f860fdef000)
	libc.so.6 => /lib/x86_64-linux-gnu/libc.so.6 (0x00007f860fa26000)
	/lib64/ld-linux-x86-64.so.2 (0x00007f8610fca000)
```

Sometimes these versions change (e.g. `libgmp.so.10` was `.9` some years ago).
Especially, there is no `libtinfo` which **is changed** recently,
e.g. GHC itself is affected by that change (debian9 binaries don't work on debian10).

<h3>Testing</h3>

After we built a cabal we can test it.

```sh
curl -O https://oleg.fi/cabal-install-3.4.0.0-rc1-bootstrapped/cabal-install-3.4.0.0-x86_64-trusty-linux-bootstrapped.tar.xz
tar -xJvf cabal-install-3.4.0.0-x86_64-trusty-linux-bootstrapped.tar.xz
chmod a+x cabal
./cabal
```

Seems to work.

We can test it on various systems, available quite easily, thanks to docker:

```sh
docker run --rm -ti centos:7      # another old system
docker run --rm -ti debian:10     # recent system
docker run --rm -ti ubuntu:20.04  # even more recent one
```

and repeating the above steps there.

On Debian and Ubuntu I needed to install some packages

```sh
apt-get update
apt-get install -y curl xz-utils
```

 `x86_64` Linux is covered.

macOS
-----

I have to admit, that I don't know what are the best practices with building
macOS binaries nowadays. I assume that using as old macOS version
as you have *is not a bad idea*, for similar reasons as with Linux:
binaries built on older macOS tend to work on newer ones,
but not vice versa.

An example is [`_utimensat` error](https://gitlab.haskell.org/ghc/ghc/-/issues/17895).
Builders run more recent macOS, so linking against `directory` fails
on older ones. I have `10.12` i.e. Sierra machine still.
If I use it, I'll run into above bug when using GHC-8.8.3.
So I'll use GHC-8.6.3. (Yes, 8.6.3, not 8.6.4 or GHC-8.6.5).

The GHC-8.6.3 I used is installed from GHCs bindists.
Download, unpack, configure, `make install`. I had it already,
so I don't have commands here.

One need to adjust `linux-8.6.5.json` to downgrade the dependencies.
We do use Linux package list, as it seems to work for macs.

As this was done on another machine,
I haven't copied the commands here.
They are quite simialr to above (or below).

Please tell whether the resulting binary built on sierra


Windows
-------

Windows is not covered for one or two reasons.

First there is no `windows-*.json` bootstrap receipt file.
And the reason for that, is that we don't want to encourage people
using bootstrapping script. There are older just fine `cabal-install`
binaries for Windows which you should use to compile newer ones.


Linux aarch64
-------------

There are two options, local or hosted.

<h3>QEMU</h3>

QEMU in Ubuntu-18.04 (which I have on my desktop atm) is quite old,
but it can emulate `aarch64`. Maybe newer ones are faster.

On host

```sh
#!/bin/sh

# Using https://wiki.debian.org/Arm64Qemu

# These packages are available on Ubuntu too
sudo apt-get install qemu-utils qemu-efi-aarch64 qemu-system-arm

# IMAGE=$(pwd)/debian-10.4.3-20200610-openstack-arm64.qcow2
IMAGE=$(pwd)/debian-9.13.0-openstack-arm64.qcow2

# GHC Usage occasionally went near 4G
MEMORY=8G

# More harddrive space
qemu-img resize $IMAGE +20G

qemu-system-aarch64 -m $MEMORY -M virt -cpu cortex-a57 \
  -bios /usr/share/qemu-efi-aarch64/QEMU_EFI.fd \
  -drive if=none,file="$IMAGE",id=hd0 -device virtio-blk-device,drive=hd0 \
  -device e1000,netdev=net0 -netdev user,id=net0,hostfwd=tcp:127.0.0.1:5555-:22 \
  -nographic
```

In emulation

```sh
apt-get update
apt-get install xz-utils build-essential git autoconf python3 libgmp-dev libncurses-dev libnuma-dev zlib1g-dev vim tmux htop

vim /etc/apt/sources.list # Add line for buster copying the stretch one
echo 'APT::Default-Release "stretch";' | tee /etc/apt/apt.conf.d/00local
apt-get update
apt-get -t buster install python3.7

# There don't seem to be 8.8 aarch official bindists
curl -O https://downloads.haskell.org/~ghc/8.10.1/ghc-8.10.1-aarch64-deb9-linux.tar.xz
tar -xJvf ghc-8.10.1-aarch64-deb9-linux.tar.xz
cd ghc-8.10.1
sudo mkdir /opt/ghc
sudo chown ubuntu /opt/ghc
./configure --prefix=/opt/ghc/8.10.1
make install

# For GHC-8.10.1, we need LLVM-9
export LLVM_DIR=/opt/llvm
export LLVM_VERSION=9.0.0
wget https://releases.llvm.org/$LLVM_VERSION/clang+llvm-$LLVM_VERSION-aarch64-linux-gnu.tar.xz
tar -xJvf clang+llvm-$LLVM_VERSION-aarch64-linux-gnu.tar.xz
rm -rf $LLVM_DIR
mv clang+llvm-$LLVM_VERSION-aarch64-linux-gnu $LLVM_DIR
export PATH=$LLVM_DIR/bin:$PATH
llc --version

git clone https://github.com/phadej/cabal.git
cd cabal
git checkout release-py-3.4          # branch with modified scripts

# this takes many hours, because of emulation...
# Looking at htop output for `llc` of compiling `LicenseId` module is new kind of benchmark
# (htop eats plenty of CPU itself, so you save real time by not having it open)
time python3.7 ./bootstrap/bootstrap.py -w /opt/ghc/8.10.1/bin/ghc --deps bootstrap/linux-8.10.1.json 
```

Some links
- https://gist.github.com/ag88/163a7c389af0c6dcef5a32a3394e8bac#netboot-from-iso--install-from-debian-iso
- http://arvanta.net/~mps/install-aarch64-under-qemu.txt
- https://futurewei-cloud.github.io/ARM-Datacenter/qemu/how-to-launch-aarch64-vm/

<h3>AWS hosted instance</h3>

I picked Ubuntu-18.04 image, it also has `libtinfo5`.
I picked instance with 4 cores and 16GB RAM. Make volume of 20G or so.
We need more memory than CPUs.

```sh
add-apt-repository ppa:deadsnakes/ppa # this Python PPA has aarch64 builds tool!
apt-get update
apt-get install python3.8 llvm-9 xz-utils build-essential git autoconf python3 libgmp-dev libncurses-dev libnuma-dev zlib1g-dev vim tmux htop

curl -O https://downloads.haskell.org/~ghc/8.10.1/ghc-8.10.1-aarch64-deb9-linux.tar.xz
tar -xJvf ghc-8.10.1-aarch64-deb9-linux.tar.xz
cd ghc-8.10.1
sudo mkdir /opt/ghc
sudo chown ubuntu /opt/ghc
./configure --prefix=/opt/ghc/8.10.1
make install
cd

git clone https://github.com/phadej/cabal.git
cd cabal
git checkout release-py-3.4          # branch with modified scripts
time python3.8 ./bootstrap/bootstrap.py -w /opt/ghc/8.10.1/bin/ghc --deps bootstrap/linux-8.10.1.json

curl -O https://oleg.fi/cabal-install-3.4.0.0-rc1-bootstrapped/cabal-install-3.4.0.0-aarch64-bionic-linux-bootstrapped.tar.xz
mkdir cabal-3.4.0.0-bootstrapped
tar -xJvf cabal-install-3.4.0.0-* -C cabal-3.4.0.0-bootstrapped
chmod a+x cabal-3.4.0.0-bootstrapped/cabal
cabal-3.4.0.0-bootstrapped/cabal --version

git clone https://github.com/haskell/cabal.git
cd cabal
git checkout 3.4
time python3.8 release.py -w /opt/ghc/8.10.1/bin/ghc -C $HOME/cabal-3.4.0.0-bootstrapped/cabal
```


FreeBSD
-------

As I'm now have used QEMU, let me use it a bit more.

FreeBSD has virtual machine images available: https://www.freebsd.org/where.html
Julian Ospald kindly builds FreeBSD images of GHC available at: https://files.hasufell.de/ghc/

He was wondering on Freenode's `#ghc` though:

```
maerwald  I'm wondering if there is strong interest in FreeBSD bindists,
          because it seems quite time consuming and I've not seen FreeBSD
          users step up maintaining those. Am I correct to assume they
          are fine with porst + building from source?
```

Because I don't know anything about FreeBSD, I went with the latest release.

```sh
sudo apt-get install qemu-system-x86

curl https://download.freebsd.org/ftp/releases/VM-IMAGES/12.1-RELEASE/amd64/Latest/FreeBSD-12.1-RELEASE-amd64.qcow2.xz | xz -d > FreeBSD-12.1-RELEASE-amd64.qcow2
qemu-img resize FreeBSD*qcow2 +15G

MEMORY=8G
IMAGE=FreeBSD-12.1-RELEASE-amd64.qcow2

qemu-system-x86_64 -m $MEMORY \
  -hda $IMAGE
  -device e1000,netdev=net0 -netdev user,id=net0,hostfwd=tcp:127.0.0.1:6666-:22 \
```

One need to start `sshd` to get better terminal.
Changing password (`passwd`) makes sense too.

```sh
pkg update -f
pkg install git curl vim-console gmake libiconv python3 tmux zlib

curl -O https://files.hasufell.de/ghc/ghc-8.8.4-x86_64-portbld-freebsd.tar.xz
tar -xJvf ghc-8.8.4-x86_64-portbld-freebsd.tar.xz 
cd ghc-8.4.4
./configure --prefix=/opt/ghc/8.8.4
gmake install

git clone https://github.com/phadej/cabal.git
cd cabal
git checkout release-py-3.4     # branch with modified scripts
vim bootstrap/linux-8.8.3.json  # process-1.6.9.0 ghc-boot-th-8.8.4 bytestring-0.10.10.1
time python3.7 ./bootstrap/bootstrap.py -w /opt/ghc/8.8.4/bin/ghc --deps bootstrap/linux-8.8.3.json
```

```
time python3.7 release.py -w /opt/ghc/8.8.4/bin/ghc -C $HOME/cabal-3.4.0.0-bootstrapped/cabal
````


Alpine Linux x86\_64
-------------------


```sh
docker run --rm -ti alpine:3.11
```

```
cd

apk add curl python3 git xz perl gcc musl-dev make vim gmp-dev zlib-dev zlib-static tmux openssh-client

curl -O https://files.hasufell.de/ghc/ghc-8.8.4-x86_64-alpine-linux.tar.xz
tar -xJvf ghc-8.8.4-x86_64-alpine-linux.tar.xz
cd ghc-8.8.4
./configure --prefix=/opt/ghc/8.8.4
make install
cd

curl -O https://oleg.fi/cabal-install-3.4.0.0-rc1-bootstrapped/cabal-install-3.4.0.0-x86_64-alpine-3.11.6-linux-bootstrapped.tar.xz
mkdir cabal-3.4.0.0
tar -xJvf cabal-install-3.4.0.0-x86_64-alpine-3.11.6-linux-bootstrapped.tar.xz -C cabal-3.4.0.0
chmod a+x cabal-3.4.0.0/cabal
$HOME/cabal-3.4.0.0/cabal --version

# bootstrap
git clone https://github.com/phadej/cabal.git
cd cabal
git checkout release-py-3.4
vim bootstrap/linux-8.8.3.json
time python3 ./bootstrap/bootstrap.py -w /opt/ghc/8.8.4/bin/ghc --deps bootstrap/linux-8.8.3.json

# release
git clone https://github.com/haskell/cabal.git
cd cabal
git checkout 3.4
git log -1
time python3.8 release.py -w /opt/ghc/8.8.4/bin/ghc -C $HOME/cabal-3.4.0.0/cabal --enable-static-executable --disable-ofd
```

```
# process-1.6.9.0 ghc-boot-th-8.8.4 bytestring-0.10.10.1
```

Note: this `cabal-install` is dynamically linked,
which is fine. We can use it to build static one when assembling
proper release.

for static builds we need

```
apk add zlib-static
```

And modify `bootstrap.py` to add `--enable-executable-static`.


Ubuntu 16.04 release
--------------------

```
docker run --rm -ti ubuntu:16.04
```

```
cd

apt-get update
apt-get install -y software-properties-common
add-apt-repository -y ppa:hvr/ghc
add-apt-repository -y ppa:deadsnakes/ppa
apt-get update
apt-get install -y cabal-install-3.2 ghc-8.8.4 python3.8 git build-essential zlib1g-dev

git clone https://github.com/haskell/cabal.git
cd cabal
git checkout 3.4
time python3.8 release.py -w /opt/ghc/8.8.4/bin/ghc -C /opt/cabal/3.2/bin/cabal
```

FreeBSD release
---------------

```
cd
curl -O https://oleg.fi/cabal-install-3.4.0.0-rc1-bootstrapped/cabal-install-3.4.0.0-amd64-12.01-RELEASE-freebsd-bootstrapped.tar.xz
mkdir cabal-3.4.0.0-bootstrapped
tar -xJvf cabal-install-3.4.0.0-amd64-12.01-RELEASE-freebsd-bootstrapped.tar.xz -C cabal-3.4.0.0-bootstrapped
chmod a+x cabal-3.4.0.0-bootstrapped/cabal
cabal-3.4.0.0-bootstrapped/cabal --version

git clone https://github.com/haskell/cabal.git
cd cabal
git checkout 3.4
time python3.7 release.py -w /opt/ghc/8.8.4/bin/ghc -C $HOME/cabal-3.4.0.0-bootstrapped/cabal
```
