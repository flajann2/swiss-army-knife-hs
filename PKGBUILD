# Maintainer: Fred Mitchell <fred.mitchell@atomlogik.de>
pkgname=swiss-army-knfe
pkgver=0.0.0.1
pkgrel=1
pkgdesc="A collection of powerful but usefuli small tools."
arch=('x86_64')
url="https://github.com/flajann2/swiss-army-knife-hs"
license=('MIT')
depends=('ghc' 'haskell-stack')
makedepends=('git')
source=("https://github.com/flajann2/swiss-army-knife-hs.git")
md5sums=('SKIP')

build() {
    cd "$srcdir/$pkgname"
    stack build --system-ghc
}

package() {
    cd "$srcdir/$pkgname"
    install -Dm755 "$(stack path --local-install-root)/bin/$pkgname" "$pkgdir/usr/bin/$pkgname"
    install -Dm644 LICENSE "$pkgdir/usr/share/licenses/$pkgname/LICENSE"
}
