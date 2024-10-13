# Maintainer: Fred Mitchell <fred.mitchell@atomlogik.de>
pkgname=swiss-army-knife
pkgver=0.0.0.1
pkgrel=1
pkgdesc="A collection of powerful but useful small tools."
arch=('x86_64')
url="https://github.com/flajann2/swiss-army-knife-hs"
license=('MIT')
depends=('ghc' 'stack')
makedepends=('git')
source=("https://github.com/flajann2/swiss-army-knife-hs.git")
md5sums=('SKIP')

build() {
    cd "$srcdir/$pkgname"
    stack build --system-ghc --ghc-options="-O3"
}

package() {
    cd "$srcdir/$pkgname"
    install -Dm755 "$(stack path --local-install-root)/bin/$pkgname" "$pkgdir/usr/bin/$pkgname"
    install -Dm644 LICENSE "$pkgdir/usr/share/licenses/$pkgname/LICENSE"
}
