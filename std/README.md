# llstd: llrl standard library

llrl modules under this directory, except `tests`, are bundled with the llrl compiler and are available under the package named `std`. Also, as a language feature, all llrl modules that do not contain `(no-implicit-std)` implicitly import `std`.

As a Rust crate, this embeds all the source code of the std package and provides it as a `HashMap<String, String>`.
