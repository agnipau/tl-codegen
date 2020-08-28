# tl-codegen

This crate exposes a library to parse [TL
files](https://core.telegram.org/mtproto/TL) like
[this](https://github.com/tdlib/td/blob/master/td/generate/scheme/td_api.tl)
and generate equivalent rust types.

The library uses [nom](https://github.com/Geal/nom) for the parser and
[quote](https://github.com/dtolnay/quote) and
[proc_macro2](https://github.com/alexcrichton/proc-macro2) to convert the
data to Rust `TokenStream`s. So if the code emitted is syntactically not
correct it is a compile time error. I think it is a better approach than to
work with plain strings.

At the moment it was only tested with the td_api.tl included in the
[1.6.0](https://github.com/tdlib/td/releases/tag/v1.6.0) release.

#### License

<sup>
Licensed under either of <a href="LICENSE-APACHE">Apache License, Version
2.0</a> or <a href="LICENSE-MIT">MIT license</a> at your option.
</sup>

<br>

<sub>
Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in this crate by you, as defined in the Apache-2.0 license, shall
be dual licensed as above, without any additional terms or conditions.
</sub>

