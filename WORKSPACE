workspace(name = "mixtape")

###
### Nix Packages for local toolchains!
###

nixpkgs_version = "cd2ed701127ebf7f8f21d37feb1d678e4fdf85e5"
http_archive(
    name = "io_tweag_rules_nixpkgs",
    strip_prefix = "rules_nixpkgs-%s" % (nixpkgs_version,),
    urls = ["https://github.com/tweag/rules_nixpkgs/archive/%s.tar.gz" % (nixpkgs_version,)],
    )
load("@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl", "nixpkgs_git_repository", "nixpkgs_package")

nixpkgs_package(
    name = "sys_erlang",
    attribute_path = "erlangR20",
    build_file_content = """\
package(default_visibility = ["//visibility:public"])

filegroup(
  name = "bin",
  srcs = glob(["*-erlang-*/bin/*"]),
)
"""
)

###
### Erlang/OTP Support
###
